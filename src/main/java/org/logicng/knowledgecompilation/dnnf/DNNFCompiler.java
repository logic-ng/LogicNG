package org.logicng.knowledgecompilation.dnnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.DnnfCompilationHandler;
import org.logicng.knowledgecompilation.dnnf.datastructures.dtree.DTree;
import org.logicng.knowledgecompilation.dnnf.datastructures.dtree.DTreeGenerator;
import org.logicng.knowledgecompilation.dnnf.datastructures.dtree.DTreeLeaf;
import org.logicng.knowledgecompilation.dnnf.datastructures.dtree.DTreeNode;
import org.logicng.predicates.satisfiability.SATPredicate;
import org.logicng.solvers.sat.MiniSatStyleSolver;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;

/**
 * Implementation of a DNNF compiler based on ideas by Adnan Darwiche in
 * "New advances in compiling CNF to decomposable negation normal form."
 * @version 2.0.0
 * @since 2.0.0
 */
public class DNNFCompiler {

    private final FormulaFactory f;

    private final Formula cnf;
    private final Formula unitClauses;
    private final Formula nonUnitClauses;
    private final DNNFSATSolver solver;

    private final int numberOfVariables;

    private final Map<BitSet, Formula> cache;
    private DnnfCompilationHandler handler;

    private BitSet[][] localCacheKeys;
    private int[][][] localOccurrences;
    private final List<Formula> leafResultOperands;
    private final List<Literal> leafCurrentLiterals;

    /**
     * Constructs a new DNNF compiler for the given formula.
     * @param formula the formula to compile
     */
    public DNNFCompiler(final Formula formula) {
        this.f = formula.factory();
        this.cnf = formula;
        final Pair<Formula, Formula> pair = initializeClauses();
        this.unitClauses = this.f.and(pair.first());
        this.nonUnitClauses = this.f.and(pair.second());
        this.solver = new DNNFMiniSatStyleSolver(this.f, this.cnf.variables().size());
        this.solver.add(this.cnf);
        this.numberOfVariables = this.cnf.variables().size();
        this.cache = new HashMap<>();
        final int maxClauseSize = computeMaxClauseSize(this.cnf);
        this.leafResultOperands = new ArrayList<>(maxClauseSize);
        this.leafCurrentLiterals = new ArrayList<>(maxClauseSize);
    }

    /**
     * Performs the compilation using the given DTree generator.
     * @param generator the DTree generator
     * @return the compiled DNNF
     */
    public Formula compile(final DTreeGenerator generator) {
        return compile(generator, null);
    }

    /**
     * Performs the compilation using the given DTree generator and the compilation handler.
     * @param generator the DTree generator
     * @param handler   the compilation handler
     * @return the compiled DNNF
     */
    public Formula compile(final DTreeGenerator generator, final DnnfCompilationHandler handler) {
        if (!this.cnf.holds(new SATPredicate(this.f))) {
            return this.f.falsum();
        }
        final DTree dTree = generateDTree(generator);
        return compile(dTree, handler);
    }

    private int computeMaxClauseSize(final Formula cnf) {
        switch (cnf.type()) {
            case OR:
                return cnf.numberOfOperands();
            case AND:
                int max = 1;
                for (final Formula op : cnf) {
                    if (op.numberOfOperands() > max) {
                        max = op.numberOfOperands();
                    }
                }
                return max;
            default:
                return 1;
        }
    }

    private Pair<Formula, Formula> initializeClauses() {
        final List<Formula> units = new ArrayList<>();
        final List<Formula> nonUnits = new ArrayList<>();
        switch (this.cnf.type()) {
            case AND:
                for (final Formula clause : this.cnf) {
                    if (clause.isAtomicFormula()) {
                        units.add(clause);
                    } else {
                        nonUnits.add(clause);
                    }
                }
                break;
            case OR:
                nonUnits.add(this.cnf);
                break;
            default:
                units.add(this.cnf);
        }
        return new Pair<>(this.f.and(units), this.f.and(nonUnits));
    }

    private DTree generateDTree(final DTreeGenerator generator) {
        if (this.nonUnitClauses.isAtomicFormula()) {
            return null;
        }
        final DTree tree = generator.generate(this.nonUnitClauses);
        tree.initialize(this.solver);
        return tree;
    }

    private Formula compile(final DTree dTree, final DnnfCompilationHandler handler) {
        if (this.nonUnitClauses.isAtomicFormula()) {
            return this.cnf;
        }
        if (!this.solver.start()) {
            return this.f.falsum();
        }
        initializeCaches(dTree);
        this.handler = handler;
        if (handler != null) {
            handler.started();
        }

        Formula result;
        try {
            result = cnf2Ddnnf(dTree);
        } catch (final TimeoutException e) {
            result = null;
        }
        this.handler = null;
        return result == null ? null : this.f.and(this.unitClauses, result);
    }

    private void initializeCaches(final DTree dTree) {
        final int depth = dTree.depth() + 1;
        final int sep = dTree.widestSeparator() + 1;
        final int variables = this.cnf.variables().size();

        this.localCacheKeys = new BitSet[depth][sep];
        this.localOccurrences = new int[depth][sep][variables];
        for (int i = 0; i < depth; i++) {
            for (int j = 0; j < sep; j++) {
                this.localCacheKeys[i][j] = new BitSet(dTree.size() + variables);
                Arrays.fill(this.localOccurrences[i][j], -1);
            }
        }
    }

    private Formula cnf2Ddnnf(final DTree tree) throws TimeoutException {
        return cnf2Ddnnf(tree, 0);
    }

    private Formula cnf2Ddnnf(final DTree tree, final int currentShannons) throws TimeoutException {
        final BitSet separator = tree.dynamicSeparator();
        final Formula implied = this.newlyImpliedLiterals(tree.staticVarSet());

        if (separator.isEmpty()) {
            if (tree instanceof DTreeLeaf) {
                return this.f.and(implied, leaf2Ddnnf((DTreeLeaf) tree));
            } else {
                return conjoin(implied, (DTreeNode) tree, currentShannons);
            }
        } else {
            final int var = chooseShannonVariable(tree, separator, currentShannons);

            if (this.handler != null && !this.handler.shannonExpansion()) {
                throw new TimeoutException();
            }

            /* Positive branch */
            Formula positiveDnnf = this.f.falsum();
            if (this.solver.decide(var, true)) {
                positiveDnnf = cnf2Ddnnf(tree, currentShannons + 1);
            }
            this.solver.undoDecide(var);
            if (positiveDnnf == this.f.falsum()) {
                if (this.solver.atAssertionLevel() && this.solver.assertCdLiteral()) {
                    return cnf2Ddnnf(tree);
                } else {
                    return this.f.falsum();
                }
            }

            /* Negative branch */
            Formula negativeDnnf = this.f.falsum();
            if (this.solver.decide(var, false)) {
                negativeDnnf = cnf2Ddnnf(tree, currentShannons + 1);
            }
            this.solver.undoDecide(var);
            if (negativeDnnf == this.f.falsum()) {
                if (this.solver.atAssertionLevel() && this.solver.assertCdLiteral()) {
                    return cnf2Ddnnf(tree);
                } else {
                    return this.f.falsum();
                }
            }

            final Literal lit = this.solver.litForIdx(var);
            final Formula positiveBranch = this.f.and(lit, positiveDnnf);
            final Formula negativeBranch = this.f.and(lit.negate(), negativeDnnf);
            return this.f.and(implied, this.f.or(positiveBranch, negativeBranch));
        }
    }

    private int chooseShannonVariable(final DTree tree, final BitSet separator, final int currentShannons) {
        final int[] occurrences = this.localOccurrences[tree.depth()][currentShannons];
        for (int i = 0; i < occurrences.length; i++) {
            occurrences[i] = separator.get(i) ? 0 : -1;
        }
        tree.countUnsubsumedOccurrences(occurrences);

        int max = -1;
        int maxVal = -1;
        for (int i = separator.nextSetBit(0); i != -1; i = separator.nextSetBit(i + 1)) {
            final int val = occurrences[i];
            if (val > maxVal) {
                max = i;
                maxVal = val;
            }
        }
        return max;
    }

    private Formula conjoin(final Formula implied, final DTreeNode tree, final int currentShannons) throws TimeoutException {
        final Formula left;
        final Formula right;
        if (implied == this.f.falsum() ||
                (left = cnfAux(tree.left(), currentShannons)) == this.f.falsum() ||
                (right = cnfAux(tree.right(), currentShannons)) == this.f.falsum()) {
            return this.f.falsum();
        } else {
            return this.f.and(implied, left, right);
        }
    }

    private Formula cnfAux(final DTree tree, final int currentShannons) throws TimeoutException {
        if (tree instanceof DTreeLeaf) {
            return leaf2Ddnnf((DTreeLeaf) tree);
        } else {
            final BitSet key = computeCacheKey(tree, currentShannons);
            if (this.cache.containsKey(key)) {
                return this.cache.get(key);
            } else {
                final Formula dnnf = cnf2Ddnnf(tree);
                if (dnnf != this.f.falsum()) {
                    this.cache.put((BitSet) key.clone(), dnnf);
                }
                return dnnf;
            }
        }
    }

    private BitSet computeCacheKey(final DTree tree, final int currentShannons) {
        final BitSet key = this.localCacheKeys[tree.depth()][currentShannons];
        key.clear();
        tree.cacheKey(key, this.numberOfVariables);
        return key;
    }

    private Formula leaf2Ddnnf(final DTreeLeaf leaf) {
        final Iterator<Literal> literals = leaf.clause().literals().iterator();
        this.leafResultOperands.clear();
        this.leafCurrentLiterals.clear();
        Literal lit;
        int index = 0;
        while (literals.hasNext()) {
            lit = literals.next();
            switch (this.solver.valueOf(MiniSatStyleSolver.mkLit(this.solver.variableIndex(lit), !lit.phase()))) {
                case TRUE:
                    return this.f.verum();
                case UNDEF:
                    this.leafCurrentLiterals.add(lit);
                    this.leafResultOperands.add(this.f.and(this.leafCurrentLiterals));
                    this.leafCurrentLiterals.set(index, lit.negate());
                    index++;
            }
        }
        return this.f.or(this.leafResultOperands);
    }

    private Formula newlyImpliedLiterals(final BitSet knownVariables) {
        return this.solver.newlyImplied(knownVariables);
    }
}
