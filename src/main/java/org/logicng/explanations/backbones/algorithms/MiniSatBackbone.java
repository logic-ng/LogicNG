package org.logicng.explanations.backbones.algorithms;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.explanations.backbones.BackboneConfig;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.datastructures.MSWatcher;
import org.logicng.solvers.sat.MiniSat2Solver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;

/**
 * An extension of MiniSat to compute the backbone of a formula.
 * <p>
 * The algorithm iteratively checks each variable of an initial model (candidates) whether the variable
 * is a backbone variable. For each check the SAT solving procedure is called. Thus, at the number of SAT calls is at
 * most the number of variables.
 * <p>
 * Reference: Algorithm 3 in M. Janota, I. Lynce, J. Marques-Silva, Algorithms for Computing Backbones of Propositional
 * Formulae, AI Communications, Volume 28(2), 161-177, 2015.
 * @version 1.5
 * @since 1.5
 */
public class MiniSatBackbone extends MiniSat2Solver {

    // TODO experimental backbone solver and not extensively tested yet!
    // TODO BackboneType

    private final BackboneConfig config;

    /**
     * Candidates to test for backbone.
     *
     * The integers are solver internal literal integers.
     */
    private Stack<Integer> candidates;

    /**
     * Assumptions used to call the solver with, filled by identified backbone literals.
     */
    private LNGIntVector assumptions;

    /**
     * Backbone map: integer literal -> Tristate.
     *
     * {@link Tristate#TRUE} is a positive backbone variable.
     * {@link Tristate#FALSE} is a negative backbone variable.
     * {@link Tristate#UNDEF} is an optional variable.
     */
    private HashMap<Integer, Tristate> backboneMap;

    /**
     * Creates a new backbone solver using the given configuration.
     * @param config configuration
     */
    public MiniSatBackbone(final BackboneConfig config) {
        this.config = config;
    }

    /**
     * Creates a new backbone solver using the default configuration.
     */
    public MiniSatBackbone() {
        this(new BackboneConfig.Builder().build());
    }

    /**
     * Adds an arbitrary formula to the propagator.
     * @param formula the formula
     */
    public void add(final Formula formula) {
        final Formula cnf = formula.cnf();
        switch (cnf.type()) {
            case TRUE:
                break;
            case FALSE:
            case LITERAL:
            case OR:
                this.addClause(generateClauseVector(cnf), null);
                break;
            case AND:
                for (final Formula op : cnf) {
                    this.addClause(generateClauseVector(op), null);
                }
                break;
            default:
                throw new IllegalStateException("Unexpected formula type in CNF: " + cnf.type());
        }
    }

    /**
     * Returns a list of relevant variable indices. A relevant variable is known by the solver.
     * @param variables variables to convert and filter
     * @return list of relevant variable indices
     */
    private List<Integer> getRelevantVarIndices(final Collection<Variable> variables) {
        final List<Integer> relevantVarIndices = new ArrayList<>(variables.size());
        for (final Variable var : variables) {
            final Integer idx = this.name2idx.get(var.name());
            // Note: Unknown variables are variables added to the solver yet. Thus, these are optional variables and can
            // be left out for the backbone computation.
            if (idx != null) {
                relevantVarIndices.add(idx);
            }
        }
        return relevantVarIndices;
    }

    /**
     * Builds the backbone object from the computed backbone literals.
     * @param variables relevant variables
     * @return backbone
     */
    private Backbone buildBackbone(final Collection<Variable> variables) {
        final SortedSet<Variable> posBackboneVars = new TreeSet<>();
        final SortedSet<Variable> negBackboneVars = new TreeSet<>();
        final SortedSet<Variable> optionalVars = new TreeSet<>();
        for (final Variable var : variables) {
            final Integer idx = this.name2idx.get(var.name());
            if (idx == null) {
                optionalVars.add(var);
            } else {
                switch (this.backboneMap.get(idx)) {
                    case TRUE:
                        posBackboneVars.add(var);
                        break;
                    case FALSE:
                        negBackboneVars.add(var);
                        break;
                    case UNDEF:
                        optionalVars.add(var);
                        break;
                    default:
                        throw new IllegalStateException("Unknown tristate: " + this.backboneMap.get(idx));
                }
            }
        }
        return new Backbone(posBackboneVars, negBackboneVars, optionalVars);
    }

    /**
     * Computes the backbone of the given variables with respect to the formulas added to the solver.
     * @param variables variables to test
     * @return the backbone projected to the relevant variables or {@code null} if the formula on the solver with the restrictions are not satisfiable
     */
    public Backbone compute(final Collection<Variable> variables) {
        final boolean sat = solve(null) == Tristate.TRUE;
        Backbone backbone = null;
        if (sat) {
            final List<Integer> relevantVarIndices = getRelevantVarIndices(variables);
            compute(relevantVarIndices);
            backbone = buildBackbone(variables);
        }
        return backbone;
    }

    /**
     * Tests the given variable whether it is a unit propagated literal on level 0.
     *
     * Assumption: The formula on the solver has successfully been tested to be satisfiable before.
     * @param var variable index to test
     * @return {@code true} if the variable is a unit propagated literal on level 0, otherwise {@code false}
     */
    private boolean isUPZeroLit(final int var) {
        return this.vars.get(var).level() == 0;
    }

    /**
     * Tests the given literal whether it is unit in the given clause.
     * @param lit    literal to test
     * @param clause clause containing the literal
     * @return {@code true} if the literal is unit, {@code false} otherwise
     */
    private boolean isUnit(final int lit, final MSClause clause) {
        for (int i = 0; i < clause.size(); ++i) {
            final int clauseLit = clause.get(i);
            // TODO this unit check can surely be improved
            if (lit != clauseLit && this.model.get(var(clauseLit)) != sign(clauseLit)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Tests the given literal whether it is rotatable in the current model.
     * @param lit literal to test
     * @return {@code true} if the literal is rotatable, otherwise {@code false}
     */
    private boolean isRotatable(final int lit) {
        // Unit propagated literals cannot be rotatable
        if (v(var(lit)).reason() != null) {
            return false;
        }
        // A rotatable literal MUST NOT be unit
        for (final MSWatcher watcher : this.watches.get(not(lit))) {
            if (isUnit(lit, watcher.clause())) {
                return false;
            }
        }
        return true;
    }

    /**
     * Adds the given literal to the backbone result and optionally adds the literal to the solver.
     * @param lit literal to add
     */
    private void addBackboneLiteral(final int lit) {
        this.backboneMap.put(var(lit), sign(lit) ? Tristate.FALSE : Tristate.TRUE);
        this.assumptions.push(lit);
    }

    /**
     * Creates the initial candidate literals for the backbone computation.
     * @param variables variables to test
     * @return initial candidates
     */
    private Stack<Integer> createInitialCandidates(final List<Integer> variables) {
        for (final Integer var : variables) {
            if (this.config.isInitialLBCheckForUPZeroLiterals() && isUPZeroLit(var)) {
                final int backboneLit = mkLit(var, !this.model.get(var));
                addBackboneLiteral(backboneLit);
            } else {
                final int lit = mkLit(var, !this.model.get(var));
                if (!this.config.isInitialUBCheckForRotatableLiterals() || !isRotatable(lit)) {
                    this.candidates.add(lit);
                }
            }
        }
        return this.candidates;
    }

    /**
     * Refines the upper bound by optional checks (UP zero literal, complement model literal, rotatable literal).
     */
    private void refineUpperBound() {
        for (final Integer lit : new ArrayList<>(this.candidates)) {
            final int var = var(lit);
            if (this.config.isCheckForUPZeroLiterals() && isUPZeroLit(var)) {
                this.candidates.remove(lit);
                addBackboneLiteral(lit);
            } else if (this.config.isCheckForComplementModelLiterals() && this.model.get(var) == sign(lit)) {
                this.candidates.remove(lit);
            } else if (this.config.isCheckForRotatableLiterals() && isRotatable(lit)) {
                this.candidates.remove(lit);
            }
        }
    }

    /**
     * Tests the given literal with the formula on the solver for satisfiability.
     * @param lit literal to test
     * @return {@code true} if satisfiable, otherwise {@code false}
     */
    private boolean solve(final int lit) {
        this.assumptions.push(not(lit));
        final boolean sat = solve(null, this.assumptions) == Tristate.TRUE;
        this.assumptions.pop();
        return sat;
    }

    /**
     * Initializes the internal solver state.
     * @param variables to test
     */
    private void init(final List<Integer> variables) {
        this.candidates = new Stack<>();
        this.assumptions = new LNGIntVector(variables.size());
        this.backboneMap = new HashMap<>();
        for (final Integer var : variables) {
            this.backboneMap.put(var, Tristate.UNDEF);
        }
    }

    @Override
    protected void cancelUntil(final int level) {
        if (decisionLevel() > level) {
            for (int c = this.trail.size() - 1; c >= this.trailLim.get(level); c--) {
                final int x = var(this.trail.get(c));
                final MSVariable v = this.vars.get(x);
                v.assign(Tristate.UNDEF);
                v.setPolarity(false);
                insertVarOrder(x);
            }
            this.qhead = this.trailLim.get(level);
            this.trail.removeElements(this.trail.size() - this.trailLim.get(level));
            this.trailLim.removeElements(this.trailLim.size() - level);
        }
    }

    /**
     * Computes the backbone for the given variables.
     * @param variables variables to test
     */
    private void compute(final List<Integer> variables) {
        init(variables);
        final Stack<Integer> candidates = createInitialCandidates(variables);
        while (candidates.size() > 0) {
            final int lit = candidates.pop();
            if (solve(lit)) {
                refineUpperBound();
            } else {
                addBackboneLiteral(lit);
            }
        }
    }

    /**
     * Generates a solver vector of a clause.
     * @param clause the clause
     * @return the solver vector
     */
    private LNGIntVector generateClauseVector(final Formula clause) {
        final LNGIntVector clauseVec = new LNGIntVector(clause.numberOfOperands());
        for (final Literal lit : clause.literals()) {
            int index = this.idxForName(lit.name());
            if (index == -1) {
                index = this.newVar(false, true);
                this.addName(lit.name(), index);
            }
            final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
            clauseVec.push(litNum);
        }
        return clauseVec;
    }
}