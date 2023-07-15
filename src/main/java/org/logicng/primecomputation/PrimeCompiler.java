// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.primecomputation;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;
import static org.logicng.handlers.OptimizationHandler.satHandler;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.OptimizationFunction;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.transformations.LiteralSubstitution;
import org.logicng.util.FormulaHelper;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Compiler for prime implicants and prime implicates of formulas.
 * <p>
 * Implementation is based on &quot;Prime compilation of non-clausal formulae&quot;,
 * (Previti, Ignatiev, Morgado, &amp; Marques-Silva, 2015).
 * <p>
 * The algorithm computes either <b>all</b> prime implicants and a <b>cover</b> of
 * prime implicates or a <b>cover</b> of prime implicants and <b>all</b> prime implicates.
 * This can be configured via the {@link org.logicng.primecomputation.PrimeResult.CoverageType}.
 * <p>
 * Furthermore, the algorithm comes in two flavors: One which searches for maximum models
 * {@link #getWithMaximization()} and another which searches for minimum models
 * {@link #getWithMaximization()}. From experience, the one with minimum models usually
 * outperforms the one with maximum models.
 * @version 2.1.0
 * @since 2.0.0
 */
public final class PrimeCompiler {

    private static final String POS = "_POS";
    private static final String NEG = "_NEG";
    private static final PrimeCompiler INSTANCE_MIN = new PrimeCompiler(false);
    private static final PrimeCompiler INSTANCE_MAX = new PrimeCompiler(true);

    private final boolean computeWithMaximization;

    private PrimeCompiler(final boolean computeWithMaximization) {
        this.computeWithMaximization = computeWithMaximization;
    }

    /**
     * Returns a compiler which uses minimum models to compute the primes.
     * @return a compiler which uses minimum models to compute the primes
     */
    public static PrimeCompiler getWithMinimization() {
        return INSTANCE_MIN;
    }

    /**
     * Returns a compiler which uses maximum models to compute the primes.
     * @return a compiler which uses maximum models to compute the primes
     */
    public static PrimeCompiler getWithMaximization() {
        return INSTANCE_MAX;
    }

    /**
     * Computes prime implicants and prime implicates for a given formula.
     * The coverage type specifies if the implicants or the implicates will
     * be complete, the other one will still be a cover of the given formula.
     * @param formula the formula
     * @param type    the coverage type
     * @return the prime result
     */
    public PrimeResult compute(final Formula formula, final PrimeResult.CoverageType type) {
        return compute(formula, type, null);
    }

    /**
     * Computes prime implicants and prime implicates for a given formula.
     * The coverage type specifies if the implicants or the implicates will
     * be complete, the other one will still be a cover of the given formula.
     * <p>
     * The prime compiler can be called with an {@link OptimizationHandler}.
     * The given handler instance will be used for every subsequent
     * {@link org.logicng.solvers.functions.OptimizationFunction} call and
     * the handler's SAT handler is used for every subsequent SAT call.
     * @param formula the formula
     * @param type    the coverage type
     * @param handler an optimization handler, can be {@code null}
     * @return the prime result or null if the computation was aborted by the handler
     */
    public PrimeResult compute(final Formula formula, final PrimeResult.CoverageType type, final OptimizationHandler handler) {
        start(handler);
        final boolean completeImplicants = type == PrimeResult.CoverageType.IMPLICANTS_COMPLETE;
        final Formula formulaForComputation = completeImplicants ? formula : formula.negate();
        final Pair<List<SortedSet<Literal>>, List<SortedSet<Literal>>> result = computeGeneric(formulaForComputation, handler);
        if (result == null || aborted(handler)) {
            return null;
        }
        return new PrimeResult(
                completeImplicants ? result.first() : negateAll(result.second()),
                completeImplicants ? result.second() : negateAll(result.first()),
                type);
    }

    private Pair<List<SortedSet<Literal>>, List<SortedSet<Literal>>> computeGeneric(final Formula formula, final OptimizationHandler handler) {
        final FormulaFactory f = formula.factory();
        final SubstitutionResult sub = createSubstitution(formula);
        final SATSolver hSolver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        hSolver.add(sub.constraintFormula);
        final SATSolver fSolver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        fSolver.add(formula.negate());
        final NaivePrimeReduction primeReduction = new NaivePrimeReduction(formula);
        final List<SortedSet<Literal>> primeImplicants = new ArrayList<>();
        final List<SortedSet<Literal>> primeImplicates = new ArrayList<>();
        while (true) {
            final Assignment hModel = hSolver.execute(this.computeWithMaximization
                    ? OptimizationFunction.builder().handler(handler).literals(sub.newVar2oldLit.keySet()).maximize().build()
                    : OptimizationFunction.builder().handler(handler).literals(sub.newVar2oldLit.keySet()).minimize().build());
            if (aborted(handler)) {
                return null;
            }
            if (hModel == null) {
                return new Pair<>(primeImplicants, primeImplicates);
            }
            final Assignment fModel = transformModel(hModel, sub.newVar2oldLit);
            final Tristate fSat = fSolver.sat(satHandler(handler), fModel.literals());
            if (aborted(handler)) {
                return null;
            }
            if (fSat == Tristate.FALSE) {
                final SortedSet<Literal> primeImplicant = this.computeWithMaximization ? primeReduction.reduceImplicant(fModel.literals(), satHandler(handler)) : fModel.literals();
                if (primeImplicant == null || aborted(handler)) {
                    return null;
                }
                primeImplicants.add(primeImplicant);
                final List<Literal> blockingClause = new ArrayList<>();
                for (final Literal lit : primeImplicant) {
                    blockingClause.add(((Literal) lit.transform(sub.substitution)).negate());
                }
                hSolver.add(f.or(blockingClause));
            } else {
                final SortedSet<Literal> implicate = new TreeSet<>();
                for (final Literal lit : (this.computeWithMaximization ? fModel : fSolver.model(formula.variables())).literals()) {
                    implicate.add(lit.negate());
                }
                final SortedSet<Literal> primeImplicate = primeReduction.reduceImplicate(implicate, satHandler(handler));
                if (primeImplicate == null || aborted(handler)) {
                    return null;
                }
                primeImplicates.add(primeImplicate);
                hSolver.add(f.or(primeImplicate).transform(sub.substitution));
            }
        }
    }

    private SubstitutionResult createSubstitution(final Formula formula) {
        final Map<Variable, Literal> newVar2oldLit = new HashMap<>();
        final LiteralSubstitution substitution = new LiteralSubstitution();
        final List<Formula> constraintOps = new ArrayList<>();
        for (final Variable variable : formula.variables()) {
            final Variable posVar = formula.factory().variable(variable.name() + POS);
            newVar2oldLit.put(posVar, variable);
            substitution.addSubstitution(variable, posVar);
            final Variable negVar = formula.factory().variable(variable.name() + NEG);
            newVar2oldLit.put(negVar, variable.negate());
            substitution.addSubstitution(variable.negate(), negVar);
            constraintOps.add(formula.factory().amo(posVar, negVar));
        }
        return new SubstitutionResult(newVar2oldLit, substitution, formula.factory().and(constraintOps));
    }

    private Assignment transformModel(final Assignment model, final Map<Variable, Literal> mapping) {
        final Assignment mapped = new Assignment();
        for (final Variable var : model.positiveVariables()) {
            mapped.addLiteral(mapping.get(var));
        }
        return mapped;
    }

    private List<SortedSet<Literal>> negateAll(final Collection<SortedSet<Literal>> literalSets) {
        final List<SortedSet<Literal>> result = new ArrayList<>();
        for (final SortedSet<Literal> literals : literalSets) {
            result.add(FormulaHelper.negateLiterals(literals, TreeSet::new));
        }
        return result;
    }

    private static class SubstitutionResult {
        private final Map<Variable, Literal> newVar2oldLit;
        private final LiteralSubstitution substitution;
        private final Formula constraintFormula;

        private SubstitutionResult(final Map<Variable, Literal> newVar2oldLit, final LiteralSubstitution substitution, final Formula constraintFormula) {
            this.newVar2oldLit = newVar2oldLit;
            this.substitution = substitution;
            this.constraintFormula = constraintFormula;
        }
    }
}
