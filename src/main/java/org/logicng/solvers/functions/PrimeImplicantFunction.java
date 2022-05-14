package org.logicng.solvers.functions;

import static org.logicng.handlers.Handler.aborted;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.Arrays;
import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

public class PrimeImplicantFunction implements SolverFunction<LNGIntVector> {
    final SATHandler handler;
    final boolean isMinimal;
    final Collection<Variable> variables;

    private PrimeImplicantFunction(final SATHandler handler, final boolean isMinimal, final Collection<Variable> variables) {
        this.handler = handler;
        this.isMinimal = isMinimal;
        this.variables = variables;
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public LNGIntVector apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        // TODO handler
        final Tristate sat = solver.sat();
        if (sat == Tristate.FALSE) {
            return null;
        }
        final LNGBooleanVector model = solver.underlyingSolver().model();
        final SortedSet<Integer> primeImplicant = new TreeSet<>();
        for (int i = 0; i < model.size(); i++) {
            final String name = solver.underlyingSolver().nameForIdx(i);
            final boolean variablesContainName = this.variables == null || this.variables.stream().anyMatch(x -> x.name().equals(name));
            if (isRelevantVariable(name) && variablesContainName) {
                primeImplicant.add(MiniSatStyleSolver.mkLit(i, !model.get(i)));
            }
        }
        for (int i = 0; i < model.size(); i++) {
            final String name = solver.underlyingSolver().nameForIdx(i);
            if (!isRelevantVariable(name)) {
                continue;
            }
            final int lit = MiniSatStyleSolver.mkLit(i, !model.get(i));
            primeImplicant.remove(lit);
            final LNGIntVector lngIntVector = getLngIntVector(primeImplicant);
            final boolean satCall = solver.underlyingSolver().solve(handler, lngIntVector) == Tristate.TRUE;
            if (aborted(handler)) {
                return null;
            }
            if (satCall) {
                primeImplicant.add(lit);
            }
        }
        if (this.isMinimal) {
            // TODO gebe minimales Modell aus
            // final Assignment minimumModel = solver.execute(OptimizationFunction.minimize(primeImplicant));
        }
        return getLngIntVector(primeImplicant);
    }

    final boolean isRelevantVariable(final String name) {
        return !name.startsWith(FormulaFactory.CNF_PREFIX) && !name.startsWith(FormulaFactory.CC_PREFIX) && !name.startsWith(FormulaFactory.PB_PREFIX);
    }

    LNGIntVector getLngIntVector(final SortedSet<Integer> set) {
        final LNGIntVector vector = new LNGIntVector(set.size());
        for (final Integer integer : set) {
            vector.push(integer);
        }
        return vector;
    }

    /**
     * The builder for a model counting function.
     */
    public static class Builder {
        private SATHandler handler;
        private boolean isMinimal;
        private Collection<Variable> variables;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the handler for this function
         * @param handler the handler
         * @return the current builder
         */
        public Builder handler(final SATHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the flag whether the prime implicant which is being returned should be minimal.
         * @param isMinimal flag whether the implicant should be minimal
         * @return the current builder
         */
        public Builder isMinimal(final boolean isMinimal) {
            this.isMinimal = isMinimal;
            return this;
        }

        /**
         * Sets the set of variables over which the prime implicants should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the prime implicants should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public PrimeImplicantFunction build() {
            return new PrimeImplicantFunction(this.handler, this.isMinimal, this.variables);
        }
    }

}
