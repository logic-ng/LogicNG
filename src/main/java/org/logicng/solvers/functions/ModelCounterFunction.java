package org.logicng.solvers.functions;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.handlers.Handler.start;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.functions.MinimumPrimeImplicantFunction;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

// TODO check builder methods additional variables.
public class ModelCounterFunction implements SolverFunction<BigInteger> {
    private final ModelEnumerationHandler handler;
    private final Collection<Variable> variables;
    private final Collection<Variable> additionalVariables;

    private ModelCounterFunction(final ModelEnumerationHandler handler, final Collection<Variable> variables,
                                 final Collection<Variable> additionalVariables) {
        this.handler = handler;
        this.variables = variables;
        this.additionalVariables = additionalVariables;
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static ModelCounterFunction.Builder builder() {
        return new ModelCounterFunction.Builder();
    }

    @Override
    public BigInteger apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        start(this.handler);
        BigInteger modelCount = BigInteger.ZERO;
        SolverState stateBeforeEnumeration = null;
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            stateBeforeEnumeration = solver.saveState();
        }
        final boolean proceed = true;
        final LNGIntVector relevantIndices;
        if (this.variables == null) {
            if (!solver.getConfig().isAuxiliaryVariablesInModels()) {
                relevantIndices = new LNGIntVector();
                for (final Map.Entry<String, Integer> entry : solver.underlyingSolver().getName2idx().entrySet()) {
                    if (solver.isRelevantVariable(entry.getKey())) {
                        relevantIndices.push(entry.getValue());
                    }
                }
            } else {
                relevantIndices = null;
            }
        } else {
            relevantIndices = new LNGIntVector(this.variables.size());
            for (final Variable var : this.variables) {
                relevantIndices.push(solver.underlyingSolver().idxForName(var.name()));
            }
        }
        LNGIntVector relevantAllIndices = null;
        final SortedSet<Variable> uniqueAdditionalVariables =
                new TreeSet<>(this.additionalVariables == null ? Collections.emptyList() : this.additionalVariables);
        if (this.variables != null) {
            uniqueAdditionalVariables.removeAll(this.variables);
        }
        if (relevantIndices != null) {
            if (uniqueAdditionalVariables.isEmpty()) {
                relevantAllIndices = relevantIndices;
            } else {
                relevantAllIndices = new LNGIntVector(relevantIndices.size() + uniqueAdditionalVariables.size());
                for (int i = 0; i < relevantIndices.size(); ++i) {
                    relevantAllIndices.push(relevantIndices.get(i));
                }
                for (final Variable var : uniqueAdditionalVariables) {
                    relevantAllIndices.push(solver.underlyingSolver().idxForName(var.name()));
                }
            }
        }
        final Map<String, Integer> name2idx = solver.underlyingSolver().getName2idx();
        final boolean continueLoop = modelEnumerationSATCall(solver, this.handler);
        if (!continueLoop) {
            return modelCount;
        }
        while (true) {
            final Set<Formula> formulasOnSolver = solver.execute(FormulaOnSolverFunction.get());
            if (formulasOnSolver.isEmpty()) {
                System.out.println("Somethings wrong");
            }
            System.out.println("formulas on solver: " + formulasOnSolver);
            final Formula formulaOnSolver = solver.factory().and(formulasOnSolver);
            if (formulaOnSolver.holds(new TautologyPredicate(solver.factory()))) {
                final int numberOfVars = solver.underlyingSolver().getName2idx().size();
                modelCount = modelCount.add(BigInteger.valueOf(2).pow(numberOfVars));
                return modelCount;
            }
            final SortedSet<Literal> minimumPrimeImplicant = MinimumPrimeImplicantFunction.get().apply(formulaOnSolver, true);
            System.out.println("minimumPrimeImplicant = " + minimumPrimeImplicant);

            if (minimumPrimeImplicant != null) {
                final LNGIntVector blockingClause = new LNGIntVector(minimumPrimeImplicant.size());
                for (final Literal lit : minimumPrimeImplicant) {
                    final int idx = solver.underlyingSolver().idxForName(lit.name());
                    blockingClause.push(lit.phase() ? idx * 2 ^ 1 : idx * 2);
                }
                final long numberOfVarsWithoutAuxiliary = name2idx.keySet().stream().filter(x -> !x.startsWith("@")).count();

                final int dontCareSize = (int) (numberOfVarsWithoutAuxiliary - minimumPrimeImplicant.size());
                modelCount = modelCount.add(BigInteger.valueOf(2).pow(dontCareSize));
                solver.underlyingSolver().addClause(blockingClause, null);
                resultSetter.accept(UNDEF);
            } else {
                break;
            }
        }
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            solver.loadState(stateBeforeEnumeration);
        }
        return modelCount;
    }

    private boolean modelEnumerationSATCall(final MiniSat solver, final ModelEnumerationHandler handler) {
        if (handler == null) {
            return solver.sat((SATHandler) null) == TRUE;
        }
        final Tristate tristate = solver.sat(handler.satHandler());
        return !handler.aborted() && tristate == TRUE;
    }

    /**
     * The builder for a model counting function.
     */
    public static class Builder {
        private ModelEnumerationHandler handler;
        private Collection<Variable> relevantVariables;
        private Collection<Variable> variables;
        private Collection<Variable> additionalVariables;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the model enumeration handler for this function
         * @param handler the handler
         * @return the current builder
         */
        public ModelCounterFunction.Builder handler(final ModelEnumerationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public ModelCounterFunction.Builder relevantVariables(final Collection<Variable> variables) {
            this.relevantVariables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public ModelCounterFunction.Builder relevantVariables(final Variable... variables) {
            this.relevantVariables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public ModelCounterFunction.Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public ModelCounterFunction.Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public ModelCounterFunction.Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = variables;
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public ModelCounterFunction.Builder additionalVariables(final Variable... variables) {
            this.additionalVariables = Arrays.asList(variables);
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public ModelCounterFunction build() {
            return new ModelCounterFunction(this.handler, this.variables, this.additionalVariables);
        }
    }

}
