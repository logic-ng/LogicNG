package org.logicng.solvers.functions;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.handlers.Handler.start;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
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
import java.util.stream.Collectors;

public class ModelCounterFunction implements SolverFunction<BigInteger> {
    private final SATHandler handler;
    private final Collection<Variable> variables;
    private final Collection<Variable> additionalVariables;

    private ModelCounterFunction(final SATHandler handler, final Collection<Variable> variables,
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

        // if (this.variables != null) {
        //     final Set<String> irrelevantVars = name2idx.keySet().stream().filter(this::isRelevantVariable).collect(Collectors.toSet());
        //     this.variables.stream().map(Literal::name).collect(Collectors.toList()).forEach(irrelevantVars::remove);
        //     for (final String var : irrelevantVars) {
        //         final Variable variable = solver.factory().variable(var);
        //         final Tristate sat = solver.sat(variable);
        //         if (sat == Tristate.TRUE) {
        //             solver.add(variable);
        //         } else {
        //             solver.add(variable.negate());
        //         }
        //     }
        // }
        while (true) {
            final LNGIntVector primeImplicant = solver.execute(PrimeImplicantFunction.builder().handler(handler).isMinimal(true).build());
            if (primeImplicant != null) {
                final LNGIntVector blockingClause = new LNGIntVector(primeImplicant.size());
                for (int i = 0; i < primeImplicant.size(); i++) {
                    blockingClause.push(primeImplicant.get(i) ^ 1);
                }
                final Set<String> allVariables = name2idx.keySet().stream().filter(this::isRelevantVariable).collect(Collectors.toSet());
                final int allVarsSize = allVariables.size();
                // if (this.variables != null) {
                //     allVarsSize = relevantAllIndices.size();
                // } else {
                //     allVarsSize = allVariables.size();
                // }
                final int dontCareSize = allVarsSize - blockingClause.size();
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

    final boolean isRelevantVariable(final String name) {
        return !name.startsWith(FormulaFactory.CNF_PREFIX) && !name.startsWith(FormulaFactory.CC_PREFIX) && !name.startsWith(FormulaFactory.PB_PREFIX);
    }

    private boolean modelEnumerationSATCall(final MiniSat solver, final SATHandler handler) {
        if (handler == null) {
            return solver.sat((SATHandler) null) == TRUE;
        }
        final Tristate tristate = solver.sat(handler);
        return !handler.aborted() && tristate == TRUE;
    }

    /**
     * The builder for a model counting function.
     */
    public static class Builder {
        private SATHandler handler;
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
        public Builder handler(final SATHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = variables;
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Variable... variables) {
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
