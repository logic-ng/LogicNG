package org.logicng.solvers.functions;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.splitvariablesprovider.MostCommonVariablesProvider;
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * A default strategy for the advanced model enumeration.
 * <p>
 * It takes a {@link SplitVariableProvider} and a maximum number of models.
 * <p>
 * The split variable provider is used to compute the initial split variables on recursion depth 0.
 * Afterwards (including the {@link #reduceSplitVars the reduction of split variables}), always the
 * first half of the variables is returned.
 * <p>
 * {@link #maxNumberOfModels} is always returned for both {@link #maxNumberOfModelsForEnumeration}
 * and {@link #maxNumberOfModelsForSplitAssignments}, ignoring the recursion depth.
 * <p>
 * This class can potentially be extended if you want to fine-tune some methods, e.g. to change the
 * maximum number of models depending on the recursion depth or whether the models are required for
 * enumeration or for split assignments.
 * @version 2.4.0
 * @since 2.4.0
 */
public class DefaultAdvancedModelEnumerationStrategy implements AdvancedModelEnumerationStrategy {

    protected static final int MIN_NUMBER_OF_MODELS = 3;

    protected final SplitVariableProvider splitVariableProvider;
    protected final int maxNumberOfModels;

    /**
     * Constructs a new default advanced model enumeration strategy.
     * @param splitVariableProvider the split variables provider
     * @param maxNumberOfModels     the maximum number of models before a model split is performed.  In order to guarantee termination of the enumeration algorithm,
     *                              this number must be &gt; 2.  If a smaller number is provided, it is automatically set to 3.
     */
    protected DefaultAdvancedModelEnumerationStrategy(final SplitVariableProvider splitVariableProvider, final int maxNumberOfModels) {
        this.splitVariableProvider = splitVariableProvider;
        this.maxNumberOfModels = maxNumberOfModels;
    }

    /**
     * Returns a new builder for the strategy.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public int maxNumberOfModelsForEnumeration(final int recursionDepth) {
        return Math.max(this.maxNumberOfModels, MIN_NUMBER_OF_MODELS);
    }

    @Override
    public int maxNumberOfModelsForSplitAssignments(final int recursionDepth) {
        return Math.max(this.maxNumberOfModels, MIN_NUMBER_OF_MODELS);
    }

    @Override
    public SortedSet<Variable> splitVarsForRecursionDepth(final Collection<Variable> variables, final SATSolver solver, final int recursionDepth) {
        if (recursionDepth == 0) {
            return this.splitVariableProvider.getSplitVars(solver, variables);
        } else {
            return reduceSplitVars(variables, recursionDepth);
        }
    }

    @Override
    public SortedSet<Variable> reduceSplitVars(final Collection<Variable> variables, final int recursionDepth) {
        return variables.stream().limit(variables.size() / 2).collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * The builder for a default advanced model enumeration strategy.
     * @version 2.4.0
     * @since 2.4.0
     */
    public static class Builder {
        private SplitVariableProvider splitVariableProvider = new MostCommonVariablesProvider();
        private int maxNumberOfModels = 500;

        private Builder() {
        }

        /**
         * Sets the split variable provider for this strategy. The default is {@link MostCommonVariablesProvider}.
         * @param splitVariableProvider the split variable provider
         * @return the builder
         */
        public Builder splitVariableProvider(final SplitVariableProvider splitVariableProvider) {
            this.splitVariableProvider = splitVariableProvider;
            return this;
        }

        /**
         * Sets the maximum number of models to be enumerated in a single step (either final enumeration or enumeration of split assignments).
         * The default is 500.
         * @param maxNumberOfModels the maximum number of models to be enumerated in a single step
         * @return the builder
         */
        public Builder maxNumberOfModels(final int maxNumberOfModels) {
            this.maxNumberOfModels = maxNumberOfModels;
            return this;
        }

        /**
         * Returns the strategy.
         * @return the strategy
         */
        public DefaultAdvancedModelEnumerationStrategy build() {
            return new DefaultAdvancedModelEnumerationStrategy(this.splitVariableProvider, this.maxNumberOfModels);
        }
    }
}
