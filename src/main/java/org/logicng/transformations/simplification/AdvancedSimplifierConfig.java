package org.logicng.transformations.simplification;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.handlers.OptimizationHandler;

/**
 * The configuration object for the {@link AdvancedSimplifier}.
 * @version 2.3.0
 * @since 2.3.0
 */

public class AdvancedSimplifierConfig extends Configuration {

    boolean restrictBackbone;
    boolean factorOut;
    boolean simplifyNegations;
    RatingFunction<?> ratingFunction;
    OptimizationHandler handler;

    @Override
    public String toString() {
        return "AdvancedSimplifierConfig{" +
                "restrictBackbone=" + restrictBackbone +
                ", factorOut=" + factorOut +
                ", simplifyNegations=" + simplifyNegations +
                ", ratingFunction=" + ratingFunction +
                ", handler=" + handler +
                '}';
    }

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    private AdvancedSimplifierConfig(final Builder builder) {
        super(ConfigurationType.ADVANCED_SIMPLIFIER);
        this.restrictBackbone = builder.restrictBackbone;
        this.factorOut = builder.factorOut;
        this.simplifyNegations = builder.simplifyNegations;
        this.ratingFunction = builder.ratingFunction;
        this.handler = builder.handler;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder for the advanced simplifier configuration.
     */
    public static class Builder {

        boolean restrictBackbone = true;
        boolean factorOut = true;
        boolean simplifyNegations = true;
        private RatingFunction<?> ratingFunction = new DefaultRatingFunction();
        private OptimizationHandler handler = null;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the flag for whether the formula should be restricted with the backbone. The default is 'true'.
         * @param restrictBackbone flag for the restriction
         * @return the current builder
         */
        public Builder restrictBackbone(final boolean restrictBackbone) {
            this.restrictBackbone = restrictBackbone;
            return this;
        }

        /**
         * Sets the flag for whether the formula should be factorized. The default is 'true'.
         * @param factorOut flag for the factorisation
         * @return the current builder
         */
        public Builder factorOut(final boolean factorOut) {
            this.factorOut = factorOut;
            return this;
        }

        /**
         * Sets the flag for whether negations shall be simplified. The default is 'true'.
         * @param simplifyNegations flag
         * @return the current builder
         */
        public Builder simplifyNegations(final boolean simplifyNegations) {
            this.simplifyNegations = simplifyNegations;
            return this;
        }

        /**
         * Sets the rating function. The aim of the simplification is to minimize the formula with respect to this rating function,
         * e.g. finding a formula with a minimal number of symbols when represented as string. The default is the {@code DefaultRatingFunction}.
         * @param ratingFunction the desired rating function
         * @return the current builder
         */
        public Builder ratingFunction(final RatingFunction<?> ratingFunction) {
            this.ratingFunction = ratingFunction;
            return this;
        }

        /**
         * Sets the handler to control the computation. The default is 'no handler'.
         * @param handler the optimization handler
         * @return the current builder
         */
        public Builder handler(final OptimizationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Builds the configuration.
         * @return the configuration.
         */
        public AdvancedSimplifierConfig build() {
            return new AdvancedSimplifierConfig(this);
        }
    }
}
