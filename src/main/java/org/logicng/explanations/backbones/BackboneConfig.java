package org.logicng.explanations.backbones;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration object for the Backbone generation.
 * @version 1.5
 * @since 1.5
 */
public class BackboneConfig extends Configuration {

    /**
     * The algorithm for the Backbone generation.
     */
    public enum Algorithm {
        MINI_SAT_BACKBONE
    }
    final Algorithm algorithm;

    /**
     * Creation of initial candidate literals
     */
    private final boolean initialLBCheckForUPZeroLiterals;
    private final boolean initialUBCheckForRotatableLiterals;

    /**
     * Reductions during search
     */
    private final boolean checkForUPZeroLiterals;
    private final boolean checkForComplementModelLiterals;
    private final boolean checkForRotatableLiterals;

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    public BackboneConfig(final Builder builder) {
        super(ConfigurationType.BACKBONE);
        this.algorithm = builder.algorithm;
        this.initialLBCheckForUPZeroLiterals = builder.initialLBCheckForUPZeroLiterals;
        this.initialUBCheckForRotatableLiterals = builder.initialUBCheckForRotatableLiterals;
        this.checkForUPZeroLiterals = builder.checkForUPZeroLiterals;
        this.checkForComplementModelLiterals = builder.checkForComplementModelLiterals;
        this.checkForRotatableLiterals = builder.checkForRotatableLiterals;
    }

    public boolean isInitialLBCheckForUPZeroLiterals() {
        return this.initialLBCheckForUPZeroLiterals;
    }

    public boolean isInitialUBCheckForRotatableLiterals() {
        return this.initialUBCheckForRotatableLiterals;
    }

    public boolean isCheckForUPZeroLiterals() {
        return this.checkForUPZeroLiterals;
    }

    public boolean isCheckForComplementModelLiterals() {
        return this.checkForComplementModelLiterals;
    }

    public boolean isCheckForRotatableLiterals() {
        return this.checkForRotatableLiterals;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BackboneConfig{").append(System.lineSeparator());
        sb.append("initialLBCheckForUPZeroLiterals=").append(this.initialLBCheckForUPZeroLiterals).append(System.lineSeparator());
        sb.append("initialUBCheckForRotatableLiterals=").append(this.initialUBCheckForRotatableLiterals).append(System.lineSeparator());
        sb.append("checkForUPZeroLiterals=").append(this.checkForUPZeroLiterals).append(System.lineSeparator());
        sb.append("checkForComplementModelLiterals=").append(this.checkForComplementModelLiterals).append(System.lineSeparator());
        sb.append("checkForRotatableLiterals=").append(this.checkForRotatableLiterals).append(System.lineSeparator());
        sb.append("}").append(System.lineSeparator());
        return sb.toString();
    }

    /**
     * The builder for a Backbone configuration.
     */
    public static class Builder {

        /**
         * Default configurations
         */
        private Algorithm algorithm = Algorithm.MINI_SAT_BACKBONE;
        private boolean initialLBCheckForUPZeroLiterals = true;
        private boolean initialUBCheckForRotatableLiterals = true;
        private boolean checkForUPZeroLiterals = true;
        private boolean checkForComplementModelLiterals = true;
        private boolean checkForRotatableLiterals = true;

        /**
         * Sets the algorithm for the Backbone generation. The default value is {@code MINI_SAT_BACKBONE}.
         * @param algorithm the algorithm for the Backbone generation
         * @return the builder
         */
        public Builder algorithm(final Algorithm algorithm) {
            this.algorithm = algorithm;
            return this;
        }

        /**
         * Sets whether the algorithm should check for rotatable literals. The default value is {@code true}.
         * @param checkForRotatableLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                  rotatables or {@code false} otherwise.
         * @return the builder
         */
        public BackboneConfig.Builder checkForRotatableLiterals(final boolean checkForRotatableLiterals) {
            this.checkForRotatableLiterals = checkForRotatableLiterals;
            return this;
        }

        /**
         * Sets whether the algorithm should check for rotatable literals during initial unit propagation. The default
         * value is {@code true}.
         * @param initialUBCheckForRotatableLiterals the boolean value that is {@code true} if the algorithm should
         *                                           check for rotatables or {@code false} otherwise.
         * @return the builder
         */
        public BackboneConfig.Builder initialUBCheckForRotatableLiterals(final boolean initialUBCheckForRotatableLiterals) {
            this.initialUBCheckForRotatableLiterals = initialUBCheckForRotatableLiterals;
            return this;
        }

        /**
         * Sets whether the algorithm should check for zero literals in the unit propagation. The default value is
         * {@code true}.
         * @param checkForUPZeroLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                  zero literals or {@code false} otherwise.
         * @return the builder
         */
        public BackboneConfig.Builder checkForUPZeroLiterals(final boolean checkForUPZeroLiterals) {
            this.checkForUPZeroLiterals = checkForUPZeroLiterals;
            return this;
        }

        /**
         * Sets whether the algorithm should check for zero literals. The default value is {@code true}.
         * @param initialLBCheckForUPZeroLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                  zero or {@code false} otherwise.
         * @return the builder
         */
        public BackboneConfig.Builder initialLBCheckForUPZeroLiterals(final boolean initialLBCheckForUPZeroLiterals) {
            this.initialLBCheckForUPZeroLiterals = initialLBCheckForUPZeroLiterals;
            return this;
        }

        /**
         * Sets whether the algorithm should check for complement model literals. The default value is {@code true}.
         * @param checkForComplementModelLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                  complement literals or {@code false} otherwise.
         * @return the builder
         */
        public BackboneConfig.Builder checkForComplementModelLiterals(final boolean checkForComplementModelLiterals) {
            this.checkForComplementModelLiterals = checkForComplementModelLiterals;
            return this;
        }

        /**
         * Builds the configuration.
         * @return the configuration.
         */
        public BackboneConfig build() {
            return new BackboneConfig(this);
        }
    }
}
