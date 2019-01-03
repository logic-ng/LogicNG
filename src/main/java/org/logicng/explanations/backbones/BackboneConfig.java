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
        ENUMERATION, ITERATIVE_PLAIN, ITERATIVE_ONE_TEST, ITERATIVE_PLUS, CHUNKING, CORE, CORE_CHUNKING
    }

    final Algorithm algorithm;

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    public BackboneConfig(final Builder builder) {
        super(ConfigurationType.BACKBONE);
        this.algorithm = builder.algorithm;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BackboneConfig{").append(System.lineSeparator());
        sb.append("algorithm=").append(this.algorithm).append(System.lineSeparator());
        sb.append("}").append(System.lineSeparator());
        return sb.toString();
    }

    /**
     * The builder for a Backbone configuration.
     */
    public static class Builder {

        // TODO Change to default algorithm depending on which algorithm is the most efficient!
        private Algorithm algorithm = Algorithm.ITERATIVE_PLAIN;

        /**
         * Sets the algorithm for the Backbone generation. The default value is {@code TODO}.
         * @param algorithm the algorithm for the Backbone generation
         * @return the builder
         */
        public Builder algorithm(final Algorithm algorithm) {
            this.algorithm = algorithm;
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
