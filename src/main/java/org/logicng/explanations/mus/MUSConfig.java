// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.explanations.mus;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.handlers.SATHandler;

/**
 * The configuration object for the MUS generation.
 * @version 2.1.0
 * @since 1.1
 */
public final class MUSConfig extends Configuration {

    /**
     * The algorithm for the MUS generation.
     */
    public enum Algorithm {
        DELETION,
        PLAIN_INSERTION
    }

    final Algorithm algorithm;
    final SATHandler handler;

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    private MUSConfig(final Builder builder) {
        super(ConfigurationType.MUS);
        this.algorithm = builder.algorithm;
        this.handler = builder.handler;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("MUSConfig{").append(System.lineSeparator());
        sb.append("algorithm=").append(this.algorithm).append(System.lineSeparator());
        sb.append("}").append(System.lineSeparator());
        return sb.toString();
    }

    /**
     * The builder for a MUS configuration.
     */
    public static class Builder {

        private Algorithm algorithm = Algorithm.DELETION;
        private SATHandler handler = null;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the algorithm for the MUS generation. The default value is
         * {@code DELETION}.
         * @param algorithm the algorithm for the MUS generation
         * @return the builder
         */
        public Builder algorithm(final Algorithm algorithm) {
            this.algorithm = algorithm;
            return this;
        }

        /**
         * Sets the SAT handler for the MUS generation.
         * @param handler the SAT handler
         * @return the current builder
         */
        public Builder handler(final SATHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Builds the configuration.
         * @return the configuration.
         */
        public MUSConfig build() {
            return new MUSConfig(this);
        }
    }
}
