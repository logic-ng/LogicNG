///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.solvers.maxsat.algorithms;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

import java.io.PrintStream;

/**
 * The configuration object for a MaxSAT solver.
 * @version 2.0.0
 * @since 1.0
 */
public final class MaxSATConfig extends Configuration {

    /**
     * The solver type of the underlying SAT solver.
     */
    public enum SolverType {
        MINISAT, GLUCOSE
    }

    /**
     * The incremental strategy for cardinality and pseudo-boolean constraints.
     */
    public enum IncrementalStrategy {
        NONE, ITERATIVE
    }

    /**
     * The AMO encoding.
     */
    public enum AMOEncoding {
        LADDER
    }

    /**
     * The pseudo Boolean encoding.
     */
    public enum PBEncoding {
        SWC
    }

    /**
     * The cardinality constraint encoding.
     */
    public enum CardinalityEncoding {
        TOTALIZER, MTOTALIZER
    }

    /**
     * The weight strategy.
     */
    public enum WeightStrategy {
        NONE, NORMAL, DIVERSIFY
    }

    /**
     * The verbosity of the solver.
     */
    public enum Verbosity {
        NONE, SOME
    }

    final IncrementalStrategy incrementalStrategy;
    final AMOEncoding amoEncoding;
    final PBEncoding pbEncoding;
    final CardinalityEncoding cardinalityEncoding;
    final WeightStrategy weightStrategy;
    final SolverType solverType;
    final Verbosity verbosity;
    final PrintStream output;
    final boolean symmetry;
    final int limit;
    final boolean bmo;

    /**
     * Constructor for a MaxSAT configuration.
     * @param builder the builder
     */
    private MaxSATConfig(final Builder builder) {
        super(ConfigurationType.MAXSAT);
        this.incrementalStrategy = builder.incrementalStrategy;
        this.amoEncoding = builder.amoEncoding;
        this.pbEncoding = builder.pbEncoding;
        this.cardinalityEncoding = builder.cardinalityEncoding;
        this.weightStrategy = builder.weightStrategy;
        this.solverType = builder.solverType;
        this.verbosity = builder.verbosity;
        this.output = builder.output;
        this.symmetry = builder.symmetry;
        this.limit = builder.limit;
        this.bmo = builder.bmo;
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
        final StringBuilder sb = new StringBuilder("MaxSATConfig{").append(System.lineSeparator());
        sb.append("incrementalStrategy=").append(this.incrementalStrategy).append(System.lineSeparator());
        sb.append("pbEncoding=").append(this.amoEncoding).append(System.lineSeparator());
        sb.append("pbEncoding=").append(this.pbEncoding).append(System.lineSeparator());
        sb.append("cardinalityEncoding=").append(this.cardinalityEncoding).append(System.lineSeparator());
        sb.append("weightStrategy=").append(this.weightStrategy).append(System.lineSeparator());
        sb.append("solverType=").append(this.solverType).append(System.lineSeparator());
        sb.append("verbosity=").append(this.verbosity).append(System.lineSeparator());
        sb.append("symmetry=").append(this.symmetry).append(System.lineSeparator());
        sb.append("limit=").append(this.limit).append(System.lineSeparator());
        sb.append("bmo=").append(this.bmo).append(System.lineSeparator());
        sb.append("}");
        return sb.toString();
    }

    /**
     * The builder for a MaxSAT configuration.
     */
    public static class Builder {
        private final AMOEncoding amoEncoding;
        private final PBEncoding pbEncoding;
        private IncrementalStrategy incrementalStrategy = IncrementalStrategy.NONE;
        private CardinalityEncoding cardinalityEncoding = CardinalityEncoding.TOTALIZER;
        private WeightStrategy weightStrategy = WeightStrategy.NONE;
        private SolverType solverType = SolverType.GLUCOSE;
        private Verbosity verbosity = Verbosity.NONE;
        private PrintStream output = System.out;
        private boolean symmetry = true;
        private int limit = Integer.MAX_VALUE;
        private boolean bmo = true;

        /**
         * Constructor for the builder.
         */
        private Builder() {
            this.amoEncoding = AMOEncoding.LADDER;
            this.pbEncoding = PBEncoding.SWC;
        }

        /**
         * Sets the incremental strategy. The default value is {@code NONE}.
         * @param inc the incremental strategy
         * @return the builder
         */
        public Builder incremental(final IncrementalStrategy inc) {
            this.incrementalStrategy = inc;
            return this;
        }

        /**
         * Sets the cardinality encoding. The default value is {@code TOTALIZER}.
         * @param card the cardinality encoding
         * @return the builder
         */
        public Builder cardinality(final CardinalityEncoding card) {
            this.cardinalityEncoding = card;
            return this;
        }

        /**
         * Sets the weight strategy. The default value is {@code NONE}.
         * @param weight the weight strategy
         * @return the builder
         */
        public Builder weight(final WeightStrategy weight) {
            this.weightStrategy = weight;
            return this;
        }

        /**
         * Sets the underlying solver type. The default value is {@code GLUCOSE}.
         * @param solver the underlying solver type
         * @return the builder
         */
        public Builder solver(final SolverType solver) {
            this.solverType = solver;
            return this;
        }

        /**
         * Enables symmetry handling. The default value is {@code true}.
         * @param symm {code true} if symmetry handling should be activated, {@code false} otherwise
         * @return the builder
         */
        public Builder symmetry(final boolean symm) {
            this.symmetry = symm;
            return this;
        }

        /**
         * Sets the symmetry limit. The default value is {@code Integer.MAX_VALUE}.
         * @param lim the symmetry limit
         * @return the builder
         */
        public Builder limit(final int lim) {
            this.limit = lim;
            return this;
        }

        /**
         * Enables BMO (Boolean Multilevel Optimization). The default value is {@code true}.
         * @param bmo {code true} if BMO should be activated, {@code false} otherwise
         * @return the builder
         */
        public Builder bmo(final boolean bmo) {
            this.bmo = bmo;
            return this;
        }

        /**
         * Sets the verbosity. The default value is {@code NONE}.  If you set the verbosity to {@code SOME} you have also to
         * set an output stream.
         * @param verb the verbosity level
         * @return the builder
         */
        public Builder verbosity(final Verbosity verb) {
            this.verbosity = verb;
            return this;
        }

        /**
         * Sets the output stream for logging information.  The default ist {@code System.out}.
         * @param output the output stream for logging information
         * @return the builder
         */
        public Builder output(final PrintStream output) {
            this.output = output;
            return this;
        }

        /**
         * Builds the configuration.
         * @return the configuration.
         */
        public MaxSATConfig build() {
            return new MaxSATConfig(this);
        }
    }
}
