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

package org.logicng.transformations.simplification;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.solvers.maxsat.OptimizationConfig;

/**
 * The configuration object for the {@link AdvancedSimplifier}.
 * @version 2.6.0
 * @since 2.3.0
 */

public class AdvancedSimplifierConfig extends Configuration {

    boolean restrictBackbone;
    boolean minimalDnfCover;
    boolean factorOut;
    boolean simplifyNegations;
    boolean returnIntermediateResult;
    RatingFunction<?> ratingFunction;
    OptimizationConfig optimizationConfig;

    @Override
    public String toString() {
        return "AdvancedSimplifierConfig{" +
                "restrictBackbone=" + this.restrictBackbone +
                ", minimalDnfCover=" + this.minimalDnfCover +
                ", factorOut=" + this.factorOut +
                ", simplifyNegations=" + this.simplifyNegations +
                ", returnIntermediateResult=" + this.returnIntermediateResult +
                ", ratingFunction=" + this.ratingFunction +
                ", optimizationConfig=" + this.optimizationConfig +
                '}';
    }

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    private AdvancedSimplifierConfig(final Builder builder) {
        super(ConfigurationType.ADVANCED_SIMPLIFIER);
        this.restrictBackbone = builder.restrictBackbone;
        this.minimalDnfCover = builder.minimalDnfCover;
        this.factorOut = builder.factorOut;
        this.simplifyNegations = builder.simplifyNegations;
        this.returnIntermediateResult = builder.returnIntermediateResult;
        this.ratingFunction = builder.ratingFunction;
        this.optimizationConfig = builder.optimizationConfig;
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

        private boolean restrictBackbone = true;
        private boolean minimalDnfCover = true;
        private boolean factorOut = true;
        private boolean simplifyNegations = true;
        private boolean returnIntermediateResult = false;
        private RatingFunction<?> ratingFunction = DefaultRatingFunction.get();
        private OptimizationConfig optimizationConfig = OptimizationConfig.sat(null);

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the flag for whether the formula should be restricted with the
         * backbone. The default is 'true'.
         * @param restrictBackbone flag for the restriction
         * @return the current builder
         */
        public Builder restrictBackbone(final boolean restrictBackbone) {
            this.restrictBackbone = restrictBackbone;
            return this;
        }

        /**
         * Sets the flag for whether a minimal DNF cover should be computed. The
         * default is 'true'.
         * @param minimalDnfCover flag for the minimal DNF cover computation
         * @return the current builder
         */
        public Builder minimalDnfCover(final boolean minimalDnfCover) {
            this.minimalDnfCover = minimalDnfCover;
            return this;
        }

        /**
         * Sets the flag for whether the formula should be factorized. The
         * default is 'true'.
         * @param factorOut flag for the factorisation
         * @return the current builder
         */
        public Builder factorOut(final boolean factorOut) {
            this.factorOut = factorOut;
            return this;
        }

        /**
         * Sets the flag for whether negations shall be simplified. The
         * default is 'true'.
         * @param simplifyNegations flag
         * @return the current builder
         */
        public Builder simplifyNegations(final boolean simplifyNegations) {
            this.simplifyNegations = simplifyNegations;
            return this;
        }

        /**
         * Sets the rating function. The aim of the simplification is to
         * minimize the formula with respect to this rating function,
         * e.g. finding a formula with a minimal number of symbols when
         * represented as string. The default is the
         * {@code DefaultRatingFunction}.
         * @param ratingFunction the desired rating function
         * @return the current builder
         */
        public Builder ratingFunction(final RatingFunction<?> ratingFunction) {
            this.ratingFunction = ratingFunction;
            return this;
        }

        /**
         * Sets the handler to control the computation. The default is
         * 'no handler'.
         * @param handler the optimization handler
         * @return the current builder
         * @deprecated use the {@link #optimizationConfig}
         */
        @Deprecated
        public Builder handler(final OptimizationHandler handler) {
            this.optimizationConfig = OptimizationConfig.sat(handler);
            return this;
        }

        /**
         * Sets the optimization config of the computation.  The default is a
         * config with a SAT based optimization without a handler.
         * @param config the optimization config
         * @return the current builder
         */
        public Builder optimizationConfig(final OptimizationConfig config) {
            this.optimizationConfig = config;
            return this;
        }

        /**
         * Sets whether the intermediate result should be returned in case of
         * a handler abortion.  The default is 'false'.
         * @param returnIntermediateResult the flag
         * @return the current builder
         */
        public Builder returnIntermediateResult(final boolean returnIntermediateResult) {
            this.returnIntermediateResult = returnIntermediateResult;
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
