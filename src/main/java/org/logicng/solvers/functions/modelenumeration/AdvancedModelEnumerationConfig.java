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

package org.logicng.solvers.functions.modelenumeration;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.functions.AdvancedModelEnumerationFunction;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.MostCommonVariablesProvider;

/**
 * The configuration object for the {@link AdvancedModelEnumerationFunction}.
 * @version 2.5.0
 * @since 2.5.0
 */
public class AdvancedModelEnumerationConfig extends Configuration {

    final AdvancedModelEnumerationHandler handler;
    final AdvancedModelEnumerationStrategy strategy;

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    private AdvancedModelEnumerationConfig(final Builder builder) {
        super(ConfigurationType.ADVANCED_MODEL_ENUMERATION);
        this.handler = builder.handler;
        this.strategy = builder.strategy;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder for a model enumeration configuration.
     * @version 2.4.0
     * @since 2.4.0
     */
    public static class Builder {
        private AdvancedModelEnumerationHandler handler = null;
        private AdvancedModelEnumerationStrategy strategy = DefaultAdvancedModelEnumerationStrategy.builder().build();

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the model enumeration handler for this function.  The default is no handler.
         * @param handler the handler, may be {@code null}
         * @return the current builder
         */
        public Builder handler(final AdvancedModelEnumerationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the model enumeration strategy for this function. The default is the {@link DefaultAdvancedModelEnumerationStrategy} with the
         * {@link MostCommonVariablesProvider} and a maximum number of models of 500.
         * <p>
         * In case of {@code null} the computation will fall back to the default model enumeration without split assignments
         * @param strategy the strategy
         * @return the current builder
         */
        public Builder strategy(final AdvancedModelEnumerationStrategy strategy) {
            this.strategy = strategy;
            return this;
        }

        /**
         * Builds the model enumeration configuration with the current builder's configuration.
         * @return the model enumeration configuration
         */
        public AdvancedModelEnumerationConfig build() {
            return new AdvancedModelEnumerationConfig(this);
        }
    }
}
