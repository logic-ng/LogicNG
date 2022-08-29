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

package org.logicng.solvers.functions;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.functions.splitvariablesprovider.MostCommonVariablesProvider;
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;

/**
 * The configuration object for the {@link AdvancedModelEnumerationFunction}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class AdvancedModelEnumerationConfig extends Configuration {

    final AdvancedModelEnumerationHandler handler;
    final SplitVariableProvider splitVariableProvider;
    final int maxNumberOfModels;

    /**
     * Constructs a new configuration with a given type.
     * @param builder the builder
     */
    private AdvancedModelEnumerationConfig(final Builder builder) {
        super(ConfigurationType.ADVANCED_MODEL_ENUMERATION);
        this.handler = builder.handler;
        this.splitVariableProvider = builder.splitVariableProvider;
        this.maxNumberOfModels = builder.maxNumberOfModels;
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
     */
    public static class Builder {
        protected AdvancedModelEnumerationHandler handler = null;
        protected SplitVariableProvider splitVariableProvider = new MostCommonVariablesProvider();
        protected int maxNumberOfModels = 500;

        Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the model enumeration handler for this function.  The default is no handler.
         * @param handler the handler
         * @return the current builder
         */
        public Builder handler(final AdvancedModelEnumerationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the split variable provider. If no split variable provider is given, enumeration is performed without splits. Otherwise, the enumeration is
         * performed with the split variables provided by the {@link SplitVariableProvider}.  The default value is the most common variables provider.
         * @param splitVariableProvider the given split variable provider
         * @return the builder
         */
        public Builder splitVariableProvider(final SplitVariableProvider splitVariableProvider) {
            this.splitVariableProvider = splitVariableProvider;
            return this;
        }

        /**
         * The maximum number of models before a model split is performed.  In order to guarantee termination of the enumeration algorithm,
         * this number must be &gt; 2.  If a smaller number is provided, it is automatically set to 3.  The default value is 500.
         * @param maxNumberOfModels the maximum number of models
         * @return the builder
         */
        public Builder maxNumberOfModels(final int maxNumberOfModels) {
            this.maxNumberOfModels = Math.max(maxNumberOfModels, 3);
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
