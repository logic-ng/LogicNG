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

package org.logicng.backbones;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration object for the Backbone generation.
 * @version 1.5.0
 * @since 1.5.0
 */
public class BackboneConfig extends Configuration {

    private final boolean initialUBCheckForRotatableLiterals;
    private final boolean checkForComplementModelLiterals;
    private final boolean checkForRotatableLiterals;

    /**
     * Constructs a new backbone configuration from a given builder.
     * @param builder the builder
     */
    private BackboneConfig(final Builder builder) {
        super(ConfigurationType.BACKBONE);
        this.initialUBCheckForRotatableLiterals = builder.initialUBCheckForRotatableLiterals;
        this.checkForComplementModelLiterals = builder.checkForComplementModelLiterals;
        this.checkForRotatableLiterals = builder.checkForRotatableLiterals;
    }

    /**
     * Returns whether the algorithm should check for rotatable literals during initial unit propagation.
     * @return whether the algorithm should check for rotatable literals during initial unit propagation
     */
    public boolean isInitialUBCheckForRotatableLiterals() {
        return this.initialUBCheckForRotatableLiterals;
    }

    /**
     * Returns whether the algorithm should check for complement model literals.
     * @return whether the algorithm should check for complement model literals
     */
    public boolean isCheckForComplementModelLiterals() {
        return this.checkForComplementModelLiterals;
    }

    /**
     * Returns whether the algorithm should check for rotatable literals.
     * @return whether the algorithm should check for rotatable literals
     */
    public boolean isCheckForRotatableLiterals() {
        return this.checkForRotatableLiterals;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BackboneConfig{").append(System.lineSeparator());
        sb.append("initialUBCheckForRotatableLiterals=").append(this.initialUBCheckForRotatableLiterals).append(System.lineSeparator());
        sb.append("checkForComplementModelLiterals=").append(this.checkForComplementModelLiterals).append(System.lineSeparator());
        sb.append("checkForRotatableLiterals=").append(this.checkForRotatableLiterals).append(System.lineSeparator());
        sb.append("}").append(System.lineSeparator());
        return sb.toString();
    }

    /**
     * The builder for a Backbone configuration.
     */
    public static class Builder {
        private boolean initialUBCheckForRotatableLiterals = true;
        private boolean checkForComplementModelLiterals = true;
        private boolean checkForRotatableLiterals = true;

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
         * Sets whether the algorithm should check for complement model literals. The default value is {@code true}.
         * @param checkForComplementModelLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                        complement literals or {@code false} otherwise.
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
