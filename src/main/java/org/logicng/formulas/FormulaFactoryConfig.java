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

package org.logicng.formulas;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.printer.DefaultStringRepresentation;
import org.logicng.formulas.printer.FormulaStringRepresentation;

import java.util.function.Supplier;

/**
 * The configuration object for a formula factory.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FormulaFactoryConfig extends Configuration {

    /**
     * Strategies for combining formulas of different formula factories.
     * Possible values are:
     * <ul>
     *     <li>{@link #PANIC}: If an operand of a formula comes from a different formula factory
     *     an {@link UnsupportedOperationException} is thrown</li>
     *     <li>{@link #IMPORT}: Operands from different formula factories are {@link FormulaFactory#importFormula(Formula) imported}
     *     before the new formula is constructed</li>
     * </ul>
     */
    public enum FormulaMergeStrategy {
        PANIC,
        IMPORT
    }

    final String name;
    final FormulaMergeStrategy formulaMergeStrategy;
    final Supplier<FormulaStringRepresentation> stringRepresentation;
    final boolean simplifyComplementaryOperands;

    private FormulaFactoryConfig(final Builder builder) {
        super(ConfigurationType.FORMULA_FACTORY);
        this.name = builder.name;
        this.formulaMergeStrategy = builder.formulaMergeStrategy;
        this.stringRepresentation = builder.stringRepresentation;
        this.simplifyComplementaryOperands = builder.simplifyComplementaryOperands;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder for a formula factory configuration.
     * @version 2.0.0
     * @since 2.0.0
     */
    public static class Builder {
        private String name = "";
        private FormulaMergeStrategy formulaMergeStrategy = FormulaMergeStrategy.PANIC;
        private Supplier<FormulaStringRepresentation> stringRepresentation = DefaultStringRepresentation::new;
        private boolean simplifyComplementaryOperands = true;

        /**
         * Sets the name of this formula factory. The default is an empty string.
         * <p>
         * Setting a name is only useful when multiple formula factories are used in the same context.
         * The name is used to create individual names for generated variables s.t. the generated variables
         * of different formula factories will not clash.
         * @param name the name
         * @return the builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * Sets the strategy defining how to proceed if one or more operands of a formula
         * were created by another formula factory. The default value is {@link FormulaMergeStrategy#PANIC}.
         * @param formulaMergeStrategy the strategy
         * @return the builder
         */
        public Builder formulaMergeStrategy(final FormulaMergeStrategy formulaMergeStrategy) {
            this.formulaMergeStrategy = formulaMergeStrategy;
            return this;
        }

        /**
         * Sets the formula string representation which should be used by default for creating strings
         * from a formula. The default is {@link DefaultStringRepresentation}.
         * @param stringRepresentation the formula string representation
         * @return the builder
         */
        public Builder stringRepresentation(final Supplier<FormulaStringRepresentation> stringRepresentation) {
            this.stringRepresentation = stringRepresentation;
            return this;
        }

        /**
         * Sets the flag whether trivial contradictions and tautologies are simplified in formulas.
         * If set to false, a formula like {@code A & ~A} or {@code A | ~A} can be generated on the
         * formula factory.  If set to true, the formulas will be simplified to {@code $false} or
         * {@code true} respectively.  The default is {@code true}.
         * @param simplifyComplementaryOperands the flag whether to simplify trivial
         *                                      contradictions and tautologies or not
         * @return the builder
         */
        public Builder simplifyComplementaryOperands(final boolean simplifyComplementaryOperands) {
            this.simplifyComplementaryOperands = simplifyComplementaryOperands;
            return this;
        }

        /**
         * Builds the configuration.
         * @return the configuration.
         */
        public FormulaFactoryConfig build() {
            return new FormulaFactoryConfig(this);
        }
    }
}
