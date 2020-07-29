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

package org.logicng.util;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.Variable;

import java.util.Collection;

/**
 * A configuration for randomizing formulas. The following things can be configured:
 * <ul>
 *     <li>the seed -- use a value {@code != 0} to get deterministic results</li>
 *     <li>the variables -- if {@link #variables} is not {@code null} this list of variables will be used,
 *     otherwise {@link #numVars} variables will be generated. The probabilities of being chosen are the
 *     same for all variables.</li>
 *     <li>weights for different formula types, defining how often a formula type is generated compared to other types</li>
 *     <li>weights for comparator types in pseudo boolean constraints and cardinality constraints</li>
 *     <li>maximum numbers of operands for conjunctions, disjunctions, PBCs, and CCs</li>
 * </ul>
 * Note that the weights can only be applied for inner nodes of the generated formula,
 * since the 'leafs' of a formula in LogicNG are <b>always</b> literals or PBCs. So the weight of literals and PBCs
 * will effectively be higher and the weights of all other formula types (especially conjunctions and disjunctions)
 * will be lower. Similarly, the weight of constants will usually be lower, because they are always reduced in LogicNG
 * unless they are a single formula.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FormulaRandomizerConfig extends Configuration {

    final long seed;
    final Collection<Variable> variables;
    final int numVars;
    final double weightConstant;
    final double weightPositiveLiteral;
    final double weightNegativeLiteral;
    final double weightOr;
    final double weightAnd;
    final double weightNot;
    final double weightImpl;
    final double weightEquiv;
    final int maximumOperandsAnd;
    final int maximumOperandsOr;

    final double weightPbc;
    final double weightPbcCoeffPositive;
    final double weightPbcCoeffNegative;
    final double weightPbcTypeLe;
    final double weightPbcTypeLt;
    final double weightPbcTypeGe;
    final double weightPbcTypeGt;
    final double weightPbcTypeEq;
    final int maximumOperandsPbc;
    final int maximumCoefficientPbc;

    final double weightCc;
    final double weightAmo;
    final double weightExo;
    final int maximumOperandsCc;

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("FormulaRandomizerConfig{").append(System.lineSeparator());
        sb.append("seed=").append(this.seed).append(System.lineSeparator());
        sb.append("variables=").append(this.variables).append(System.lineSeparator());
        sb.append("numVars=").append(this.numVars).append(System.lineSeparator());
        sb.append("weightConstant=").append(this.weightConstant).append(System.lineSeparator());
        sb.append("weightPositiveLiteral=").append(this.weightPositiveLiteral).append(System.lineSeparator());
        sb.append("weightNegativeLiteral=").append(this.weightNegativeLiteral).append(System.lineSeparator());
        sb.append("weightOr=").append(this.weightOr).append(System.lineSeparator());
        sb.append("weightAnd=").append(this.weightAnd).append(System.lineSeparator());
        sb.append("weightNot=").append(this.weightNot).append(System.lineSeparator());
        sb.append("weightImpl=").append(this.weightImpl).append(System.lineSeparator());
        sb.append("weightEquiv=").append(this.weightEquiv).append(System.lineSeparator());
        sb.append("maximumOperandsAnd=").append(this.maximumOperandsAnd).append(System.lineSeparator());
        sb.append("maximumOperandsOr=").append(this.maximumOperandsOr).append(System.lineSeparator());
        sb.append("weightPbc=").append(this.weightPbc).append(System.lineSeparator());
        sb.append("weightPbcCoeffPositive=").append(this.weightPbcCoeffPositive).append(System.lineSeparator());
        sb.append("weightPbcCoeffNegative=").append(this.weightPbcCoeffNegative).append(System.lineSeparator());
        sb.append("weightPbcTypeLe=").append(this.weightPbcTypeLe).append(System.lineSeparator());
        sb.append("weightPbcTypeLt=").append(this.weightPbcTypeLt).append(System.lineSeparator());
        sb.append("weightPbcTypeGe=").append(this.weightPbcTypeGe).append(System.lineSeparator());
        sb.append("weightPbcTypeGt=").append(this.weightPbcTypeGt).append(System.lineSeparator());
        sb.append("weightPbcTypeEq=").append(this.weightPbcTypeEq).append(System.lineSeparator());
        sb.append("maximumOperandsPbc=").append(this.maximumOperandsPbc).append(System.lineSeparator());
        sb.append("maximumCoefficientPbc=").append(this.maximumCoefficientPbc).append(System.lineSeparator());
        sb.append("weightCc=").append(this.weightCc).append(System.lineSeparator());
        sb.append("weightAmo=").append(this.weightAmo).append(System.lineSeparator());
        sb.append("weightExo=").append(this.weightExo).append(System.lineSeparator());
        sb.append("maximumOperandsCc=").append(this.maximumOperandsCc).append(System.lineSeparator());
        sb.append('}');
        return sb.toString();
    }

    /**
     * Constructs a new configuration from a given builder.
     * @param builder the builder
     */
    private FormulaRandomizerConfig(final Builder builder) {
        super(ConfigurationType.FORMULA_RANDOMIZER);
        this.seed = builder.seed;
        this.variables = builder.variables;
        this.numVars = builder.numVars;
        this.weightConstant = builder.weightConstant;
        this.weightPositiveLiteral = builder.weightPositiveLiteral;
        this.weightNegativeLiteral = builder.weightNegativeLiteral;
        this.weightOr = builder.weightOr;
        this.weightAnd = builder.weightAnd;
        this.weightNot = builder.weightNot;
        this.weightImpl = builder.weightImpl;
        this.weightEquiv = builder.weightEquiv;
        this.maximumOperandsAnd = builder.maximumOperandsAnd;
        this.maximumOperandsOr = builder.maximumOperandsOr;
        this.weightPbc = builder.weightPbc;
        this.weightPbcCoeffPositive = builder.weightPbcCoeffPositive;
        this.weightPbcCoeffNegative = builder.weightPbcCoeffNegative;
        this.weightPbcTypeLe = builder.weightPbcTypeLe;
        this.weightPbcTypeLt = builder.weightPbcTypeLt;
        this.weightPbcTypeGe = builder.weightPbcTypeGe;
        this.weightPbcTypeGt = builder.weightPbcTypeGt;
        this.weightPbcTypeEq = builder.weightPbcTypeEq;
        this.maximumOperandsPbc = builder.maximumOperandsPbc;
        this.maximumCoefficientPbc = builder.maximumCoefficientPbc;
        this.weightCc = builder.weightCc;
        this.weightAmo = builder.weightAmo;
        this.weightExo = builder.weightExo;
        this.maximumOperandsCc = builder.maximumOperandsCc;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder for a formula randomizer configuration.
     * @version 2.0.0
     * @since 2.0.0
     */
    public static class Builder {
        private long seed = 0;
        private Collection<Variable> variables = null;
        private int numVars = 25;
        private double weightConstant = 0.1;
        private double weightPositiveLiteral = 1.0;
        private double weightNegativeLiteral = 1.0;
        private double weightOr = 30.0;
        private double weightAnd = 30.0;
        private double weightNot = 1.0;
        private double weightImpl = 1.0;
        private double weightEquiv = 1.0;
        private int maximumOperandsAnd = 5;
        private int maximumOperandsOr = 5;
        private double weightPbc = 0.0;
        private double weightPbcCoeffPositive = 1.0;
        private double weightPbcCoeffNegative = 0.2;
        private double weightPbcTypeLe = 0.2;
        private double weightPbcTypeLt = 0.2;
        private double weightPbcTypeGe = 0.2;
        private double weightPbcTypeGt = 0.2;
        private double weightPbcTypeEq = 0.2;
        private int maximumOperandsPbc = 5;
        private int maximumCoefficientPbc = 10;
        private double weightCc = 0.0;
        private double weightAmo = 0.0;
        private double weightExo = 0.0;
        private int maximumOperandsCc = 5;

        /**
         * Builds the formula randomizer configuration.
         * @return the formula randomizer configuration
         */
        public FormulaRandomizerConfig build() {
            return new FormulaRandomizerConfig(this);
        }

        /**
         * Sets the seed of the randomizer (passed to the constructor of {@link java.util.Random}).
         * This provides the possibility to get deterministic random formulas. Just choose an arbitrary
         * number and you get the same random formulas with every run.
         * <p>
         * The default value is 0, meaning that the seed depends on the system time, so determinism
         * is lost.
         * @param seed the seed
         * @return the builder
         */
        public Builder seed(final long seed) {
            this.seed = seed;
            return this;
        }

        /**
         * Sets the variables which may occur in the generated formulas.
         * <p>
         * If this value is set, the {@link #numVars number of variables} will be ignored.
         * <p>
         * The default value is {@code null} (i.e. {@link #numVars} will be used.).
         * @param variables the variables to use
         * @return the builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the number of different variables which may occur in the generated formulas.
         * <p>
         * This value is ignored if also the {@link #variables} are set.
         * <p>
         * The default value is 0.
         * @param numVars the number of different variables.
         * @return the builder
         */
        public Builder numVars(final int numVars) {
            this.numVars = numVars;
            return this;
        }

        /**
         * Sets the relative weight of a constant.
         * @param weightConstant the relative weight of a constant
         * @return the builder
         */
        public Builder weightConstant(final double weightConstant) {
            this.weightConstant = weightConstant;
            return this;
        }

        /**
         * Sets the relative weight of a positive literal.
         * @param weightPositiveLiteral the relative weight of a positive literal
         * @return the builder
         */
        public Builder weightPositiveLiteral(final double weightPositiveLiteral) {
            this.weightPositiveLiteral = weightPositiveLiteral;
            return this;
        }

        /**
         * Sets the relative weight of a negative literal.
         * @param weightNegativeLiteral the relative weight of a negative literal
         * @return the builder
         */
        public Builder weightNegativeLiteral(final double weightNegativeLiteral) {
            this.weightNegativeLiteral = weightNegativeLiteral;
            return this;
        }

        /**
         * Sets the relative weight of a disjuction.
         * @param weightOr the relative weight of a disjuction
         * @return the builder
         */
        public Builder weightOr(final double weightOr) {
            this.weightOr = weightOr;
            return this;
        }

        /**
         * Sets the relative weight of a conjunction.
         * @param weightAnd the relative weight of a conjunction
         * @return the builder
         */
        public Builder weightAnd(final double weightAnd) {
            this.weightAnd = weightAnd;
            return this;
        }

        /**
         * Sets the relative weight of a negation.
         * @param weightNot the relative weight of a negation
         * @return the builder
         */
        public Builder weightNot(final double weightNot) {
            this.weightNot = weightNot;
            return this;
        }

        /**
         * Sets the relative weight of an implication.
         * @param weightImpl the relative weight of an implication
         * @return the builder
         */
        public Builder weightImpl(final double weightImpl) {
            this.weightImpl = weightImpl;
            return this;
        }

        /**
         * Sets the relative weight of an equivalence.
         * @param weightEquiv the relative weight of an equivalence
         * @return the builder
         */
        public Builder weightEquiv(final double weightEquiv) {
            this.weightEquiv = weightEquiv;
            return this;
        }

        /**
         * Sets the maximum number of operands in a conjunction.
         * @param maximumOperandsAnd the maximum number of operands in a conjunction
         * @return the builder
         */
        public Builder maximumOperandsAnd(final int maximumOperandsAnd) {
            this.maximumOperandsAnd = maximumOperandsAnd;
            return this;
        }

        /**
         * Sets the maximum number of operands in a disjunction.
         * @param maximumOperandsOr the maximum number of operands in a disjunction
         * @return the builder
         */
        public Builder maximumOperandsOr(final int maximumOperandsOr) {
            this.maximumOperandsOr = maximumOperandsOr;
            return this;
        }

        /**
         * Sets the relative weight of a pseudo boolean constraint.
         * Note that the constraint may by chance also be a cardinality constraint,
         * or even a literal or a constant.
         * @param weightPbc the relative weight of a pseudo boolean constraint
         * @return the builder
         */
        public Builder weightPbc(final double weightPbc) {
            this.weightPbc = weightPbc;
            return this;
        }

        /**
         * Sets the relative weight of a positive coefficient.
         * @param weightPbcCoeffPositive the relative weight of a positive coefficient
         * @return the builder
         */
        public Builder weightPbcCoeffPositive(final double weightPbcCoeffPositive) {
            this.weightPbcCoeffPositive = weightPbcCoeffPositive;
            return this;
        }

        /**
         * Sets the relative weight of a negative coefficient.
         * @param weightPbcCoeffNegative the relative weight of a negative coefficient
         * @return the builder
         */
        public Builder weightPbcCoeffNegative(final double weightPbcCoeffNegative) {
            this.weightPbcCoeffNegative = weightPbcCoeffNegative;
            return this;
        }

        /**
         * Sets the relative weight of a LE constraint ({@link org.logicng.formulas.CType#LE}.
         * @param weightPbcTypeLe the relative weight of a LE constraint
         * @return the builder
         */
        public Builder weightPbcTypeLe(final double weightPbcTypeLe) {
            this.weightPbcTypeLe = weightPbcTypeLe;
            return this;
        }

        /**
         * Sets the relative weight of a LT constraint ({@link org.logicng.formulas.CType#LT}.
         * @param weightPbcTypeLt the relative weight of a LT constraint
         * @return the builder
         */
        public Builder weightPbcTypeLt(final double weightPbcTypeLt) {
            this.weightPbcTypeLt = weightPbcTypeLt;
            return this;
        }

        /**
         * Sets the relative weight of a GE constraint ({@link org.logicng.formulas.CType#GE}.
         * @param weightPbcTypeGe the relative weight of a GE constraint
         * @return the builder
         */
        public Builder weightPbcTypeGe(final double weightPbcTypeGe) {
            this.weightPbcTypeGe = weightPbcTypeGe;
            return this;
        }

        /**
         * Sets the relative weight of a GT constraint ({@link org.logicng.formulas.CType#GT}.
         * @param weightPbcTypeGt the relative weight of a GT constraint
         * @return the builder
         */
        public Builder weightPbcTypeGt(final double weightPbcTypeGt) {
            this.weightPbcTypeGt = weightPbcTypeGt;
            return this;
        }

        /**
         * Sets the relative weight of a EQ constraint ({@link org.logicng.formulas.CType#EQ}.
         * @param weightPbcTypeEq the relative weight of a EQ constraint
         * @return the builder
         */
        public Builder weightPbcTypeEq(final double weightPbcTypeEq) {
            this.weightPbcTypeEq = weightPbcTypeEq;
            return this;
        }

        /**
         * Sets the maximum number of operands in a pseudo boolean constraint.
         * @param maximumOperandsPbc the maximum number of operands
         * @return the builder
         */
        public Builder maximumOperandsPbc(final int maximumOperandsPbc) {
            this.maximumOperandsPbc = maximumOperandsPbc;
            return this;
        }

        /**
         * Sets the maximum absolute value of a coefficient in a pseudo boolean constraint.
         * Whether the coefficient is positive or negative is depends on {@link #weightPbcCoeffPositive}
         * and {@link #weightPbcCoeffNegative}.
         * @param maximumCoefficientPbc the maximum absolute value of a coefficient
         * @return the builder
         */
        public Builder maximumCoefficientPbc(final int maximumCoefficientPbc) {
            this.maximumCoefficientPbc = maximumCoefficientPbc;
            return this;
        }

        /**
         * Sets the relative weight of a cardinality constraint.
         * Note that the cardinality constraint may by chance also be an AMO or EXO constraint,
         * or even a literal or a constant.
         * @param weightCc the relative weight of a cardinality constraint
         * @return the builder
         */
        public Builder weightCc(final double weightCc) {
            this.weightCc = weightCc;
            return this;
        }

        /**
         * Sets the relative weight of an at-most-one constraint.
         * @param weightAmo the relative weight of an at-most-one constraint
         * @return the builder
         */
        public Builder weightAmo(final double weightAmo) {
            this.weightAmo = weightAmo;
            return this;
        }

        /**
         * Sets the relative weight of an exactly-one constraint.
         * @param weightExo the relative weight of an exactly-one constraint
         * @return the builder
         */
        public Builder weightExo(final double weightExo) {
            this.weightExo = weightExo;
            return this;
        }

        /**
         * Sets the maximum number of operands in a cardinality, AMO, or EXO constraint.
         * @param maximumOperandsCc the maximum number of operands in a cardinality, AMO, or EXO constraint
         * @return the builder
         */
        public Builder maximumOperandsCc(final int maximumOperandsCc) {
            this.maximumOperandsCc = maximumOperandsCc;
            return this;
        }
    }
}
