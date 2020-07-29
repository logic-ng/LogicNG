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

import org.logicng.formulas.CType;
import org.logicng.formulas.Constant;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * A randomizer for formulas.
 * <p>
 * The formula types included in the generated formulas can be configured
 * with a {@link FormulaRandomizerConfig}.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FormulaRandomizer {

    private final FormulaFactory f;
    private final FormulaRandomizerConfig config;
    private final Random random;
    private final Variable[] variables;

    private final FormulaTypeProbabilities formulaTypeProbabilities;
    private final CTypeProbabilities cTypeProbabilities;
    private final double phaseProbability;
    private final double coefficientNegativeProbability;

    /**
     * Generates a new formula randomizer. With the given formula factory and configuration.
     * @param f      the formula factory
     * @param config the formula randomizer configuration
     */
    public FormulaRandomizer(final FormulaFactory f, final FormulaRandomizerConfig config) {
        this.f = f;
        this.config = config;
        this.random = config.seed != 0 ? new Random(config.seed) : new Random();
        this.variables = generateVars(f, config);
        this.formulaTypeProbabilities = new FormulaTypeProbabilities(config);
        this.cTypeProbabilities = new CTypeProbabilities(config);
        this.phaseProbability = generatePhaseProbability(config);
        this.coefficientNegativeProbability = config.weightPbcCoeffNegative / (config.weightPbcCoeffPositive + config.weightPbcCoeffNegative);
    }

    /**
     * Returns a random constant.
     * @return the random constant
     */
    public Constant constant() {
        return this.f.constant(this.random.nextBoolean());
    }

    /**
     * Returns a random variable.
     * @return the random variable
     */
    public Variable variable() {
        return this.variables[this.random.nextInt(this.variables.length)];
    }

    /**
     * Returns a random literal. The probability of whether it is positive or negative
     * depends on the configuration.
     * @return the random literal
     */
    public Literal literal() {
        return this.f.literal(this.variables[this.random.nextInt(this.variables.length)].name(), this.random.nextDouble() < this.phaseProbability);
    }

    /**
     * Returns a random atom. This includes constants, literals, pseudo boolean constraints, and cardinality constraints (including amo and exo).
     * @return the random atom
     */
    public Formula atom() {
        final double n = this.random.nextDouble() * this.formulaTypeProbabilities.exo;
        if (n < this.formulaTypeProbabilities.constant) {
            return constant();
        } else if (n < this.formulaTypeProbabilities.literal) {
            return literal();
        } else if (n < this.formulaTypeProbabilities.pbc) {
            return pbc();
        } else if (n < this.formulaTypeProbabilities.cc) {
            return cc();
        } else if (n < this.formulaTypeProbabilities.amo) {
            return amo();
        } else {
            return exo();
        }
    }

    /**
     * Returns a random negation with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random negation
     */
    public Formula not(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        }
        final Formula not = this.f.not(formula(maxDepth - 1));
        if (maxDepth >= 2 && not.type() != FType.NOT) {
            return not(maxDepth);
        }
        return not;
    }

    /**
     * Returns a random implication with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random implication
     */
    public Formula impl(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        }
        final Formula implication = this.f.implication(formula(maxDepth - 1), formula(maxDepth - 1));
        if (implication.type() != FType.IMPL) {
            return impl(maxDepth);
        }
        return implication;
    }

    /**
     * Returns a random equivalence with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random equivalence
     */
    public Formula equiv(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        }
        final Formula equiv = this.f.equivalence(formula(maxDepth - 1), formula(maxDepth - 1));
        if (equiv.type() != FType.EQUIV) {
            return equiv(maxDepth);
        }
        return equiv;
    }

    /**
     * Returns a random conjunction with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random conjunction
     */
    public Formula and(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        }
        final Formula[] operands = new Formula[2 + this.random.nextInt(this.config.maximumOperandsAnd - 2)];
        for (int i = 0; i < operands.length; i++) {
            operands[i] = formula(maxDepth - 1);
        }
        final Formula formula = this.f.and(operands);
        if (formula.type() != FType.AND) {
            return and(maxDepth);
        }
        return formula;
    }

    /**
     * Returns a random disjunction with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random disjunction
     */
    public Formula or(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        }
        final Formula[] operands = new Formula[2 + this.random.nextInt(this.config.maximumOperandsOr - 2)];
        for (int i = 0; i < operands.length; i++) {
            operands[i] = formula(maxDepth - 1);
        }
        final Formula formula = this.f.or(operands);
        if (formula.type() != FType.OR) {
            return or(maxDepth);
        }
        return formula;
    }

    /**
     * Returns a random cardinality constraint.
     * @return the random cardinality constraint
     */
    public Formula cc() {
        final Variable[] variables = variables();
        final CType type = cType();
        int rhsBound = variables.length;
        if (type == CType.GT) {
            rhsBound = variables.length + 1;
        } else if (type == CType.LT) {
            rhsBound = variables.length + 1;
        }
        int rhsOffset = 0;
        if (type == CType.GT) {
            rhsOffset = -1;
        } else if (type == CType.LT) {
            rhsOffset = 1;
        }
        final int rhs = rhsOffset + this.random.nextInt(rhsBound);
        final Formula cc = this.f.cc(type, rhs, variables);
        if (cc.isConstantFormula()) {
            return cc();
        }
        return cc;
    }

    /**
     * Returns a random at-most-one constraint.
     * @return the random at-most-one constraint
     */
    public Formula amo() {
        return this.f.amo(variables());
    }

    /**
     * Returns a random exactly-one constraint.
     * @return the random exactly-one constraint
     */
    public Formula exo() {
        return this.f.exo(variables());
    }

    /**
     * Returns a random pseudo boolean constraint.
     * @return the random pseudo boolean constraint
     */
    public Formula pbc() {
        final int numOps = this.random.nextInt(this.config.maximumOperandsPbc);
        final Literal[] literals = new Literal[numOps];
        final int[] coefficients = new int[numOps];
        int minSum = 0; // (positive) sum of all negative coefficients
        int maxSum = 0; // sum of all positive coefficients
        for (int i = 0; i < numOps; i++) {
            literals[i] = literal();
            coefficients[i] = this.random.nextInt(this.config.maximumCoefficientPbc) + 1;
            if (this.random.nextDouble() < this.coefficientNegativeProbability) {
                minSum += coefficients[i];
                coefficients[i] = -coefficients[i];
            } else {
                maxSum += coefficients[i];
            }
        }
        final CType type = cType();
        final int rhs = this.random.nextInt(maxSum + minSum + 1) - minSum;
        final Formula pbc = this.f.pbc(type, rhs, literals, coefficients);
        if (pbc.isConstantFormula()) {
            return pbc();
        }
        return pbc;
    }

    /**
     * Returns a random formula with a given maximal depth.
     * @param maxDepth the maximal depth
     * @return the random formula
     */
    public Formula formula(final int maxDepth) {
        if (maxDepth == 0) {
            return atom();
        } else {
            final double n = this.random.nextDouble();
            if (n < this.formulaTypeProbabilities.constant) {
                return constant();
            } else if (n < this.formulaTypeProbabilities.literal) {
                return literal();
            } else if (n < this.formulaTypeProbabilities.pbc) {
                return pbc();
            } else if (n < this.formulaTypeProbabilities.cc) {
                return cc();
            } else if (n < this.formulaTypeProbabilities.amo) {
                return amo();
            } else if (n < this.formulaTypeProbabilities.exo) {
                return exo();
            } else if (n < this.formulaTypeProbabilities.or) {
                return or(maxDepth);
            } else if (n < this.formulaTypeProbabilities.and) {
                return and(maxDepth);
            } else if (n < this.formulaTypeProbabilities.not) {
                return not(maxDepth);
            } else if (n < this.formulaTypeProbabilities.impl) {
                return impl(maxDepth);
            } else {
                return equiv(maxDepth);
            }
        }
    }

    /**
     * Returns a list of {@code numConstraints} random formula with a given maximal depth.
     * @param numConstraints the number of constraints to be generated
     * @param maxDepth       the maximal depth
     * @return the random formula
     */
    public List<Formula> constraintSet(final int numConstraints, final int maxDepth) {
        return Stream.generate(() -> formula(maxDepth)).limit(numConstraints).collect(Collectors.toList());
    }

    private Variable[] variables() {
        final Variable[] variables = new Variable[this.random.nextInt(this.config.maximumOperandsCc - 1) + 2];
        for (int i = 0; i < variables.length; i++) {
            variables[i] = variable();
        }
        return variables;
    }

    private static Variable[] generateVars(final FormulaFactory f, final FormulaRandomizerConfig config) {
        if (config.variables != null) {
            return config.variables.toArray(new Variable[0]);
        } else {
            final Variable[] variables = new Variable[config.numVars];
            final int decimalPlaces = (int) Math.ceil(Math.log10(config.numVars));
            for (int i = 0; i < variables.length; i++) {
                variables[i] = f.variable("v" + String.format("%0" + decimalPlaces + "d", i));
            }
            return variables;
        }
    }

    private double generatePhaseProbability(final FormulaRandomizerConfig config) {
        return config.weightPositiveLiteral / (config.weightPositiveLiteral + config.weightNegativeLiteral);
    }

    private CType cType() {
        final CType type;
        final double n = this.random.nextDouble();
        if (n < this.cTypeProbabilities.le) {
            type = CType.LE;
        } else if (n < this.cTypeProbabilities.lt) {
            type = CType.LT;
        } else if (n < this.cTypeProbabilities.ge) {
            type = CType.GE;
        } else if (n < this.cTypeProbabilities.gt) {
            type = CType.GT;
        } else {
            type = CType.EQ;
        }
        return type;
    }

    private static class FormulaTypeProbabilities {
        private final double constant;
        private final double literal;
        private final double pbc;
        private final double cc;
        private final double amo;
        private final double exo;
        private final double or;
        private final double and;
        private final double not;
        private final double impl;
        private final double equiv;

        private FormulaTypeProbabilities(final FormulaRandomizerConfig config) {
            final double total = config.weightConstant + config.weightPositiveLiteral + config.weightNegativeLiteral + config.weightOr +
                    config.weightAnd + config.weightNot + config.weightImpl + config.weightEquiv +
                    config.weightPbc + config.weightCc + config.weightAmo + config.weightExo;
            this.constant = config.weightConstant / total;
            this.literal = this.constant + (config.weightPositiveLiteral + config.weightNegativeLiteral) / total;
            this.pbc = this.literal + config.weightPbc / total;
            this.cc = this.pbc + config.weightCc / total;
            this.amo = this.cc + config.weightAmo / total;
            this.exo = this.amo + config.weightExo / total;
            this.or = this.exo + config.weightOr / total;
            this.and = this.or + config.weightAnd / total;
            this.not = this.and + config.weightNot / total;
            this.impl = this.not + config.weightImpl / total;
            this.equiv = this.impl + config.weightEquiv / total;
            assert Math.abs(this.equiv - 1) < 0.00000001;
        }
    }

    private static class CTypeProbabilities {
        private final double le;
        private final double lt;
        private final double ge;
        private final double gt;
        private final double eq;

        private CTypeProbabilities(final FormulaRandomizerConfig config) {
            final double total = config.weightPbcTypeLe + config.weightPbcTypeLt + config.weightPbcTypeGe + config.weightPbcTypeGt + config.weightPbcTypeEq;
            this.le = config.weightPbcTypeLe / total;
            this.lt = this.le + config.weightPbcTypeLt / total;
            this.ge = this.lt + config.weightPbcTypeGe / total;
            this.gt = this.ge + config.weightPbcTypeGt / total;
            this.eq = this.gt + config.weightPbcTypeEq / total;
            assert Math.abs(this.eq - 1) < 0.00000001;
        }
    }
}
