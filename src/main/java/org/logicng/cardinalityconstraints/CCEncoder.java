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

package org.logicng.cardinalityconstraints;

import static org.logicng.util.FormulaHelper.literalsAsVariables;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.util.Pair;

import java.util.Collections;
import java.util.List;

/**
 * An encoder for cardinality constraints.
 * <p>
 * An encoder is configured with a {@link CCConfig} configuration.  There are two possible ways:
 * <ol>
 * <li>Initialize the encoder with a given configuration in the constructor.  Then this configuration will be bound
 * to the encoder for its whole lifetime.</li>
 * <li>Initialize the encoder only with a {@link FormulaFactory}.  Then each encoding will be performed with the
 * current cardinality constraint encoder configuration of the factory or the default configuration if the factory
 * has no associated cardinality constraint encoder configuration.  If you change the configuration in the factory,
 * all encoders constructed for this factory will be affected.</li>
 * </ol>
 * @version 2.0.0
 * @since 1.1
 */
public class CCEncoder {

    protected final FormulaFactory f;
    protected final CCConfig config;
    protected final CCConfig defaultConfig;

    protected CCAMOPure amoPure;
    protected CCAMOLadder amoLadder;
    protected CCAMOProduct amoProduct;
    protected CCAMONested amoNested;
    protected CCAMOCommander amoCommander;
    protected CCAMOBinary amoBinary;
    protected CCAMOBimander amoBimander;

    protected CCAMKCardinalityNetwork amkCardinalityNetwork;
    protected CCAMKModularTotalizer amkModularTotalizer;
    protected CCAMKTotalizer amkTotalizer;

    protected CCALKTotalizer alkTotalizer;
    protected CCALKModularTotalizer alkModularTotalizer;
    protected CCALKCardinalityNetwork alkCardinalityNetwork;

    protected CCEXKTotalizer exkTotalizer;
    protected CCEXKCardinalityNetwork exkCardinalityNetwork;

    /**
     * Constructs a new cardinality constraint encoder with a given configuration.
     * @param f      the formula factory
     * @param config the configuration
     */
    public CCEncoder(final FormulaFactory f, final CCConfig config) {
        this.f = f;
        this.config = config;
        this.defaultConfig = CCConfig.builder().build();
    }

    /**
     * Constructs a new cardinality constraint encoder which uses the configuration of the formula factory.
     * @param f the formula factory
     */
    public CCEncoder(final FormulaFactory f) {
        this(f, null);
    }

    /**
     * Encodes a cardinality constraint and returns its CNF encoding.
     * @param cc the cardinality constraint
     * @return the CNF encoding of the cardinality constraint
     */
    public List<Formula> encode(final CardinalityConstraint cc) {
        final EncodingResult result = EncodingResult.resultForFormula(this.f);
        this.encodeConstraint(cc, result);
        return Collections.unmodifiableList(result.result());
    }

    /**
     * Encodes a cardinality constraint in a given result.
     * @param cc     the cardinality constraint
     * @param result the result of the encoding
     */
    public void encode(final CardinalityConstraint cc, final EncodingResult result) {
        this.encodeConstraint(cc, result);
    }

    /**
     * Encodes an incremental cardinality constraint and returns its encoding.
     * @param cc the cardinality constraint
     * @return the encoding of the constraint and the incremental data
     */
    public Pair<List<Formula>, CCIncrementalData> encodeIncremental(final CardinalityConstraint cc) {
        final EncodingResult result = EncodingResult.resultForFormula(this.f);
        final CCIncrementalData incData = this.encodeIncremental(cc, result);
        return new Pair<>(Collections.unmodifiableList(result.result()), incData);
    }

    /**
     * Encodes an incremental cardinality constraint on a given solver.
     * @param cc     the cardinality constraint
     * @param result the result of the encoding
     * @return the incremental data
     */
    public CCIncrementalData encodeIncremental(final CardinalityConstraint cc, final EncodingResult result) {
        return this.encodeIncrementalConstraint(cc, result);
    }

    protected CCIncrementalData encodeIncrementalConstraint(final CardinalityConstraint cc, final EncodingResult result) {
        final Variable[] ops = literalsAsVariables(cc.operands());
        if (cc.isAmo()) {
            throw new IllegalArgumentException("Incremental encodings are not supported for at-most-one constraints");
        }
        switch (cc.comparator()) {
            case LE:
                return this.amkIncremental(result, ops, cc.rhs());
            case LT:
                return this.amkIncremental(result, ops, cc.rhs() - 1);
            case GE:
                return this.alkIncremental(result, ops, cc.rhs());
            case GT:
                return this.alkIncremental(result, ops, cc.rhs() + 1);
            default:
                throw new IllegalArgumentException("Incremental encodings are only supported for at-most-k and at-least k constraints.");
        }
    }

    /**
     * Returns the current configuration of this encoder.  If the encoder was constructed with a given configuration, this
     * configuration will always be used.  Otherwise the current configuration of the formula factory is used or - if not
     * present - the default configuration.
     * @return the current configuration of
     */
    public CCConfig config() {
        if (this.config != null) {
            return this.config;
        }
        final Configuration ccConfig = this.f.configurationFor(ConfigurationType.CC_ENCODER);
        return ccConfig != null ? (CCConfig) ccConfig : this.defaultConfig;
    }

    /**
     * Encodes the constraint in the given result.
     * @param cc     the constraint
     * @param result the result
     */
    protected void encodeConstraint(final CardinalityConstraint cc, final EncodingResult result) {
        final Variable[] ops = literalsAsVariables(cc.operands());
        switch (cc.comparator()) {
            case LE:
                if (cc.rhs() == 1) {
                    this.amo(result, ops);
                } else {
                    this.amk(result, ops, cc.rhs());
                }
                break;
            case LT:
                if (cc.rhs() == 2) {
                    this.amo(result, ops);
                } else {
                    this.amk(result, ops, cc.rhs() - 1);
                }
                break;
            case GE:
                this.alk(result, ops, cc.rhs());
                break;
            case GT:
                this.alk(result, ops, cc.rhs() + 1);
                break;
            case EQ:
                if (cc.rhs() == 1) {
                    this.exo(result, ops);
                } else {
                    this.exk(result, ops, cc.rhs());
                }
                break;
            default:
                throw new IllegalArgumentException("Unknown pseudo-Boolean comparator: " + cc.comparator());
        }
    }

    /**
     * Encodes an at-most-one constraint.
     * @param result the result
     * @param vars   the variables of the constraint
     */
    protected void amo(final EncodingResult result, final Variable... vars) {
        if (vars.length <= 1) {
            return;
        }
        switch (this.config().amoEncoder) {
            case PURE:
                if (this.amoPure == null) {
                    this.amoPure = new CCAMOPure();
                }
                this.amoPure.build(result, vars);
                break;
            case LADDER:
                if (this.amoLadder == null) {
                    this.amoLadder = new CCAMOLadder();
                }
                this.amoLadder.build(result, vars);
                break;
            case PRODUCT:
                if (this.amoProduct == null) {
                    this.amoProduct = new CCAMOProduct(this.config().productRecursiveBound);
                }
                this.amoProduct.build(result, vars);
                break;
            case NESTED:
                if (this.amoNested == null) {
                    this.amoNested = new CCAMONested(this.config().nestingGroupSize);
                }
                this.amoNested.build(result, vars);
                break;
            case COMMANDER:
                if (this.amoCommander == null) {
                    this.amoCommander = new CCAMOCommander(this.config().commanderGroupSize);
                }
                this.amoCommander.build(result, vars);
                break;
            case BINARY:
                if (this.amoBinary == null) {
                    this.amoBinary = new CCAMOBinary();
                }
                this.amoBinary.build(result, vars);
                break;
            case BIMANDER:
                if (this.config().bimanderGroupSize != CCConfig.BIMANDER_GROUP_SIZE.FIXED || this.amoBimander == null) {
                    final int groupSize;
                    switch (this.config().bimanderGroupSize) {
                        case FIXED:
                            groupSize = this.config().bimanderFixedGroupSize;
                            break;
                        case HALF:
                            groupSize = vars.length / 2;
                            break;
                        case SQRT:
                            groupSize = (int) Math.sqrt(vars.length);
                            break;
                        default:
                            throw new IllegalStateException("Unknown bimander group size: " + this.config().bimanderGroupSize);
                    }
                    this.amoBimander = new CCAMOBimander(groupSize);
                }
                this.amoBimander.build(result, vars);
                break;
            case BEST:
                this.bestAMO(vars.length).build(result, vars);
                break;
            default:
                throw new IllegalStateException("Unknown at-most-one encoder: " + this.config().amoEncoder);
        }
    }

    /**
     * Encodes an at-most-one constraint.
     * @param result the result
     * @param vars   the variables of the constraint
     */
    protected void exo(final EncodingResult result, final Variable... vars) {
        if (vars.length == 0) {
            result.addClause();
            return;
        }
        if (vars.length == 1) {
            result.addClause(vars[0]);
            return;
        }
        this.amo(result, vars);
        result.addClause(vars);
    }

    /**
     * Encodes an at-most-k constraint.
     * @param result the result
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     */
    protected void amk(final EncodingResult result, final Variable[] vars, final int rhs) {
        if (rhs < 0) {
            throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
        }
        if (rhs >= vars.length) // there is no constraint
        {
            return;
        }
        if (rhs == 0) { // no variable can be true
            for (final Variable var : vars) {
                result.addClause(var.negate());
            }
            return;
        }
        switch (this.config().amkEncoder) {
            case TOTALIZER:
                if (this.amkTotalizer == null) {
                    this.amkTotalizer = new CCAMKTotalizer();
                }
                this.amkTotalizer.build(result, vars, rhs);
                break;
            case MODULAR_TOTALIZER:
                if (this.amkModularTotalizer == null) {
                    this.amkModularTotalizer = new CCAMKModularTotalizer(this.f);
                }
                this.amkModularTotalizer.build(result, vars, rhs);
                break;
            case CARDINALITY_NETWORK:
                if (this.amkCardinalityNetwork == null) {
                    this.amkCardinalityNetwork = new CCAMKCardinalityNetwork();
                }
                this.amkCardinalityNetwork.build(result, vars, rhs);
                break;
            case BEST:
                this.bestAMK(vars.length).build(result, vars, rhs);
                break;
            default:
                throw new IllegalStateException("Unknown at-most-k encoder: " + this.config().amkEncoder);
        }
    }

    /**
     * Encodes an at-most-k constraint for incremental usage.
     * @param result the result
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     * @return the incremental data
     */
    protected CCIncrementalData amkIncremental(final EncodingResult result, final Variable[] vars, final int rhs) {
        if (rhs < 0) {
            throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
        }
        if (rhs >= vars.length) // there is no constraint
        {
            return null;
        }
        if (rhs == 0) { // no variable can be true
            for (final Variable var : vars) {
                result.addClause(var.negate());
            }
            return null;
        }
        switch (this.config().amkEncoder) {
            case TOTALIZER:
                if (this.amkTotalizer == null) {
                    this.amkTotalizer = new CCAMKTotalizer();
                }
                this.amkTotalizer.build(result, vars, rhs);
                return this.amkTotalizer.incrementalData();
            case MODULAR_TOTALIZER:
                if (this.amkModularTotalizer == null) {
                    this.amkModularTotalizer = new CCAMKModularTotalizer(this.f);
                }
                this.amkModularTotalizer.build(result, vars, rhs);
                return this.amkModularTotalizer.incrementalData();
            case CARDINALITY_NETWORK:
                if (this.amkCardinalityNetwork == null) {
                    this.amkCardinalityNetwork = new CCAMKCardinalityNetwork();
                }
                this.amkCardinalityNetwork.buildForIncremental(result, vars, rhs);
                return this.amkCardinalityNetwork.incrementalData();
            case BEST:
                this.bestAMK(vars.length).build(result, vars, rhs);
                return this.bestAMK(vars.length).incrementalData();
            default:
                throw new IllegalStateException("Unknown at-most-k encoder: " + this.config().amkEncoder);
        }
    }

    /**
     * Encodes an at-lest-k constraint.
     * @param result the result
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     */
    protected void alk(final EncodingResult result, final Variable[] vars, final int rhs) {
        if (rhs < 0) {
            throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
        }
        if (rhs > vars.length) {
            result.addClause();
            return;
        }
        if (rhs == 0) {
            return;
        }
        if (rhs == 1) {
            result.addClause(vars);
            return;
        }
        if (rhs == vars.length) {
            for (final Variable var : vars) {
                result.addClause(var);
            }
            return;
        }
        switch (this.config().alkEncoder) {
            case TOTALIZER:
                if (this.alkTotalizer == null) {
                    this.alkTotalizer = new CCALKTotalizer();
                }
                this.alkTotalizer.build(result, vars, rhs);
                break;
            case MODULAR_TOTALIZER:
                if (this.alkModularTotalizer == null) {
                    this.alkModularTotalizer = new CCALKModularTotalizer(this.f);
                }
                this.alkModularTotalizer.build(result, vars, rhs);
                break;
            case CARDINALITY_NETWORK:
                if (this.alkCardinalityNetwork == null) {
                    this.alkCardinalityNetwork = new CCALKCardinalityNetwork();
                }
                this.alkCardinalityNetwork.build(result, vars, rhs);
                break;
            case BEST:
                this.bestALK(vars.length).build(result, vars, rhs);
                break;
            default:
                throw new IllegalStateException("Unknown at-least-k encoder: " + this.config().alkEncoder);
        }
    }

    /**
     * Encodes an at-lest-k constraint for incremental usage.
     * @param result the result
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     * @return the incremental data
     */
    protected CCIncrementalData alkIncremental(final EncodingResult result, final Variable[] vars, final int rhs) {
        if (rhs < 0) {
            throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
        }
        if (rhs > vars.length) {
            result.addClause();
            return null;
        }
        if (rhs == 0) {
            return null;
        }
        if (rhs == 1) {
            result.addClause(vars);
            return null;
        }
        if (rhs == vars.length) {
            for (final Variable var : vars) {
                result.addClause(var);
            }
            return null;
        }
        switch (this.config().alkEncoder) {
            case TOTALIZER:
                if (this.alkTotalizer == null) {
                    this.alkTotalizer = new CCALKTotalizer();
                }
                this.alkTotalizer.build(result, vars, rhs);
                return this.alkTotalizer.incrementalData();
            case MODULAR_TOTALIZER:
                if (this.alkModularTotalizer == null) {
                    this.alkModularTotalizer = new CCALKModularTotalizer(this.f);
                }
                this.alkModularTotalizer.build(result, vars, rhs);
                return this.alkModularTotalizer.incrementalData();
            case CARDINALITY_NETWORK:
                if (this.alkCardinalityNetwork == null) {
                    this.alkCardinalityNetwork = new CCALKCardinalityNetwork();
                }
                this.alkCardinalityNetwork.buildForIncremental(result, vars, rhs);
                return this.alkCardinalityNetwork.incrementalData();
            case BEST:
                this.bestALK(vars.length).build(result, vars, rhs);
                return this.bestALK(vars.length).incrementalData();
            default:
                throw new IllegalStateException("Unknown at-least-k encoder: " + this.config().alkEncoder);
        }
    }

    /**
     * Encodes an exactly-k constraint.
     * @param result the result
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     */
    protected void exk(final EncodingResult result, final Variable[] vars, final int rhs) {
        if (rhs < 0) {
            throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
        }
        if (rhs > vars.length) {
            result.addClause();
            return;
        }
        if (rhs == 0) {
            for (final Variable var : vars) {
                result.addClause(var.negate());
            }
            return;
        }
        if (rhs == vars.length) {
            for (final Variable var : vars) {
                result.addClause(var);
            }
            return;
        }
        switch (this.config().exkEncoder) {
            case TOTALIZER:
                if (this.exkTotalizer == null) {
                    this.exkTotalizer = new CCEXKTotalizer();
                }
                this.exkTotalizer.build(result, vars, rhs);
                break;
            case CARDINALITY_NETWORK:
                if (this.exkCardinalityNetwork == null) {
                    this.exkCardinalityNetwork = new CCEXKCardinalityNetwork();
                }
                this.exkCardinalityNetwork.build(result, vars, rhs);
                break;
            case BEST:
                this.bestEXK(vars.length).build(result, vars, rhs);
                break;
            default:
                throw new IllegalStateException("Unknown exactly-k encoder: " + this.config().exkEncoder);
        }
    }

    /**
     * Returns the best at-most-one encoder for a given number of variables.  The valuation is based on theoretical and
     * practical observations.  For &lt;= 10 the pure encoding without introduction of new variables is used, otherwise
     * the product encoding is chosen.
     * @param n the number of variables
     * @return the best at-most-one encoder
     */
    protected CCAtMostOne bestAMO(final int n) {
        if (n <= 10) {
            if (this.amoPure == null) {
                this.amoPure = new CCAMOPure();
            }
            return this.amoPure;
        } else {
            if (this.amoProduct == null) {
                this.amoProduct = new CCAMOProduct(this.config().productRecursiveBound);
            }
            return this.amoProduct;
        }
    }

    /**
     * Returns the best at-most-k encoder for a given number of variables.  The valuation is based on theoretical and
     * practical observations.  Currently the modular totalizer is the best encoder for all sizes and therefore is always
     * chosen.
     * @param n the number of variables
     * @return the best at-most-one encoder
     */
    protected CCAtMostK bestAMK(final int n) {
        if (this.amkModularTotalizer == null) {
            this.amkModularTotalizer = new CCAMKModularTotalizer(this.f);
        }
        return this.amkModularTotalizer;
    }

    /**
     * Returns the best at-least-k encoder for a given number of variables.  The valuation is based on theoretical and
     * practical observations.  Currently the modular totalizer is the best encoder for all sizes and therefore is always
     * chosen.
     * @param n the number of variables
     * @return the best at-most-one encoder
     */
    protected CCAtLeastK bestALK(final int n) {
        if (this.alkModularTotalizer == null) {
            this.alkModularTotalizer = new CCALKModularTotalizer(this.f);
        }
        return this.alkModularTotalizer;
    }

    /**
     * Returns the best exactly-k encoder for a given number of variables.  The valuation is based on theoretical and
     * practical observations.  Currently the totalizer is the best encoder for all sizes and therefore is always
     * chosen.
     * @param n the number of variables
     * @return the best at-most-one encoder
     */
    protected CCExactlyK bestEXK(final int n) {
        if (this.exkTotalizer == null) {
            this.exkTotalizer = new CCEXKTotalizer();
        }
        return this.exkTotalizer;
    }

    @Override
    public String toString() {
        return this.config().toString();
    }
}
