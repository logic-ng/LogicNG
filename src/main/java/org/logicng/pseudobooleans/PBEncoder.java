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

package org.logicng.pseudobooleans;

import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.cardinalityconstraints.CCEncoder;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * An encoder for pseudo-Boolean constraints.
 * @version 2.0.0
 * @since 1.0
 */
public class PBEncoder {

    protected final FormulaFactory f;
    protected final PBConfig config;
    protected final PBConfig defaultConfig;
    protected final CCEncoder ccEncoder;

    protected PBSWC swc;
    protected PBAdderNetworks adderNetworks;

    /**
     * Constructs a new pseudo-Boolean encoder with given configurations.
     * @param f        the formula factory
     * @param pbConfig the pseudo-Boolean encoder configuration
     * @param ccConfig the cardinality constraints encoder configuration
     */
    public PBEncoder(final FormulaFactory f, final PBConfig pbConfig, final CCConfig ccConfig) {
        this.f = f;
        this.defaultConfig = PBConfig.builder().build();
        this.config = pbConfig;
        this.ccEncoder = new CCEncoder(f, ccConfig);
    }

    /**
     * Constructs a new pseudo-Boolean encoder with a given configuration.
     * @param f        the formula factory
     * @param pbConfig the pseudo-Boolean encoder configuration
     */
    public PBEncoder(final FormulaFactory f, final PBConfig pbConfig) {
        this.f = f;
        this.defaultConfig = PBConfig.builder().build();
        this.config = pbConfig;
        this.ccEncoder = new CCEncoder(f);
    }

    /**
     * Constructs a new pseudo-Boolean encoder which uses the configuration of the formula factory.
     * @param f the formula factory
     */
    public PBEncoder(final FormulaFactory f) {
        this.f = f;
        this.defaultConfig = PBConfig.builder().build();
        this.config = null;
        this.ccEncoder = new CCEncoder(f);
    }

    /**
     * Encodes a pseudo-Boolean constraint and returns its CNF encoding.
     * @param constraint the pseudo-Boolean constraint
     * @return the CNF encoding of the pseudo-Boolean constraint
     */
    public List<Formula> encode(final PBConstraint constraint) {
        if (constraint.isCC()) {
            return this.ccEncoder.encode((CardinalityConstraint) constraint);
        }
        final Formula normalized = constraint.normalize();
        switch (normalized.type()) {
            case TRUE:
                return Collections.emptyList();
            case FALSE:
                return Collections.singletonList(this.f.falsum());
            case PBC:
                final PBConstraint pbc = (PBConstraint) normalized;
                if (pbc.isCC()) {
                    return this.ccEncoder.encode((CardinalityConstraint) pbc);
                }
                return this.encode(pbc.operands(), pbc.coefficients(), pbc.rhs());
            case AND:
                final List<Formula> list = new ArrayList<>();
                for (final Formula op : normalized) {
                    switch (op.type()) {
                        case FALSE:
                            return Collections.singletonList(this.f.falsum());
                        case PBC:
                            list.addAll(this.encode((PBConstraint) op));
                            break;
                        default:
                            throw new IllegalArgumentException("Illegal return value of PBConstraint.normalize");
                    }
                }
                return Collections.unmodifiableList(list);
            default:
                throw new IllegalArgumentException("Illegal return value of PBConstraint.normalize");
        }
    }

    /**
     * Returns the current configuration of this encoder.  If the encoder was constructed with a given configuration, this
     * configuration will always be used.  Otherwise the current configuration of the formula factory is used or - if not
     * present - the default configuration.
     * @return the current configuration of
     */
    public PBConfig config() {
        if (this.config != null) {
            return this.config;
        }
        final Configuration pbConfig = this.f.configurationFor(ConfigurationType.PB_ENCODER);
        return pbConfig != null ? (PBConfig) pbConfig : this.defaultConfig;
    }

    /**
     * Builds a pseudo Boolean constraint of the form {@code c_1 * lit_1 + c_2 * lit_2 + ... + c_n * lit_n >= k}.
     * @param lits   the literals {@code lit_1 ... lit_n}
     * @param coeffs the coefficients {@code c_1 ... c_n}
     * @param rhs    the right hand side {@code k} of the constraint
     * @return the CNF encoding of the pseudo Boolean constraint
     * @throws IllegalArgumentException if the right hand side of the cardinality constraint is negative or
     *                                  larger than the number of literals
     */
    protected List<Formula> encode(final Literal[] lits, final int[] coeffs, final int rhs) {
        if (rhs == Integer.MAX_VALUE) {
            throw new IllegalArgumentException("Overflow in the Encoding");
        }
        if (rhs < 0) {
            return Collections.singletonList(this.f.falsum());
        }
        final LNGVector<Literal> simplifiedLits = new LNGVector<>();
        final LNGIntVector simplifiedCoeffs = new LNGIntVector();
        final List<Formula> result = new ArrayList<>();
        if (rhs == 0) {
            for (final Literal lit : lits) {
                result.add(lit.negate());
            }
            return result;
        }
        for (int i = 0; i < lits.length; i++) {
            if (coeffs[i] <= rhs) {
                simplifiedLits.push(lits[i]);
                simplifiedCoeffs.push(coeffs[i]);
            } else {
                result.add(lits[i].negate());
            }
        }
        if (simplifiedLits.size() <= 1) {
            return result;
        }
        switch (this.config().pbEncoder) {
            case SWC:
            case BEST:
                if (this.swc == null) {
                    this.swc = new PBSWC(this.f);
                }
                return this.swc.encode(simplifiedLits, simplifiedCoeffs, rhs, result);
            case BINARY_MERGE:
                return new PBBinaryMerge(this.f, this.config()).encode(simplifiedLits, simplifiedCoeffs, rhs, result);
            case ADDER_NETWORKS:
                if (this.adderNetworks == null) {
                    this.adderNetworks = new PBAdderNetworks(this.f);
                }
                return this.adderNetworks.encode(simplifiedLits, simplifiedCoeffs, rhs, result);
            default:
                throw new IllegalStateException("Unknown pseudo-Boolean encoder: " + this.config().pbEncoder);
        }
    }
}
