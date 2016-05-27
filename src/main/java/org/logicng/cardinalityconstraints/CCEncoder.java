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
//  Copyright 2015-2016 Christoph Zengler                                //
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

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * An encoder for cardinality constraints.
 * @version 1.1
 * @since 1.1
 */
public class CCEncoder {

  private final FormulaFactory f;
  private final CCConfig config;
  private final CCConfig defaultConfig;

  private CCAMOPure amoPure;
  private CCAMOLadder amoLadder;
  private CCAMOProduct amoProduct;
  private CCAMONested amoNested;
  private CCAMOCommander amoCommander;
  private CCAMOBinary amoBinary;
  private CCAMOBimander amoBimander;

  private CCAMKCardinalityNetwork amkCardinalityNetwork;
  private CCAMKModularTotalizer amkModularTotalizer;
  private CCAMKTotalizer amkTotalizer;
  private CCALKTotalizer alkTotalizer;

  /**
   * Constructs a new cardinality constraint encoder with a given configuration.
   * @param f      the formula factory
   * @param config the configuration
   */
  public CCEncoder(final FormulaFactory f, final CCConfig config) {
    this.f = f;
    this.config = config;
    this.defaultConfig = new CCConfig.Builder().build();
  }

  /**
   * Constructs a new cardinality constraint encoder with the default configuration.
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
  public ImmutableFormulaList encode(final PBConstraint cc) {
    return new ImmutableFormulaList(FType.AND, this.encodeConstraint(cc));
  }

  /**
   * Returns the current configuration of this encoder.  If the encoder was constructed with a given configuration, this
   * configuration will always be used.  Otherwise the current configuration of the formula factory is used or - if not
   * present - the default configuration.
   * @return the current configuration of
   */
  public CCConfig config() {
    if (this.config != null)
      return this.config;
    Configuration ccConfig = this.f.configurationFor(ConfigurationType.CC_ENCODER);
    return ccConfig != null ? (CCConfig) ccConfig : this.defaultConfig;
  }

  /**
   * Encodes the constraint.
   * @param cc the constraint
   * @return the encoding
   */
  private List<Formula> encodeConstraint(final PBConstraint cc) {
    if (!cc.isCC())
      throw new IllegalArgumentException("Cannot encode a non-cardinality constraint with a cardinality constraint encoder.");
    final Variable[] ops = litsAsVars(cc.operands());
    switch (cc.comparator()) {
      case LE:
        if (cc.rhs() == 1)
          return this.amo(ops);
        else
          return this.amk(ops, cc.rhs());
      case LT:
        if (cc.rhs() == 2)
          return this.amo(ops);
        else
          return this.amk(ops, cc.rhs() - 1);
      case GE:
        return this.alk(ops, cc.rhs());
      case GT:
        return this.alk(ops, cc.rhs() + 1);
      case EQ:
        if (cc.rhs() == 1)
          return this.exo(ops);
        else {
          final PBConstraint le = this.f.cc(CType.LE, cc.rhs(), ops);
          final PBConstraint ge = this.f.cc(CType.GE, cc.rhs(), ops);
          if (le.type() == FType.FALSE || ge.type() == FType.FALSE)
            return Collections.singletonList((Formula) this.f.falsum());
          final List<Formula> list = new LinkedList<>();
          if (le.type() != FType.TRUE)
            list.addAll(this.encode(le).toList());
          if (ge.type() != FType.TRUE)
            list.addAll(this.encode(ge).toList());
          return list;
        }
      default:
        throw new IllegalArgumentException("Unknown pseudo-Boolean comparator: " + cc.comparator());
    }
  }

  /**
   * Encodes an at-most-one constraint.
   * @param vars the variables of the constraint
   * @return the CNF encoding of the constraint
   */
  private List<Formula> amo(final Variable... vars) {
    if (vars.length <= 1)
      return new ArrayList<>();
    switch (this.config().amoEncoder) {
      case PURE:
        if (this.amoPure == null)
          this.amoPure = new CCAMOPure(this.f);
        return this.amoPure.build(vars);
      case LADDER:
        if (this.amoLadder == null)
          this.amoLadder = new CCAMOLadder(this.f);
        return this.amoLadder.build(vars);
      case PRODUCT:
        if (this.amoProduct == null)
          this.amoProduct = new CCAMOProduct(this.f, this.config().productRecursiveBound);
        return this.amoProduct.build(vars);
      case NESTED:
        if (this.amoNested == null)
          this.amoNested = new CCAMONested(this.f, this.config().nestingGroupSize);
        return this.amoNested.build(vars);
      case COMMANDER:
        if (this.amoCommander == null)
          this.amoCommander = new CCAMOCommander(this.f, this.config().commanderGroupSize);
        return this.amoCommander.build(vars);
      case BINARY:
        if (this.amoBinary == null)
          this.amoBinary = new CCAMOBinary(this.f);
        return this.amoBinary.build(vars);
      case BIMANDER:
        if (this.config().bimanderGroupSize != CCConfig.BIMANDER_GROUP_SIZE.FIXED || this.amoBimander == null) {
          int groupSize;
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
              throw new IllegalStateException("Unkown bimander group size: " + this.config().bimanderGroupSize);
          }
          this.amoBimander = new CCAMOBimander(this.f, groupSize);
        }
        return this.amoBimander.build(vars);
      case BEST:
        return this.bestAMO(vars.length).build(vars);
      default:
        throw new IllegalStateException("Unknown at-most-one encoder: " + this.config().amoEncoder);
    }
  }

  /**
   * Encodes an at-most-one constraint.
   * @param vars the variables of the constraint
   * @return the CNF encoding of the constraint
   */
  private List<Formula> exo(final Variable... vars) {
    if (vars.length == 0)
      return new ArrayList<>();
    if (vars.length == 1)
      return Collections.singletonList((Formula) vars[0]);
    final List<Formula> result = this.amo(vars);
    result.add(this.f.clause((Literal[]) vars));
    return result;
  }

  /**
   * Encodes an at-most-k constraint.
   * @param vars the variables of the constraint
   * @param rhs  the right hand side of the constraint
   * @return the CNF encoding of the constraint
   */
  private List<Formula> amk(final Variable[] vars, int rhs) {
    if (rhs < 0)
      throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
    if (rhs >= vars.length) // there is no constraint
      return new ArrayList<>();
    if (rhs == 0) { // no variable can be true
      final List<Formula> result = new ArrayList<>();
      for (final Variable var : vars)
        result.add(var.negate());
      return result;
    }
    switch (this.config().amkEncoder) {
      case TOTALIZER:
        if (this.amkTotalizer == null)
          this.amkTotalizer = new CCAMKTotalizer(this.f);
        return this.amkTotalizer.build(vars, rhs);
      case MODULAR_TOTALIZER:
        if (this.amkModularTotalizer == null)
          this.amkModularTotalizer = new CCAMKModularTotalizer(this.f);
        return this.amkModularTotalizer.build(vars, rhs);
      case CARDINALITY_NETWORK:
        if (this.amkCardinalityNetwork == null)
          this.amkCardinalityNetwork = new CCAMKCardinalityNetwork(this.f);
        return this.amkCardinalityNetwork.build(vars, rhs);
      case BEST:
        return this.bestAMK(vars.length).build(vars, rhs);
      default:
        throw new IllegalStateException("Unknown at-most-k encoder: " + this.config().amkEncoder);
    }
  }

  /**
   * Encodes an at-lest-k constraint.
   * @param vars the variables of the constraint
   * @param rhs  the right hand side of the constraint
   * @return the CNF encoding of the constraint
   */
  private List<Formula> alk(final Variable[] vars, int rhs) {
    if (rhs < 0)
      throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
    if (rhs > vars.length)
      return Collections.singletonList((Formula) this.f.falsum());
    if (rhs == 0)
      return new ArrayList<>();
    if (rhs == 1)
      return Collections.singletonList(this.f.clause((Literal[]) vars));
    if (rhs == vars.length) {
      final List<Formula> result = new ArrayList<>();
      Collections.addAll(result, vars);
      return result;
    }
    switch (this.config().alkEncoder) {
      case TOTALIZER:
      case BEST:
        if (this.alkTotalizer == null)
          this.alkTotalizer = new CCALKTotalizer(this.f);
        return this.alkTotalizer.build(vars, rhs);
      default:
        throw new IllegalStateException("Unknown at-least-k encoder: " + this.config().alkEncoder);
    }
  }

  /**
   * Returns the best at-most-one encoder for a given number of variables.  The valuation is based on theoretical and
   * practical observations.  For <= 10 the pure encoding without introduction of new variables is used, otherwise
   * the product encoding is chosen.
   * @param n the number of variables
   * @return the best at-most-one encoder
   */
  private CCAtMostOne bestAMO(int n) {
    if (n <= 10) {
      if (this.amoPure == null)
        this.amoPure = new CCAMOPure(this.f);
      return this.amoPure;
    } else {
      if (this.amoProduct == null)
        this.amoProduct = new CCAMOProduct(this.f, this.config().productRecursiveBound);
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
  private CCAtMostK bestAMK(int n) {
    if (this.amkModularTotalizer == null)
      this.amkModularTotalizer = new CCAMKModularTotalizer(this.f);
    return this.amkModularTotalizer;
  }

  /**
   * Converts a literal array to a variable array
   * <p>
   * ATTENTION: this only works if because the {@code isCC} method checks, that there are only positive literals.
   * @param lits the literals
   * @return the variables
   */
  private static Variable[] litsAsVars(final Literal[] lits) {
    final Variable[] vars = new Variable[lits.length];
    for (int i = 0; i < vars.length; i++)
      vars[i] = lits[i].variable();
    return vars;
  }

  @Override
  public String toString() {
    return this.config().toString();
  }
}
