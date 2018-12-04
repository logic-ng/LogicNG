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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration for a cardinality constraint encoder.
 * @version 1.1
 * @since 1.1
 */
public final class CCConfig extends Configuration {

  /**
   * The encoder for at-most-one and exactly-one constraints.
   */
  public enum AMO_ENCODER {
    PURE, LADDER, PRODUCT, NESTED, COMMANDER, BINARY, BIMANDER, BEST
  }

  /**
   * The encoder for at-most-k constraints.
   */
  public enum AMK_ENCODER {
    TOTALIZER, MODULAR_TOTALIZER, CARDINALITY_NETWORK, BEST
  }

  /**
   * The encoder for at-least-k constraints.
   */
  public enum ALK_ENCODER {
    TOTALIZER, MODULAR_TOTALIZER, CARDINALITY_NETWORK, BEST
  }

  /**
   * The encoder for exactly-k constraints.
   */
  public enum EXK_ENCODER {
    TOTALIZER, CARDINALITY_NETWORK, BEST
  }

  /**
   * The group size for the Bimander encoding.
   */
  public enum BIMANDER_GROUP_SIZE {
    HALF, SQRT, FIXED
  }

  final AMO_ENCODER amoEncoder;
  final AMK_ENCODER amkEncoder;
  final ALK_ENCODER alkEncoder;
  final EXK_ENCODER exkEncoder;
  final BIMANDER_GROUP_SIZE bimanderGroupSize;
  final int bimanderFixedGroupSize;
  final int nestingGroupSize;
  final int productRecursiveBound;
  final int commanderGroupSize;

  /**
   * Constructs a new cardinality constraint encoder configuration from a given builder.
   * @param builder the builder
   */
  private CCConfig(final Builder builder) {
    super(ConfigurationType.CC_ENCODER);
    this.amoEncoder = builder.amoEncoder;
    this.amkEncoder = builder.amkEncoder;
    this.alkEncoder = builder.alkEncoder;
    this.exkEncoder = builder.exkEncoder;
    this.bimanderGroupSize = builder.bimanderGroupSize;
    this.bimanderFixedGroupSize = builder.bimanderFixedGroupSize;
    this.nestingGroupSize = builder.nestingGroupSize;
    this.productRecursiveBound = builder.productRecursiveBound;
    this.commanderGroupSize = builder.commanderGroupSize;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CCConfig{").append(System.lineSeparator());
    sb.append("amoEncoder=").append(this.amoEncoder).append(System.lineSeparator());
    sb.append("amkEncoder=").append(this.amkEncoder).append(System.lineSeparator());
    sb.append("alkEncoder=").append(this.alkEncoder).append(System.lineSeparator());
    sb.append("exkEncoder=").append(this.exkEncoder).append(System.lineSeparator());
    sb.append("bimanderGroupSize=").append(this.bimanderGroupSize).append(System.lineSeparator());
    sb.append("bimanderFixedGroupSize=").append(this.bimanderFixedGroupSize).append(System.lineSeparator());
    sb.append("nestingGroupSize=").append(this.nestingGroupSize).append(System.lineSeparator());
    sb.append("productRecursiveBound=").append(this.productRecursiveBound).append(System.lineSeparator());
    sb.append("commanderGroupSize=").append(this.commanderGroupSize).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a cardinality constraint encoder configuration.
   */
  public static class Builder {
    private AMO_ENCODER amoEncoder = AMO_ENCODER.BEST;
    private AMK_ENCODER amkEncoder = AMK_ENCODER.BEST;
    private ALK_ENCODER alkEncoder = ALK_ENCODER.BEST;
    private EXK_ENCODER exkEncoder = EXK_ENCODER.BEST;
    private BIMANDER_GROUP_SIZE bimanderGroupSize = BIMANDER_GROUP_SIZE.SQRT;
    private int bimanderFixedGroupSize = 3;
    private int nestingGroupSize = 4;
    private int productRecursiveBound = 20;
    private int commanderGroupSize = 3;

    /**
     * Sets the encoder for at-most-one constraints. The default value is {@code BEST}.
     * @param amoEncoder the at-most-one encoder
     * @return the builder
     */
    public Builder amoEncoding(final AMO_ENCODER amoEncoder) {
      this.amoEncoder = amoEncoder;
      return this;
    }

    /**
     * Sets the encoder for at-most-k constraints. The default value is {@code BEST}.
     * @param amkEncoder the at-most-k encoder
     * @return the builder
     */
    public Builder amkEncoding(final AMK_ENCODER amkEncoder) {
      this.amkEncoder = amkEncoder;
      return this;
    }

    /**
     * Sets the encoder for at-least-k constraints. The default value is {@code BEST}.
     * @param alkEncoder the at-least-k encoder
     * @return the builder
     */
    public Builder alkEncoding(final ALK_ENCODER alkEncoder) {
      this.alkEncoder = alkEncoder;
      return this;
    }

    /**
     * Sets the encoder for exactly-k constraints. The default value is {@code BEST}.
     * @param exkEncoder the exactly-k encoder
     * @return the builder
     */
    public Builder exkEncoding(final EXK_ENCODER exkEncoder) {
      this.exkEncoder = exkEncoder;
      return this;
    }

    /**
     * Sets the group size for the bimander encoding. The default value is {@code SQRT}.
     * @param bimanderGroupSize the bimander encoding group size
     * @return the builder
     */
    public Builder bimanderGroupSize(final BIMANDER_GROUP_SIZE bimanderGroupSize) {
      this.bimanderGroupSize = bimanderGroupSize;
      return this;
    }

    /**
     * Sets the fixed group size for the bimander encoding. The default value is 3.  This setting is only used if
     * the bimander group size is set to {@code FIXED}.
     * @param bimanderFixedGroupSize the bimander encoding fixed group size
     * @return the builder
     */
    public Builder bimanderFixedGroupSize(int bimanderFixedGroupSize) {
      this.bimanderFixedGroupSize = bimanderFixedGroupSize;
      return this;
    }

    /**
     * Sets the group size for the nesting encoding. The default value is 4.
     * @param nestingGroupSize the group size for the nesting encoding
     * @return the builder
     */
    public Builder nestingGroupSize(int nestingGroupSize) {
      this.nestingGroupSize = nestingGroupSize;
      return this;
    }

    /**
     * Sets the recursive bound for the product encoding. The default value is 20.
     * @param productRecursiveBound the recursive bound for the product encoding
     * @return the builder
     */
    public Builder productRecursiveBound(int productRecursiveBound) {
      this.productRecursiveBound = productRecursiveBound;
      return this;
    }

    /**
     * Sets the group size for the nesting encoding. The default value is 4.
     * @param commanderGroupSize the group size for the nesting encoding
     * @return the builder
     */
    public Builder commanderGroupSize(int commanderGroupSize) {
      this.commanderGroupSize = commanderGroupSize;
      return this;
    }

    /**
     * Builds the cardinality constraint encoder configuration.
     * @return the configuration
     */
    public CCConfig build() {
      return new CCConfig(this);
    }
  }
}
