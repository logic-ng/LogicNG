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

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration for a cardinality constraint encoder.
 * @version 1.1
 * @since 1.1
 */
public final class CCConfig extends Configuration {

  public enum AMO_ENCODER {PURE, LADDER, PRODUCT, NESTED, COMMANDER, BINARY, BIMANDER, BEST}

  public enum EXO_ENCODER {PURE, LADDER, PRODUCT, NESTED, COMMANDER, BINARY, BIMANDER, BEST}

  public enum AMK_ENCODER {TOTALIZER, MODULAD_TOTALIZER, CARDINALITY_NETWORK, BEST}

  public enum ALK_ENCODER {ALK_TOTALIZER, BEST}

  public enum BIMANDER_GROUP_SIZE {HALF, SQRT, FIXED}

  final AMO_ENCODER amoEncoder;
  final EXO_ENCODER exoEncoder;
  final AMK_ENCODER amkEncoder;
  final ALK_ENCODER alkEncoder;
  final BIMANDER_GROUP_SIZE bimanderGroupSize;
  final int fixedGroupSize;
  final int nestingGroupSize;

  /**
   * The builder for a cardinality constraint encoder configuration.
   */
  public static class Builder {
    private AMO_ENCODER amoEncoder = AMO_ENCODER.BEST;
    private EXO_ENCODER exoEncoder = EXO_ENCODER.BEST;
    private AMK_ENCODER amkEncoder = AMK_ENCODER.BEST;
    private ALK_ENCODER alkEncoder = ALK_ENCODER.BEST;
    private BIMANDER_GROUP_SIZE bimanderGroupSize = BIMANDER_GROUP_SIZE.SQRT;
    private int fixedGroupSize = 3;
    private int nestingGroupSize = 4;

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
     * Sets the encoder for exactly-one constraints. The default value is {@code BEST}.
     * @param exoEncoder the exactly-one encoder
     * @return the builder
     */
    public Builder exoEncoding(final EXO_ENCODER exoEncoder) {
      this.exoEncoder = exoEncoder;
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
     * @param fixedGroupSize the bimander encoding fixed group size
     * @return the builder
     */
    public Builder fixedGroupSize(int fixedGroupSize) {
      this.fixedGroupSize = fixedGroupSize;
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
  }

  /**
   * Constructs a new cardinality constraint encoder configuration from a given builder.
   * @param builder the builder
   */
  private CCConfig(final Builder builder) {
    super(ConfigurationType.CC_ENCODER);
    this.amoEncoder = builder.amoEncoder;
    this.exoEncoder = builder.exoEncoder;
    this.amkEncoder = builder.amkEncoder;
    this.alkEncoder = builder.alkEncoder;
    this.bimanderGroupSize = builder.bimanderGroupSize;
    this.fixedGroupSize = builder.fixedGroupSize;
    this.nestingGroupSize = builder.nestingGroupSize;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CCConfig{\n");
    sb.append("amoEncoder=").append(this.amoEncoder).append("\n");
    sb.append("exoEncoder=").append(this.exoEncoder).append("\n");
    sb.append("amkEncoder=").append(this.amkEncoder).append("\n");
    sb.append("alkEncoder=").append(this.alkEncoder).append("\n");
    sb.append("bimanderGroupSize=").append(this.bimanderGroupSize).append("\n");
    sb.append("fixedGroupSize=").append(this.fixedGroupSize).append("\n");
    sb.append("nestingGroupSize=").append(this.nestingGroupSize).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
