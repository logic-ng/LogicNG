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

package org.logicng.pseudobooleans;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration for a pseudo-Boolean encoder.
 * @version 1.1
 * @since 1.1
 */
public final class PBConfig extends Configuration {

  /**
   * The pseudo-Boolean encoder.
   */
  public enum PB_ENCODER {
    SWC, BINARY_MERGE, ADDER_NETWORKS, BEST
  }

  final PB_ENCODER pbEncoder;
  final boolean binaryMergeUseGAC;
  final boolean binaryMergeNoSupportForSingleBit;
  final boolean binaryMergeUseWatchDog;

  /**
   * Constructs a new pseudo-Boolean encoder configuration from a given builder.
   * @param builder the builder
   */
  private PBConfig(final Builder builder) {
    super(ConfigurationType.PB_ENCODER);
    this.pbEncoder = builder.pbEncoder;
    this.binaryMergeUseGAC = builder.binaryMergeUseGAC;
    this.binaryMergeNoSupportForSingleBit = builder.binaryMergeNoSupportForSingleBit;
    this.binaryMergeUseWatchDog = builder.binaryMergeUseWatchDog;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PBConfig{").append(System.lineSeparator());
    sb.append("pbEncoder=").append(this.pbEncoder).append(System.lineSeparator());
    sb.append("binaryMergeUseGAC=").append(this.binaryMergeUseGAC).append(System.lineSeparator());
    sb.append("binaryMergeNoSupportForSingleBit=").append(this.binaryMergeNoSupportForSingleBit).append(System.lineSeparator());
    sb.append("binaryMergeUseWatchDog=").append(this.binaryMergeUseWatchDog).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a pseudo-Boolean encoder configuration.
   */
  public static class Builder {
    private PB_ENCODER pbEncoder = PB_ENCODER.BEST;
    private boolean binaryMergeUseGAC = true;
    private boolean binaryMergeNoSupportForSingleBit = false;
    private boolean binaryMergeUseWatchDog = true;

    /**
     * Sets the encoder for pseudo-Boolean constraints. The default value is {@code BEST}.
     * @param pbEncoder the pseudo-Boolean encoder
     * @return the builder
     */
    public Builder pbEncoding(final PB_ENCODER pbEncoder) {
      this.pbEncoder = pbEncoder;
      return this;
    }

    /**
     * Sets whether general arc consistency should be used in the binary merge encoding. The default value is {@code
     * true}.
     * @param binaryMergeUseGAC {@code true} if general arc consistency should be used, {@code false} otherwise
     * @return the builder
     */
    public Builder binaryMergeUseGAC(final boolean binaryMergeUseGAC) {
      this.binaryMergeUseGAC = binaryMergeUseGAC;
      return this;
    }

    /**
     * Sets the support for single bits in the binary merge encoding. The default value is {@code false}.
     * @param binaryMergeNoSupportForSingleBit {@code true} if the support for single bits should be disabled,
     *                                         {@code false} otherwise
     * @return the builder
     */
    public Builder binaryMergeNoSupportForSingleBit(final boolean binaryMergeNoSupportForSingleBit) {
      this.binaryMergeNoSupportForSingleBit = binaryMergeNoSupportForSingleBit;
      return this;
    }

    /**
     * Sets whether the watchdog encoding should be used in the binary merge encoding. The default value is {@code true}.
     * @param binaryMergeUseWatchDog {@code true} if the watchdog encoding should be used, {@code false} otherwise
     * @return the builder
     */
    public Builder binaryMergeUseWatchDog(final boolean binaryMergeUseWatchDog) {
      this.binaryMergeUseWatchDog = binaryMergeUseWatchDog;
      return this;
    }

    /**
     * Builds the pseudo-Boolean encoder configuration.
     * @return the configuration
     */
    public PBConfig build() {
      return new PBConfig(this);
    }
  }
}
