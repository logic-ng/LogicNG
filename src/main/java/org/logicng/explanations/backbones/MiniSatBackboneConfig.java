package org.logicng.explanations.backbones;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.solvers.sat.MiniSatConfig;

public class MiniSatBackboneConfig extends Configuration {

  // Creation of initial candidate literals
  private final boolean initialLBCheckForUPZeroLiterals;
  private final boolean initialUBCheckForRotatableLiterals;

  // Reductions during search
  private final boolean checkForUPZeroLiterals;
  private final boolean checkForComplementModelLiterals;
  private final boolean checkForRotatableLiterals;

  public MiniSatBackboneConfig(final Builder builder) {
    super(ConfigurationType.BACKBONE);
    this.initialLBCheckForUPZeroLiterals = builder.initialLBCheckForUPZeroLiterals;
    this.initialUBCheckForRotatableLiterals = builder.initialUBCheckForRotatableLiterals;
    this.checkForUPZeroLiterals = builder.checkForUPZeroLiterals;
    this.checkForComplementModelLiterals = builder.checkForComplementModelLiterals;
    this.checkForRotatableLiterals = builder.checkForRotatableLiterals;
  }

  public boolean isInitialLBCheckForUPZeroLiterals() {
    return initialLBCheckForUPZeroLiterals;
  }

  public boolean isInitialUBCheckForRotatableLiterals() {
    return initialUBCheckForRotatableLiterals;
  }

  public boolean isCheckForUPZeroLiterals() {
    return checkForUPZeroLiterals;
  }

  public boolean isCheckForComplementModelLiterals() {
    return checkForComplementModelLiterals;
  }

  public boolean isCheckForRotatableLiterals() {
    return checkForRotatableLiterals;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MiniSatBackboneConfig{").append(System.lineSeparator());
    sb.append("initialLBCheckForUPZeroLiterals=").append(this.initialLBCheckForUPZeroLiterals).append(System.lineSeparator());
    sb.append("initialUBCheckForRotatableLiterals=").append(this.initialUBCheckForRotatableLiterals).append(System.lineSeparator());
    sb.append("checkForUPZeroLiterals=").append(this.checkForUPZeroLiterals).append(System.lineSeparator());
    sb.append("checkForComplementModelLiterals=").append(this.checkForComplementModelLiterals).append(System.lineSeparator());
    sb.append("checkForRotatableLiterals=").append(this.checkForRotatableLiterals).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a Backbone configuration.
   */
  public static class Builder {

    // Creation of initial candidate literals
    private boolean initialLBCheckForUPZeroLiterals = true;
    private boolean initialUBCheckForRotatableLiterals = true;

    // Reducing candidate literals during search
    private boolean checkForUPZeroLiterals = true;
    private boolean checkForComplementModelLiterals = true;
    private boolean checkForRotatableLiterals = true;

    public MiniSatBackboneConfig.Builder initialLBCheckForUPZeroLiterals(boolean initialLBCheckForUPZeroLiterals) {
      this.initialLBCheckForUPZeroLiterals = initialLBCheckForUPZeroLiterals;
      return this;
    }

    public MiniSatBackboneConfig.Builder initialUBCheckForRotatableLiterals(boolean initialUBCheckForRotatableLiterals) {
      this.initialUBCheckForRotatableLiterals = initialUBCheckForRotatableLiterals;
      return this;
    }

    public MiniSatBackboneConfig.Builder checkForUPZeroLiterals(boolean checkForUPZeroLiterals) {
      this.checkForUPZeroLiterals = checkForUPZeroLiterals;
      return this;
    }

    public MiniSatBackboneConfig.Builder checkForComplementModelLiterals(boolean checkForComplementModelLiterals) {
      this.checkForComplementModelLiterals = checkForComplementModelLiterals;
      return this;
    }

    public MiniSatBackboneConfig.Builder checkForRotatableLiterals(boolean checkForRotatableLiterals) {
      this.checkForRotatableLiterals = checkForRotatableLiterals;
      return this;
    }

    /**
     * Builds the configuration.
     * @return the configuration.
     */
    public MiniSatBackboneConfig build() {
      return new MiniSatBackboneConfig(this);
    }
  }
}
