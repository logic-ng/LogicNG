package org.logicng.solvers.maxsat;

import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

import java.util.Objects;

public class OptimizationConfig {
    public enum OptimizationType {
        SAT_OPTIMIZATION,
        MAXSAT_INCWBO,
        MAXSAT_LINEAR_SU,
        MAXSAT_LINEAR_US,
        MAXSAT_MSU3,
        MAXSAT_OLL,
        MAXSAT_WBO,
    }

    private final OptimizationType optimizationType;
    private final MaxSATConfig maxSATConfig;
    private final OptimizationHandler optimizationHandler;
    private final MaxSATHandler maxSATHandler;

    public OptimizationConfig(
            final OptimizationType optType,
            final MaxSATConfig maxConfig,
            final OptimizationHandler optHandler,
            final MaxSATHandler maxHandler
    ) {
        this.optimizationType = optType;
        this.maxSATConfig = maxConfig;
        this.optimizationHandler = optHandler;
        this.maxSATHandler = maxHandler;
    }

    public OptimizationType getOptimizationType() {
        return this.optimizationType;
    }

    public MaxSATConfig getMaxSATConfig() {
        return this.maxSATConfig;
    }

    public OptimizationHandler getOptimizationHandler() {
        return this.optimizationHandler;
    }

    public MaxSATHandler getMaxSATHandler() {
        return this.maxSATHandler;
    }

    public MaxSATSolver genMaxSATSolver(final FormulaFactory f) {
        switch (this.optimizationType) {
            case MAXSAT_INCWBO:
                return this.maxSATConfig == null ? MaxSATSolver.incWBO(f) : MaxSATSolver.incWBO(f, this.maxSATConfig);
            case MAXSAT_LINEAR_SU:
                return this.maxSATConfig == null ? MaxSATSolver.linearSU(f) : MaxSATSolver.linearSU(f, this.maxSATConfig);
            case MAXSAT_LINEAR_US:
                return this.maxSATConfig == null ? MaxSATSolver.linearUS(f) : MaxSATSolver.linearUS(f, this.maxSATConfig);
            case MAXSAT_MSU3:
                return this.maxSATConfig == null ? MaxSATSolver.msu3(f) : MaxSATSolver.msu3(f, this.maxSATConfig);
            case MAXSAT_OLL:
                return this.maxSATConfig == null ? MaxSATSolver.oll(f) : MaxSATSolver.oll(f, this.maxSATConfig);
            case MAXSAT_WBO:
                return this.maxSATConfig == null ? MaxSATSolver.wbo(f) : MaxSATSolver.wbo(f, this.maxSATConfig);
            default:
                throw new IllegalArgumentException("Not a valid MaxSAT algorithm: " + this.optimizationType);
        }
    }

    @Override
    public boolean equals(final Object object) {
        if (this == object) {
            return true;
        }
        if (object == null || getClass() != object.getClass()) {
            return false;
        }
        final OptimizationConfig that = (OptimizationConfig) object;
        return this.optimizationType == that.optimizationType &&
                Objects.equals(this.maxSATConfig, that.maxSATConfig) &&
                Objects.equals(this.optimizationHandler, that.optimizationHandler) &&
                Objects.equals(this.maxSATHandler, that.maxSATHandler);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.optimizationType, this.maxSATConfig, this.optimizationHandler, this.maxSATHandler);
    }

    @Override
    public String toString() {
        return "OptimizationConfig{" +
                "optimizationType=" + this.optimizationType +
                ", maxSATConfig=" + this.maxSATConfig +
                ", optimizationHandler=" + this.optimizationHandler +
                ", maxSATHandler=" + this.maxSATHandler +
                '}';
    }
}
