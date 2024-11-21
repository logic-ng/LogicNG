package org.logicng.solvers.maxsat;

import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

import java.util.Objects;

/**
 * Configuration for optimization via a SAT or MaxSAT solver.
 * <p>
 * Some algorithms use optimization internally.  If they use many incremental
 * solving steps, they usually use the SAT solver based
 * {@link org.logicng.solvers.functions.OptimizationFunction}.  For some cases
 * however it can be more performant to use a MaxSAT solver based optimization
 * with the drawback of generating the solver again in each step.
 * <p>
 * These algorithms can be configured with this config object.
 * @version 2.6.0
 * @since 2.6.0
 */
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

    private OptimizationConfig(
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

    /**
     * Generate a MaxSAT solver based configuration
     * @param optType    the optimization type (MaxSAT algorithm)
     * @param maxConfig  the optional MaxSAT solver configuration
     * @param maxHandler the optional MaxSAT solver handler
     * @return the configuration
     */
    public static OptimizationConfig maxsat(
            final OptimizationType optType,
            final MaxSATConfig maxConfig,
            final MaxSATHandler maxHandler
    ) {
        if (optType == OptimizationType.SAT_OPTIMIZATION) {
            throw new IllegalArgumentException("SAT Optimization cannot be parametrized with MaxSat config and handler");
        }
        return new OptimizationConfig(optType, maxConfig, null, maxHandler);
    }

    /**
     * Generate a SAT solver based configuration
     * @param optHandler the optional optimization handler
     * @return the configuration
     */
    public static OptimizationConfig sat(final OptimizationHandler optHandler) {
        return new OptimizationConfig(OptimizationType.SAT_OPTIMIZATION, null, optHandler, null);
    }

    /**
     * Returns the optimization type.
     * @return the optimization type
     */
    public OptimizationType getOptimizationType() {
        return this.optimizationType;
    }

    /**
     * Returns the optional MaxSAT configuration
     * @return the optional MaxSAT configuration
     */
    public MaxSATConfig getMaxSATConfig() {
        return this.maxSATConfig;
    }

    /**
     * Returns the optional optimization handler.
     * @return the optional optimization handler
     */
    public OptimizationHandler getOptimizationHandler() {
        return this.optimizationHandler;
    }

    /**
     * Returns the optional MaxSAT handler.
     * @return the optional MaxSAT handler
     */
    public MaxSATHandler getMaxSATHandler() {
        return this.maxSATHandler;
    }

    /**
     * Generates a MaxSAT solver with the current configuration.
     * @param f the formula factory
     * @return the MAxSAT solver
     */
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

    /**
     * Starts this config's handler (if present)
     */
    public void startHandler() {
        if (this.optimizationHandler != null) {
            this.optimizationHandler.started();
        }
        if (this.maxSATHandler != null) {
            this.maxSATHandler.started();
        }
    }

    /**
     * Return the SAT handler of this config's handler (if present)
     * @return the SAT handler
     */
    public SATHandler satHandler() {
        if (this.optimizationHandler != null) {
            return this.optimizationHandler.satHandler();
        }
        if (this.maxSATHandler != null) {
            return this.maxSATHandler.satHandler();
        }
        return null;
    }

    /**
     * Returns whether this config's handler (if present) was aborted.
     * @return whether this config's handler was aborted
     */
    public boolean aborted() {
        if (this.optimizationHandler != null) {
            return this.optimizationHandler.aborted();
        }
        if (this.maxSATHandler != null) {
            return this.maxSATHandler.aborted();
        }
        return false;
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
