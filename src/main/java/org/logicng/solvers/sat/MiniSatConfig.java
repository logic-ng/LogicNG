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

package org.logicng.solvers.sat;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.Formula;
import org.logicng.solvers.SATSolver;

/**
 * The configuration object for a MiniSAT-style SAT solver.
 * @version 2.0.0
 * @since 1.0
 */
public final class MiniSatConfig extends Configuration {

    /**
     * The different methods for clause minimization.
     * <ul>
     * <li> {@code NONE} - no minimization is performed
     * <li> {@code BASIC} - local minimization is performed
     * <li> {@code DEEP} - recursive minimization is performed
     * </ul>
     */
    public enum ClauseMinimization {
        NONE, BASIC, DEEP
    }

    /**
     * The different methods for generating a CNF for a formula to put on the solver.
     * <ul>
     * <li> {@code FACTORY_CNF} calls the {@link Formula#cnf()} method on the formula
     * to convert it to CNF.  Therefore the CNF including all its auxiliary variables will
     * be added to the formula factory.
     * <li> {@code PG_ON_SOLVER} uses a solver-internal implementation of Plaisted-Greenbaum.
     * Auxiliary variables are only added on the solver, not on the factory.  This usually
     * leads to a reduced heap usage and often faster performance.
     * Before applying Plaisted-Greenbaum, this method performs an NNF transformation on the
     * input formula first.
     * <li> {@code FULL_PG_ON_SOLVER} uses a solver-internal implementation of Plaisted-Greenbaum.
     * Auxiliary variables are only added on the solver, not on the factory.  This usually
     * leads to a reduced heap usage and often faster performance.
     * In contrast to {@code PG_ON_SOLVER}, this method does not transform the input formula to
     * NNF first. The Plaisted-Greenbaum transformation is applied directly to all operators of
     * the formula, hence prefix {@code FULL}. Without the NNF transformation the formula factory
     * and the heap will not be polluted with intermediate formulas.
     * </ul>
     */
    public enum CNFMethod {
        FACTORY_CNF, PG_ON_SOLVER, FULL_PG_ON_SOLVER
    }

    final double varDecay;
    final double varInc;
    final ClauseMinimization clauseMin;
    final int restartFirst;
    final double restartInc;
    final double clauseDecay;
    final boolean removeSatisfied;
    final double learntsizeFactor;
    final double learntsizeInc;
    final boolean incremental;
    final boolean initialPhase;
    final boolean proofGeneration;
    final CNFMethod cnfMethod;
    final boolean auxiliaryVariablesInModels;
    final boolean bbInitialUBCheckForRotatableLiterals;
    final boolean bbCheckForComplementModelLiterals;
    final boolean bbCheckForRotatableLiterals;

    /**
     * Constructs a new MiniSAT configuration from a given builder.
     * @param builder the builder
     */
    private MiniSatConfig(final Builder builder) {
        super(ConfigurationType.MINISAT);
        this.varDecay = builder.varDecay;
        this.varInc = builder.varInc;
        this.clauseMin = builder.clauseMin;
        this.restartFirst = builder.restartFirst;
        this.restartInc = builder.restartInc;
        this.clauseDecay = builder.clauseDecay;
        this.removeSatisfied = builder.removeSatisfied;
        this.learntsizeFactor = builder.learntsizeFactor;
        this.learntsizeInc = builder.learntsizeInc;
        this.incremental = builder.incremental;
        this.initialPhase = builder.initialPhase;
        this.proofGeneration = builder.proofGeneration;
        this.cnfMethod = builder.cnfMethod;
        this.auxiliaryVariablesInModels = builder.auxiliaryVariablesInModels;
        this.bbInitialUBCheckForRotatableLiterals = builder.bbInitialUBCheckForRotatableLiterals;
        this.bbCheckForComplementModelLiterals = builder.bbCheckForComplementModelLiterals;
        this.bbCheckForRotatableLiterals = builder.bbCheckForRotatableLiterals;
    }

    /**
     * Returns a new builder for the configuration.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Returns whether the solver is incremental or not.
     * @return {@code true} if the solver is incremental, {@code false} otherwise
     */
    public boolean incremental() {
        return this.incremental;
    }

    /**
     * Returns the initial phase of the solver.
     * @return the initial phase of the solver
     */
    public boolean initialPhase() {
        return this.initialPhase;
    }

    /**
     * Returns whether proof generation should be performed or not.
     * @return whether proof generation should be performed or not
     */
    public boolean proofGeneration() {
        return this.proofGeneration;
    }

    /**
     * Returns the CNF method which should be used.
     * @return the CNF method
     */
    public CNFMethod getCnfMethod() {
        return this.cnfMethod;
    }

    /**
     * Returns whether auxiliary Variables should be included in the model or not.
     * @return whether auxiliary Variables should be included in the model or not
     */
    public boolean isAuxiliaryVariablesInModels() {
        return this.auxiliaryVariablesInModels;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("MiniSatConfig{").append(System.lineSeparator());
        sb.append("varDecay=").append(this.varDecay).append(System.lineSeparator());
        sb.append("varInc=").append(this.varInc).append(System.lineSeparator());
        sb.append("clauseMin=").append(this.clauseMin).append(System.lineSeparator());
        sb.append("restartFirst=").append(this.restartFirst).append(System.lineSeparator());
        sb.append("restartInc=").append(this.restartInc).append(System.lineSeparator());
        sb.append("clauseDecay=").append(this.clauseDecay).append(System.lineSeparator());
        sb.append("removeSatisfied=").append(this.removeSatisfied).append(System.lineSeparator());
        sb.append("learntsizeFactor=").append(this.learntsizeFactor).append(System.lineSeparator());
        sb.append("learntsizeInc=").append(this.learntsizeInc).append(System.lineSeparator());
        sb.append("incremental=").append(this.incremental).append(System.lineSeparator());
        sb.append("initialPhase=").append(this.initialPhase).append(System.lineSeparator());
        sb.append("proofGeneration=").append(this.proofGeneration).append(System.lineSeparator());
        sb.append("cnfMethod=").append(this.cnfMethod).append(System.lineSeparator());
        sb.append("auxiliaryVariablesInModels=").append(this.auxiliaryVariablesInModels).append(System.lineSeparator());
        sb.append("bbInitialUBCheckForRotatableLiterals=").append(this.bbInitialUBCheckForRotatableLiterals).append(System.lineSeparator());
        sb.append("bbCheckForComplementModelLiterals=").append(this.bbCheckForComplementModelLiterals).append(System.lineSeparator());
        sb.append("bbCheckForRotatableLiterals=").append(this.bbCheckForRotatableLiterals).append(System.lineSeparator());
        sb.append("}");
        return sb.toString();
    }

    /**
     * The builder for a MiniSAT configuration.
     * @version 2.0.0
     * @since 1.0
     */
    public static class Builder {
        private double varDecay = 0.95;
        private double varInc = 1.0;
        private ClauseMinimization clauseMin = ClauseMinimization.DEEP;
        private int restartFirst = 100;
        private double restartInc = 2.0;
        private double clauseDecay = 0.999;
        private boolean removeSatisfied = true;
        private double learntsizeFactor = 1.0 / 3.0;
        private double learntsizeInc = 1.1;
        private boolean incremental = true;
        private boolean initialPhase = false;
        private boolean proofGeneration = false;
        private CNFMethod cnfMethod = CNFMethod.PG_ON_SOLVER;
        private boolean auxiliaryVariablesInModels = false;
        private boolean bbInitialUBCheckForRotatableLiterals = true;
        private boolean bbCheckForComplementModelLiterals = true;
        private boolean bbCheckForRotatableLiterals = true;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the variable activity decay factor to a given value. The default value is 0.95.
         * @param varDecay the value (should be in the range 0..1)
         * @return the builder
         */
        public Builder varDecay(final double varDecay) {
            this.varDecay = varDecay;
            return this;
        }

        /**
         * Sets the initial value to bump a variable with each time it is used in conflict resolution to a given value.
         * The default value is 1.0.
         * @param varInc the value
         * @return the builder
         */
        public Builder varInc(final double varInc) {
            this.varInc = varInc;
            return this;
        }

        /**
         * Sets the clause minimization method. The default value is {@code DEEP}.
         * @param ccmin the value
         * @return the builder
         */
        public Builder clMinimization(final ClauseMinimization ccmin) {
            this.clauseMin = ccmin;
            return this;
        }

        /**
         * Sets the base restart interval to the given value. The default value is 100.
         * @param restartFirst the value (should be at least 1)
         * @return the builder
         */
        public Builder restartFirst(final int restartFirst) {
            this.restartFirst = restartFirst;
            return this;
        }

        /**
         * Sets the restart interval increase factor to the given value. The default value is 2.0.
         * @param restartInc the value (should be at least 1)
         * @return the builder
         */
        public Builder restartInc(final double restartInc) {
            this.restartInc = restartInc;
            return this;
        }

        /**
         * Sets the clause activity decay factor to a given value. The default value is 0.999.
         * @param clauseDecay the value (should be in the range 0..1)
         * @return the builder
         */
        public Builder clauseDecay(final double clauseDecay) {
            this.clauseDecay = clauseDecay;
            return this;
        }

        /**
         * If turned on, the satisfied original clauses will be removed when simplifying on level 0, when turned off,
         * only the satisfied learnt clauses will be removed.  The default value is {@code true}.
         * @param removeSatisfied {@code true} if the original clauses should be simplified, {@code false} otherwise
         * @return the builder
         */
        public Builder removeSatisfied(final boolean removeSatisfied) {
            this.removeSatisfied = removeSatisfied;
            return this;
        }

        /**
         * Sets the initial limit for learnt clauses as a factor of the original clauses to the given value.  The default
         * value is 1/3.
         * @param learntsizeFactor the value
         * @return the builder
         */
        public Builder lsFactor(final double learntsizeFactor) {
            this.learntsizeFactor = learntsizeFactor;
            return this;
        }

        /**
         * Sets the factor by which the limit for learnt clauses is multiplied every restart to a given value. The default
         * value is 1.1.
         * @param learntsizeInc the value
         * @return the builder
         */
        public Builder lsInc(final double learntsizeInc) {
            this.learntsizeInc = learntsizeInc;
            return this;
        }

        /**
         * Turns the incremental mode of the solver off and on.  The default value is {@code true}.
         * @param incremental {@code true} if incremental mode is turned on, {@code false} otherwise
         * @return the builder
         */
        public Builder incremental(final boolean incremental) {
            this.incremental = incremental;
            return this;
        }

        /**
         * Sets the initial phase of the solver.  The default value is {@code true}.
         * @param initialPhase the initial phase
         * @return the builder
         */
        public Builder initialPhase(final boolean initialPhase) {
            this.initialPhase = initialPhase;
            return this;
        }

        /**
         * Sets whether the information for generating a proof with DRUP should be recorded or not.  The default
         * value is {@code false}.
         * @param proofGeneration {@code true} if proof generating information should be recorded, {@code false} otherwise
         * @return the builder
         */
        public Builder proofGeneration(final boolean proofGeneration) {
            this.proofGeneration = proofGeneration;
            return this;
        }

        /**
         * Sets the CNF method for converting formula which are not in CNF for the solver.  The default value
         * is {@code FACTORY_CNF}.
         * @param cnfMethod the CNF method
         * @return the builder
         */
        public Builder cnfMethod(final CNFMethod cnfMethod) {
            this.cnfMethod = cnfMethod;
            return this;
        }

        /**
         * Sets whether auxiliary variables (CNF, cardinality constraints, pseudo-Boolean constraints) should
         * be included in methods like {@link SATSolver#model()} or {@link SATSolver#enumerateAllModels()}.  If
         * set to {@code true}, all variables will be included in these methods,  if set to {@code false}, variables
         * starting with "@RESERVED_CC_", "@RESERVED_PB_", and "@RESERVED_CNF_" will be excluded from the models.
         * The default value is {@code true}.
         * @param auxiliaryVariablesInModels {@code true} if auxiliary variables should be included in the models,
         *                                   {@code false} otherwise
         * @return the builder
         */
        public Builder auxiliaryVariablesInModels(final boolean auxiliaryVariablesInModels) {
            this.auxiliaryVariablesInModels = auxiliaryVariablesInModels;
            return this;
        }

        /**
         * Sets whether the backbone algorithm should check for rotatable literals.
         * The default value is {@code true}.
         * @param checkForRotatableLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                  rotatables or {@code false} otherwise.
         * @return the builder
         */
        public Builder bbCheckForRotatableLiterals(final boolean checkForRotatableLiterals) {
            this.bbCheckForRotatableLiterals = checkForRotatableLiterals;
            return this;
        }

        /**
         * Sets whether the backbone algorithm should check for rotatable literals during initial unit propagation.
         * The default value is {@code true}.
         * @param initialUBCheckForRotatableLiterals the boolean value that is {@code true} if the algorithm should
         *                                           check for rotatables or {@code false} otherwise.
         * @return the builder
         */
        public Builder bbInitialUBCheckForRotatableLiterals(final boolean initialUBCheckForRotatableLiterals) {
            this.bbInitialUBCheckForRotatableLiterals = initialUBCheckForRotatableLiterals;
            return this;
        }

        /**
         * Sets whether the backbone algorithm should check for complement model literals.
         * The default value is {@code true}.
         * @param checkForComplementModelLiterals the boolean value that is {@code true} if the algorithm should check for
         *                                        complement literals or {@code false} otherwise.
         * @return the builder
         */
        public Builder bbCheckForComplementModelLiterals(final boolean checkForComplementModelLiterals) {
            this.bbCheckForComplementModelLiterals = checkForComplementModelLiterals;
            return this;
        }

        /**
         * Builds the MiniSAT configuration.
         * @return the configuration
         */
        public MiniSatConfig build() {
            return new MiniSatConfig(this);
        }
    }
}
