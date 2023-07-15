// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.BitSet;

/**
 * Interface for a DNNF SAT Solver.
 * @version 2.0.0
 * @since 2.0.0
 */
public interface DnnfSatSolver {

    /**
     * Adds a formula to the solver.  The formula is first converted to CNF.
     * @param formula the formula
     */
    void add(final Formula formula);

    /**
     * Initializes the solver by performing unit propagation
     * and returns {@code false} if UP finds a contradiction, otherwise {@code true}.
     * @return {@code false} if UP finds a contradiction, otherwise {@code true}
     */
    boolean start();

    /**
     * Performs a decision of the given variable to the given phase and performs UP.
     * @param var   the variable
     * @param phase the phase
     * @return {@code false} if UP finds a contradiction, otherwise {@code true}
     */
    boolean decide(final int var, boolean phase);

    /**
     * Reverts the decision of the given variable.
     * @param var the variable
     */
    void undoDecide(final int var);

    /**
     * Returns {@code true} if the current decision level was previously
     * selected as backtrack level.
     * @return {@code true} if the current decision level was previously
     * selected as backtrack level, otherwise {@code false}
     */
    boolean atAssertionLevel();

    /**
     * Enqueues the literal which was computed by the latest backtracking, performs UP
     * and return {@code false} if UP finds a contradiction, otherwise {@code true}.
     * @return {@code false} if UP finds a contradiction, otherwise {@code true}
     */
    boolean assertCdLiteral();

    /**
     * Returns a conjunction of all literals in the known variables which were
     * propagated by the last UP.
     * @param knownVariables the known variables
     * @return a conjunction of the relevant propagated literals
     */
    Formula newlyImplied(final BitSet knownVariables);

    /**
     * Returns the index of the variable of the given literal.
     * @param lit the literal
     * @return the index of the literal's variable
     */
    int variableIndex(final Literal lit);

    /**
     * Returns the literal corresponding to the given variable index.
     * @param var the variable index
     * @return the literal corresponding to the given variable index
     */
    Literal litForIdx(final int var);

    /**
     * Returns the current assignment of the given literal.
     * @param lit the literal
     * @return the current assignment of the given literal
     */
    Tristate valueOf(final int lit);
}
