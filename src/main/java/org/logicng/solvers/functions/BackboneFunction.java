// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.solvers.functions;

import static org.logicng.handlers.Handler.start;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneType;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.Consumer;

/**
 * A solver function which computes a backbone for the formula on the solver.
 * <p>
 * Backbone functions are instantiated via their builder {@link #builder()}.
 * @version 2.1.0
 * @since 2.0.0
 */
public final class BackboneFunction implements SolverFunction<Backbone> {

    private final SATHandler handler;
    private final Collection<Variable> variables;
    private final BackboneType type;

    private BackboneFunction(final SATHandler handler, final Collection<Variable> variables, final BackboneType type) {
        this.handler = handler;
        this.variables = variables;
        this.type = type;
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public Backbone apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        start(this.handler);
        SolverState stateBeforeBackbone = null;
        if (solver.canSaveLoadState()) {
            stateBeforeBackbone = solver.saveState();
        }
        final Backbone backbone = solver.underlyingSolver().computeBackbone(this.variables, this.type, this.handler);
        if (solver.canSaveLoadState()) {
            solver.loadState(stateBeforeBackbone);
        }
        return backbone;
    }

    /**
     * The builder for a backbone function.
     */
    public static class Builder {

        private SATHandler handler;
        private Collection<Variable> variables;
        private BackboneType type = BackboneType.POSITIVE_AND_NEGATIVE;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the SAT handler for this function.
         * @param handler the handler
         * @return the current builder
         */
        public Builder handler(final SATHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the variables which are relevant for the backbone computation.
         * @param variables the variables
         * @return the current builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the variables which are relevant for the backbone computation.
         * @param variables the variables
         * @return the current builder
         */
        public Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets the type of backbone which should be computed (default: POSITIVE_AND_NEGATIVE).
         * @param type the backbone type
         * @return the current builder
         */
        public Builder type(final BackboneType type) {
            this.type = type;
            return this;
        }

        /**
         * Builds the backbone function with the current builder's configuration.
         * @return the backbone function
         */
        public BackboneFunction build() {
            return new BackboneFunction(this.handler, this.variables, this.type);
        }
    }
}
