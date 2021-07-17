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
        start(handler);
        SolverState stateBeforeBackbone = null;
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            stateBeforeBackbone = solver.saveState();
        }
        final Backbone backbone = solver.underlyingSolver().computeBackbone(this.variables, this.type, handler);
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
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
