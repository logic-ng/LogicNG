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

import org.logicng.datastructures.Tristate;
import org.logicng.solvers.MiniSat;

import java.util.function.Consumer;

/**
 * An interface for a function which works on a given SAT solver and its state.
 * <p>
 * With the help of solver functions, additional methods can be plugged to
 * SAT solver without extending the solver classes themselves.
 * @param <RESULT> the result type of the function
 * @version 2.0.0
 * @since 2.0.0
 */
public interface SolverFunction<RESULT> {

    /**
     * Applies this function to the given solver.
     * @param solver       the solver on which the function should work on
     * @param resultSetter a setter for the result of the solver
     * @return the result of the function application
     */
    RESULT apply(MiniSat solver, Consumer<Tristate> resultSetter);
}
