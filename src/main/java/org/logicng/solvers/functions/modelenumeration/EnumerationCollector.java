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

package org.logicng.solvers.functions.modelenumeration;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Model;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;

import java.util.List;

/**
 * An interface for enumeration collectors.
 * <p>
 * An enumeration collector gathers the found models given by {@link #addModel(LNGBooleanVector, MiniSat, LNGIntVector, AdvancedModelEnumerationHandler)}.
 * Added Models added can potentially be discarded later via {@link #rollback(AdvancedModelEnumerationHandler)}. To prevent models from being rolled back
 * one can call {@link #commit(AdvancedModelEnumerationHandler)}. With {@link #getResult()} the result, the models committed models, can be retrieved.
 * @param <RESULT> The result type of the model enumeration function.  Can be e.g. a model count, a list of models, or a BDD.
 * @version 2.4.0
 * @since 2.4.0
 */
public interface EnumerationCollector<RESULT> {

    /**
     * Add model to the enumeration collector.
     * @param modelFromSolver    the model from the solver
     * @param solver             the solver
     * @param relevantAllIndices the relevant indices
     * @param handler            the advanced model enumeration handler
     * @return true if adding the model was successful, false otherwise
     */
    boolean addModel(LNGBooleanVector modelFromSolver, MiniSat solver, LNGIntVector relevantAllIndices, AdvancedModelEnumerationHandler handler);

    /**
     * All founds models since the last commit call are confirmed and cannot be rolled back.
     * <p>
     * Calls the {@code commit()} routine of {@code handler}.
     * @param handler the advanced model enumeration handler
     * @return {@code true} if the computation should continue, otherwise {@code false}
     */
    boolean commit(AdvancedModelEnumerationHandler handler);

    /**
     * All found models since the last commit should be discarded.
     * <p>
     * Calls the {@code rollback} routine of {@code handler}.
     * @param handler the advanced model enumeration handler
     * @return {@code true} if the computation should continue, otherwise {@code false}
     */
    boolean rollback(AdvancedModelEnumerationHandler handler);

    /**
     * All found models since the last commit will be discarded and returned.
     * <p>
     * Calls the {@code rollback} routine of {@code handler}.
     * @param solver  solver used for the enumeration
     * @param handler the advanced model enumeration handler
     * @return list of all discarded models
     */
    List<Model> rollbackAndReturnModels(final MiniSat solver, AdvancedModelEnumerationHandler handler);

    /**
     * @return the committed state of the collector
     */
    RESULT getResult();
}
