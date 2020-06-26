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

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * Interface for a handler for the enumeration of models.
 * @version 1.6.2
 * @since 1.0
 */
public interface ModelEnumerationHandler extends Handler {

    /**
     * Returns a SAT handler which can be used to cancel internal SAT calls of the model enumeration process.
     * @return a SAT handler
     */
    SATHandler satHandler();

    /**
     * This method is called every time a model is found.
     * @param assignment the respective model
     * @return whether more models should be searched or not
     */
    boolean foundModel(Assignment assignment);

    /**
     * This method is called every time the SAT solver finished.
     * @return whether more models should be searched or not
     */
    boolean satSolverFinished();
}
