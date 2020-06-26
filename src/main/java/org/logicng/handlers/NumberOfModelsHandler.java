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
 * A model enumeration handler that terminates the solving process after a given number of models.
 * @version 1.6.2
 * @since 1.0
 */
public final class NumberOfModelsHandler extends ComputationHandler implements ModelEnumerationHandler {

    private final int bound;
    private int count;

    /**
     * Constructs a new model handler with an upper bound for the number of models (inclusive).
     * @param bound the upper bound
     * @throws IllegalArgumentException if the number of models to generate is &lt;= 0
     */
    public NumberOfModelsHandler(final int bound) {
        if (bound <= 0) {
            throw new IllegalArgumentException("You must generate at least 1 model.");
        }
        this.bound = bound;
    }

    @Override
    public void started() {
        super.started();
        this.count = 0;
    }

    @Override
    public SATHandler satHandler() {
        return null;
    }

    @Override
    public boolean foundModel(final Assignment assignment) {
        this.aborted = ++this.count >= this.bound;
        return !aborted;
    }

    @Override
    public boolean satSolverFinished() {
        return true;
    }
}
