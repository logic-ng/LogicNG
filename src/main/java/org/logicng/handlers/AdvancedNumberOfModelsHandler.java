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

/**
 * A model enumeration handler that terminates the solving process after a given number of models.
 * @version 2.4.0
 * @since 2.4.0
 */
public class AdvancedNumberOfModelsHandler extends ComputationHandler implements AdvancedModelEnumerationHandler {

    private final int bound;
    private int countCommitted;
    private int countUncommitted;

    /**
     * Constructs a new model handler with an upper bound for the number of models (inclusive).
     * @param bound the upper bound
     * @throws IllegalArgumentException if the number of models to generate is &lt;= 0
     */
    public AdvancedNumberOfModelsHandler(final int bound) {
        if (bound <= 0) {
            throw new IllegalArgumentException("You must generate at least 1 model.");
        }
        this.bound = bound;
    }

    @Override
    public void started() {
        super.started();
        this.countCommitted = 0;
        this.countUncommitted = 0;
    }

    @Override
    public SATHandler satHandler() {
        return null;
    }

    @Override
    public boolean foundModels(final int numberOfModels) {
        this.aborted = this.countUncommitted + this.countCommitted + numberOfModels > this.bound;
        if (!this.aborted) {
            this.countUncommitted += numberOfModels;
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean commit() {
        this.countCommitted += this.countUncommitted;
        this.countUncommitted = 0;
        return true;
    }

    @Override
    public boolean rollback() {
        this.countUncommitted = 0;
        return true;
    }
}
