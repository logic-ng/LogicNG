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
 * A SAT handler which cancels the solving process after a given timeout.
 * @version 1.6.2
 * @since 1.0
 */
public final class TimeoutSATHandler extends TimeoutHandler implements SATHandler {

    /**
     * Constructs a new instance with a given timeout in milliseconds.
     * <p>
     * Note that it might take a few milliseconds more until the sat solver is actually
     * canceled, since the handler depends on the solvers call to {@code detectedConflict()}.
     * @param timeout the timeout in milliseconds
     */
    public TimeoutSATHandler(final long timeout) {
        super(timeout);
    }

    @Override
    public boolean detectedConflict() {
        return timeLimitExceeded();
    }

    @Override
    public void finishedSolving() {
        // nothing to do here
    }
}
