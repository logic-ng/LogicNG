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

/*
 * Glucose -- Copyright (c) 2009-2014, Gilles Audemard, Laurent Simon
 * CRIL - Univ. Artois, France
 * LRI  - Univ. Paris Sud, France (2009-2013)
 * Labri - Univ. Bordeaux, France
 * <p>
 * Syrup (Glucose Parallel) -- Copyright (c) 2013-2014, Gilles Audemard, Laurent Simon
 * CRIL - Univ. Artois, France
 * Labri - Univ. Bordeaux, France
 * <p>
 * Glucose sources are based on MiniSat (see below MiniSat copyrights). Permissions and copyrights of
 * Glucose (sources until 2013, Glucose 3.0, single core) are exactly the same as Minisat on which it
 * is based on. (see below).
 * <p>
 * Glucose-Syrup sources are based on another copyright. Permissions and copyrights for the parallel
 * version of Glucose-Syrup (the "Software") are granted, free of charge, to deal with the Software
 * without restriction, including the rights to use, copy, modify, merge, publish, distribute,
 * sublicence, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * - The above and below copyrights notices and this permission notice shall be included in all
 * copies or substantial portions of the Software;
 * - The parallel version of Glucose (all files modified since Glucose 3.0 releases, 2013) cannot
 * be used in any competitive event (sat competitions/evaluations) without the express permission of
 * the authors (Gilles Audemard / Laurent Simon). This is also the case for any competitive event
 * using Glucose Parallel as an embedded SAT engine (single core or not).
 * <p>
 * <p>
 * --------------- Original Minisat Copyrights
 * <p>
 * Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 * Copyright (c) 2007-2010, Niklas Sorensson
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.solvers.datastructures;

import org.logicng.collections.LNGIntVector;

/**
 * A bounded integer queue (for Glucose)
 * @version 1.3
 * @since 1.0
 */
public final class LNGBoundedIntQueue {
    private final LNGIntVector elems;
    private int first;
    private int last;
    private long sumOfQueue;
    private int maxSize;
    private int queueSize;

    /**
     * Constructs a new bounded int queue.
     */
    public LNGBoundedIntQueue() {
        this.elems = new LNGIntVector();
        this.first = 0;
        this.last = 0;
        this.sumOfQueue = 0;
        this.maxSize = 0;
        this.queueSize = 0;
    }

    /**
     * Initializes the size of this queue.
     * @param size the size
     */
    public void initSize(final int size) {
        this.growTo(size);
    }

    /**
     * Pushes a new element to the queue.
     * @param x the new element
     */
    public void push(final int x) {
        if (this.queueSize == this.maxSize) {
            assert this.last == this.first;
            this.sumOfQueue -= this.elems.get(this.last);
            if ((++this.last) == this.maxSize) {
                this.last = 0;
            }
        } else {
            this.queueSize++;
        }
        this.sumOfQueue += x;
        this.elems.set(this.first, x);
        if ((++this.first) == this.maxSize) {
            this.first = 0;
            this.last = 0;
        }
    }

    /**
     * Returns the average value of this queue.
     * @return the average value of this queue
     */
    public int avg() {
        return (int) (this.sumOfQueue / this.queueSize);
    }

    /**
     * Grows this queue to a given size.
     * @param size the size
     */
    private void growTo(final int size) {
        this.elems.growTo(size, 0);
        this.first = 0;
        this.maxSize = size;
        this.queueSize = 0;
        this.last = 0;
    }

    @Override
    public String toString() {
        return String.format("LNGBoundedIntQueue{first=%d, last=%d, sumOfQueue=%d, maxSize=%d, queueSize=%d, elems=%s}",
                this.first, this.last, this.sumOfQueue, this.maxSize, this.queueSize, this.elems);
    }
}
