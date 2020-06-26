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
 * MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.solvers.datastructures;

import org.logicng.collections.LNGIntVector;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * A minimalistic heap implementation.
 * @version 1.3
 * @since 1.0
 */
public final class LNGHeap {

    private final MiniSatStyleSolver s;
    private final LNGIntVector heap;
    private final LNGIntVector indices;

    /**
     * Constructs a new heap for a given solver.  The solver is required to access it's activity information stored
     * for variables.  The initial size of the heap is 1000 elements.
     * @param solver the solver
     */
    public LNGHeap(final MiniSatStyleSolver solver) {
        this.s = solver;
        this.heap = new LNGIntVector(1000);
        this.indices = new LNGIntVector(1000);
    }

    /**
     * Returns the left position on the heap for a given position.
     * @param pos the position
     * @return the left position
     */
    private static int left(final int pos) {
        return pos * 2 + 1;
    }

    /**
     * Returns the right position on the heap for a given position.
     * @param pos the position
     * @return the right position
     */
    private static int right(final int pos) {
        return (pos + 1) * 2;
    }

    /**
     * Returns the parent position on the heap for a given position.
     * @param pos the position
     * @return the parent position
     */
    private static int parent(final int pos) {
        return (pos - 1) >> 1;
    }

    /**
     * Returns the size of the heap.
     * @return the size of the heap
     */
    public int size() {
        return this.heap.size();
    }

    /**
     * Returns {@code true} if the heap ist empty, {@code false} otherwise.
     * @return {@code true} if the heap ist empty
     */
    public boolean empty() {
        return this.heap.size() == 0;
    }

    /**
     * Returns {@code true} if a given element is in the heap, {@code false} otherwise.
     * @param n the element
     * @return {@code true} if a given variable index is in the heap
     */
    public boolean inHeap(final int n) {
        return n < this.indices.size() && this.indices.get(n) >= 0;
    }

    /**
     * Returns the element at a given position in the heap.
     * @param index the position
     * @return the element at the position
     */
    public int get(final int index) {
        assert index < this.heap.size();
        return this.heap.get(index);
    }

    /**
     * Decrease an element's position in the heap
     * @param n the element
     */
    public void decrease(final int n) {
        assert this.inHeap(n);
        this.percolateUp(this.indices.get(n));
    }

    /**
     * Inserts a given element in the heap.
     * @param n the element
     */
    public void insert(final int n) {
        this.indices.growTo(n + 1, -1);
        assert !this.inHeap(n);
        this.indices.set(n, this.heap.size());
        this.heap.push(n);
        this.percolateUp(this.indices.get(n));
    }

    /**
     * Removes the minimal element of the heap.
     * @return the minimal element of the heap
     */
    public int removeMin() {
        final int x = this.heap.get(0);
        this.heap.set(0, this.heap.back());
        this.indices.set(this.heap.get(0), 0);
        this.indices.set(x, -1);
        this.heap.pop();
        if (this.heap.size() > 1) {
            this.percolateDown(0);
        }
        return x;
    }

    /**
     * Removes a given element of the heap.
     * @param n the element
     */
    public void remove(final int n) {
        assert this.inHeap(n);
        final int kPos = this.indices.get(n);
        this.indices.set(n, -1);
        if (kPos < this.heap.size() - 1) {
            this.heap.set(kPos, this.heap.back());
            this.indices.set(this.heap.get(kPos), kPos);
            this.heap.pop();
            this.percolateDown(kPos);
        } else {
            this.heap.pop();
        }
    }

    /**
     * Rebuilds the heap from a given vector of elements.
     * @param ns the vector of elements
     */
    public void build(final LNGIntVector ns) {
        for (int i = 0; i < this.heap.size(); i++) {
            this.indices.set(this.heap.get(i), -1);
        }
        this.heap.clear();
        for (int i = 0; i < ns.size(); i++) {
            this.indices.set(ns.get(i), i);
            this.heap.push(ns.get(i));
        }
        for (int i = this.heap.size() / 2 - 1; i >= 0; i--) {
            this.percolateDown(i);
        }
    }

    /**
     * Clears the heap.
     */
    public void clear() {
        for (int i = 0; i < this.heap.size(); i++) {
            this.indices.set(this.heap.get(i), -1);
        }
        this.heap.clear();
    }

    /**
     * Bubbles a element at a given position up.
     * @param pos the position
     */
    private void percolateUp(final int pos) {
        final int x = this.heap.get(pos);
        int p = parent(pos);
        int j = pos;
        while (j != 0 && this.s.lt(x, this.heap.get(p))) {
            this.heap.set(j, this.heap.get(p));
            this.indices.set(this.heap.get(p), j);
            j = p;
            p = parent(p);
        }
        this.heap.set(j, x);
        this.indices.set(x, j);
    }

    /**
     * Bubbles a element at a given position down.
     * @param pos the position
     */
    private void percolateDown(final int pos) {
        int p = pos;
        final int y = this.heap.get(p);
        while (left(p) < this.heap.size()) {
            final int child = right(p) < this.heap.size() && this.s.lt(this.heap.get(right(p)), this.heap.get(left(p))) ? right(p) : left(p);
            if (!this.s.lt(this.heap.get(child), y)) {
                break;
            }
            this.heap.set(p, this.heap.get(child));
            this.indices.set(this.heap.get(p), p);
            p = child;
        }
        this.heap.set(p, y);
        this.indices.set(y, p);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("LNGHeap{");
        for (int i = 0; i < this.heap.size(); i++) {
            sb.append("[").append(this.heap.get(i)).append(", ");
            sb.append(this.indices.get(i)).append("]");
            if (i != this.heap.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
}
