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
 * Copyright (C) 2012 - 2014 Armin Biere JKU Linz
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package org.logicng.collections;

/**
 * A simple priority queue implementation for elements with long priorities taken from CleaneLing.
 * @version 1.3
 * @since 1.0
 */
public final class LNGLongPriorityQueue {

    private final LNGIntVector heap;
    private final LNGLongVector prior;
    private final LNGIntVector pos;

    /**
     * Creates a new priority queue.
     */
    public LNGLongPriorityQueue() {
        this.heap = new LNGIntVector();
        this.prior = new LNGLongVector();
        this.pos = new LNGIntVector();
    }

    /**
     * Returns the left position on the heap for a given position.
     * @param position the position
     * @return the left position
     */
    private static int left(final int position) {
        return 2 * position + 1;
    }

    /**
     * Returns the right position on the heap for a given position.
     * @param position the position
     * @return the right position
     */
    private static int right(final int position) {
        return 2 * position + 2;
    }

    /**
     * Returns the parent position on the heap for a given position.
     * @param position the position
     * @return the parent position
     */
    private static int parent(final int position) {
        assert position > 0;
        return (position - 1) / 2;
    }

    /**
     * Returns whether the queue is empty or not.
     * @return {@code true} if the queue is empty, {@code false} otherwise
     */
    public boolean empty() {
        return this.heap.empty();
    }

    /**
     * Returns the size of the queue.
     * @return the size of the queue
     */
    public int size() {
        return this.heap.size();
    }

    /**
     * Returns whether a given element is already imported and present in the queue or not.
     * @param element the element
     * @return {@code true} if the element is already imported and present in the queue, {@code false otherwise}.
     */
    public boolean contains(final int element) {
        return element >= 0 && this.imported(element) && this.pos.get(Math.abs(element)) >= 0;
    }

    /**
     * Returns the priority for a given element.
     * @param element the element
     * @return the priority of the element
     */
    public long priority(final int element) {
        assert this.imported(element);
        return this.prior.get(Math.abs(element));
    }

    /**
     * Returns the top element of the priority queue (= the element with the largest priority).
     * @return the top element of the priority queue
     */
    public int top() {
        return this.heap.get(0);
    }

    /**
     * Pushes a new element to the queue.
     * @param element the element
     * @throws IllegalArgumentException if the element to add is negative
     */
    public void push(final int element) {
        if (element < 0) {
            throw new IllegalArgumentException("Cannot add negative integers to the priority queue");
        }
        assert !this.contains(element);
        this.doImport(element);
        this.pos.set(element, this.heap.size());
        this.heap.push(element);
        assert this.heap.get(this.pos.get(element)) == element;
        this.up(element);
    }

    /**
     * Updated the priority of a given element.
     * @param element  the element
     * @param priority the new priority
     */
    public void update(final int element, final long priority) {
        this.doImport(element);
        final long q = this.prior.get(element);
        if (q == priority) {
            return;
        }
        this.prior.set(element, priority);
        if (this.pos.get(element) < 0) {
            return;
        }
        if (priority < q) {
            this.down(element);
        }
        if (q < priority) {
            this.up(element);
        }
    }

    /**
     * Removes a given element from the priority queue.  Its priority is kept as is.
     * @param element the element
     */
    public void pop(final int element) {
        assert this.contains(element);
        final int i = this.pos.get(element);
        this.pos.set(element, -1);
        final int last = this.heap.back();
        this.heap.pop();
        final int j = this.heap.size();
        if (i == j) {
            return;
        }
        assert i < j;
        this.pos.set(last, i);
        this.heap.set(i, last);
        this.up(last);
        this.down(last);
    }

    /**
     * Removes the top element from the priority queue.
     */
    public void pop() {
        this.pop(this.top());
    }

    /**
     * Compares two elements by their priority and returns whether the first element's priority is less then the second
     * element's priority.
     * @param e1 the first element
     * @param e2 the second element
     * @return {@code true} if the priority of the first element is less than the priority of the second element
     */
    private boolean less(final int e1, final int e2) {
        return this.prior.get(e1) < this.prior.get(e2);
    }

    /**
     * Bubbles a given element up.
     * @param element the element
     */
    private void up(final int element) {
        int epos = this.pos.get(element);
        while (epos > 0) {
            final int ppos = parent(epos);
            final int p = this.heap.get(ppos);
            if (!this.less(p, element)) {
                break;
            }
            this.heap.set(epos, p);
            this.heap.set(ppos, element);
            this.pos.set(p, epos);
            epos = ppos;
        }
        this.pos.set(element, epos);
    }

    /**
     * Bubbles a given element down.
     * @param element the element
     */
    private void down(final int element) {
        assert this.contains(element);
        int epos = this.pos.get(element);
        final int size = this.heap.size();
        while (true) {
            int cpos = left(epos);
            if (cpos >= size) {
                break;
            }
            int c = this.heap.get(cpos);
            final int o;
            final int opos = right(epos);
            if (!this.less(element, c)) {
                if (opos >= size) {
                    break;
                }
                o = this.heap.get(opos);
                if (!this.less(element, o)) {
                    break;
                }
                cpos = opos;
                c = o;
            } else if (opos < size) {
                o = this.heap.get(opos);
                if (!this.less(o, c)) {
                    cpos = opos;
                    c = o;
                }
            }
            this.heap.set(cpos, element);
            this.heap.set(epos, c);
            this.pos.set(c, epos);
            epos = cpos;
        }
        this.pos.set(element, epos);
    }

    /**
     * Returns whether a given element is already imported.
     * @param element the element
     * @return {@code true} if the element is imported, {@code false} otherwise
     */
    private boolean imported(final int element) {
        assert 0 <= element;
        return element < this.pos.size();
    }

    /**
     * Imports a given element.
     * @param element the element
     */
    private void doImport(final int element) {
        while (!this.imported(element)) {
            this.pos.push(-1);
            this.prior.push(0);
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("LNGLongPriorityQueue{");
        for (int i = 0; i < this.heap.size(); i++) {
            sb.append(String.format("<elem=%d, pos=%d, prio=%d>", this.heap.get(i), this.pos.get(i), this.prior.get(i)));
            if (i != this.heap.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
}

