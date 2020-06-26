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

package org.logicng.solvers.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;

/**
 * Unit tests for the toString() methods of the solver data structures.
 * @version 2.0.0
 * @since 1.0
 */
public class SolversDatastructuresTest {

    @Test
    public void testLNGBoundedIntQueue() {
        final LNGBoundedIntQueue queue = new LNGBoundedIntQueue();
        queue.initSize(2);
        queue.push(64);
        queue.push(32);
        queue.push(8);
        queue.push(16);
        final String expected = "LNGBoundedIntQueue{first=0, last=0, sumOfQueue=24, maxSize=2, queueSize=2, elems=[8, 16]}";
        assertThat(queue.toString()).isEqualTo(expected);
    }

    @Test
    public void testLNGBoundedLongQueue() {
        final LNGBoundedLongQueue queue = new LNGBoundedLongQueue();
        queue.initSize(2);
        queue.push(64L);
        queue.push(32L);
        queue.push(8L);
        queue.push(17L);
        final String expected = "LNGBoundedLongQueue{first=0, last=0, sumOfQueue=25, maxSize=2, queueSize=2, elems=[8, 17]}";
        assertThat(queue.toString()).isEqualTo(expected);
    }

    @Test
    public void testMSClause() {
        final LNGIntVector vec = new LNGIntVector();
        vec.push(2);
        vec.push(4);
        vec.push(6);
        final MSClause clause = new MSClause(vec, true);
        clause.setCanBeDel(true);
        clause.setLBD(42);
        clause.setSeen(true);
        final String expected = "MSClause{activity=0.0, learnt=true, szWithoutSelectors=0, seen=true, lbd=42, canBeDel=true, oneWatched=false, isAtMost=false, atMostWatchers=-1, lits=[1, 2, 3]}";
        assertThat(clause.toString()).isEqualTo(expected);
        assertThat(clause.equals(clause)).isTrue();
        assertThat(clause.hashCode()).isEqualTo(clause.hashCode());
        assertThat(clause.equals("Test")).isFalse();
    }

    @Test
    public void testMSHardClause() {
        final LNGIntVector vec = new LNGIntVector();
        vec.push(2);
        vec.push(4);
        vec.push(6);
        final MSHardClause clause = new MSHardClause(vec);
        final String expected = "MSHardClause{lits=[1, 2, 3]}";
        assertThat(clause.toString()).isEqualTo(expected);
    }

    @Test
    public void testMSSoftClause() {
        final LNGIntVector vec = new LNGIntVector();
        vec.push(2);
        vec.push(4);
        vec.push(6);
        final MSSoftClause clause = new MSSoftClause(vec, 2, 4, vec);
        final String expected = "MSSoftClause{weight=2, assumption=4 lits=[1, 2, 3] relax[1, 2, 3]}";
        assertThat(clause.toString()).isEqualTo(expected);
    }

    @Test
    public void testMSVariable() {
        final MSVariable var = new MSVariable(true);
        var.setDecision(true);
        var.setLevel(12);
        var.setReason(null);
        var.assign(Tristate.TRUE);
        final String expected = "MSVariable{assignment=TRUE, level=12, reason=null, activity=0.000000, polarity=true, decision=true}";
        assertThat(var.toString()).isEqualTo(expected);
    }

    @Test
    public void testMSWatcher() {
        final LNGIntVector vec = new LNGIntVector();
        vec.push(2);
        vec.push(4);
        vec.push(6);
        final MSClause clause = new MSClause(vec, true);
        final MSWatcher watcher = new MSWatcher(clause, 2);
        final String expected = "MSWatcher{clause=MSClause{activity=0.0, learnt=true, szWithoutSelectors=0, seen=false, lbd=0, canBeDel=true, oneWatched=false, isAtMost=false, atMostWatchers=-1, lits=[1, 2, 3]}, blocker=2}";
        assertThat(watcher.toString()).isEqualTo(expected);
        assertThat(watcher.hashCode()).isEqualTo(watcher.hashCode());
    }
}
