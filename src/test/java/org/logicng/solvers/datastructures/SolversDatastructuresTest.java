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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;

import java.util.Arrays;

/**
 * Unit tests for the toString() methods of the solver data structures.
 * @version 1.1
 * @since 1.0
 */
public class SolversDatastructuresTest {

  @Test
  public void testCLClause() {
    final CLClause clause = new CLClause();
    clause.lits().push(1);
    clause.lits().push(2);
    clause.lits().push(3);
    clause.setActivity(42);
    clause.setRedundant(true);
    clause.setImportant(true);
    clause.setGlue(12);
    final String expected = "CLClause{glue=12, redundant=true, remove=false, important=true, forcing=false, dumped=false, satisfied=false, activity=42, lits=[1, 2, 3]}";
    Assert.assertEquals(expected, clause.toString());
  }

  @Test
  public void testCLFrame() {
    final CLFrame frame = new CLFrame(10, 12, 2);
    frame.setMark(true);
    final String expected = "CLFrame{decision=10, level=12, trail=2, mark=true}";
    Assert.assertEquals(expected, frame.toString());
  }

  @Test
  public void testCLOccs() {
    final CLClause clause1 = new CLClause();
    clause1.lits().push(1);
    clause1.lits().push(2);
    clause1.lits().push(3);
    final CLClause clause2 = new CLClause();
    clause2.lits().push(4);
    clause2.lits().push(5);
    final CLOccs occs = new CLOccs();
    occs.add(clause1);
    occs.add(clause2);
    final String expected = "CLOccs{count=2, clauses=[CLClause{glue=0, redundant=false, remove=false, important=false, forcing=false, dumped=false, satisfied=false, activity=0, lits=[1, 2, 3]}, CLClause{glue=0, redundant=false, remove=false, important=false, forcing=false, dumped=false, satisfied=false, activity=0, lits=[4, 5]}]}";
    Assert.assertEquals(expected, occs.toString());
  }

  @Test
  public void testCLVar() {
    final CLVar var = new CLVar();
    var.setLevel(12);
    var.setMark(1);
    var.setReason(null);
    var.setState(CLVar.State.ELIMINATED);
    final String expected = "CLVar{state=ELIMINATED, level=12, mark=1, reason=null}";
    Assert.assertEquals(expected, var.toString());
  }

  @Test
  public void testCLWatch() {
    final CLClause clause = new CLClause();
    clause.lits().push(1);
    clause.lits().push(2);
    final CLWatch watch = new CLWatch(12, true, clause);
    final String expected = "CLWatch{blit=12, binary=true, clause=CLClause{glue=0, redundant=false, remove=false, important=false, forcing=false, dumped=false, satisfied=false, activity=0, lits=[1, 2]}}";
    Assert.assertEquals(expected, watch.toString());
  }

  @Test
  public void testLNGBoundedIntQueue() {
    final LNGBoundedIntQueue queue = new LNGBoundedIntQueue();
    queue.initSize(2);
    queue.push(64);
    queue.push(32);
    queue.push(8);
    queue.push(16);
    final String expected = "LNGBoundedIntQueue{first=0, last=0, sumOfQueue=24, maxSize=2, queueSize=2, elems=[8, 16]}";
    Assert.assertEquals(expected, queue.toString());
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
    Assert.assertEquals(expected, queue.toString());
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
    Assert.assertEquals(expected, clause.toString());
    Assert.assertTrue(clause.equals(clause));
    Assert.assertEquals(clause.hashCode(), clause.hashCode());
    Assert.assertFalse(clause.equals("Test"));
  }

  @Test
  public void testMSHardClause() {
    final LNGIntVector vec = new LNGIntVector();
    vec.push(2);
    vec.push(4);
    vec.push(6);
    final MSHardClause clause = new MSHardClause(vec);
    final String expected = "MSHardClause{lits=[1, 2, 3]}";
    Assert.assertEquals(expected, clause.toString());
  }

  @Test
  public void testMSSoftClause() {
    final LNGIntVector vec = new LNGIntVector();
    vec.push(2);
    vec.push(4);
    vec.push(6);
    final MSSoftClause clause = new MSSoftClause(vec, 2, 4, vec);
    final String expected = "MSSoftClause{weight=2, assumption=4 lits=[1, 2, 3] relax[1, 2, 3]}";
    Assert.assertEquals(expected, clause.toString());
  }

  @Test
  public void testMSVariable() {
    final MSVariable var = new MSVariable(true);
    var.setDecision(true);
    var.setLevel(12);
    var.setReason(null);
    var.assign(Tristate.TRUE);
    final String expected = "MSVariable{assignment=TRUE, level=12, reason=null, activity=0.000000, polarity=true, decision=true}";
    Assert.assertEquals(expected, var.toString());
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
    Assert.assertEquals(expected, watcher.toString());
    Assert.assertEquals(watcher.hashCode(), watcher.hashCode());
  }

  @Test
  public void testCLVarState() {
    Assert.assertEquals(CLVar.State.ELIMINATED, CLVar.State.valueOf("ELIMINATED"));
    Assert.assertTrue(Arrays.asList(CLVar.State.values()).contains(CLVar.State.valueOf("FREE")));
  }

}
