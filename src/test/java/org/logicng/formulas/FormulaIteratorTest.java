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

package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Tests for the formula iterators.
 * @version 1.0
 * @since 1.0
 */
public class FormulaIteratorTest {

  private final FormulaFactory f = new FormulaFactory();
  private final PropositionalParser p = new PropositionalParser(f);

  @Test
  public void testTrue() {
    Iterator<Formula> it = f.verum().iterator();
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testTrueNSE() {
    f.verum().iterator().next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testTrueUSO() {
    f.verum().iterator().remove();
  }

  @Test
  public void testFalse() {
    Iterator<Formula> it = f.falsum().iterator();
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testFalseNSE() {
    f.falsum().iterator().next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testFalseUSO() {
    f.falsum().iterator().remove();
  }

  @Test
  public void testLiteral() {
    Iterator<Formula> it = f.variable("a").iterator();
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testLiteralNSE() {
    f.variable("a").iterator().next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testLiteralUSO() {
    f.variable("a").iterator().remove();
  }

  @Test
  public void testNot() throws ParserException {
    Iterator<Formula> it = p.parse("~(a & (b | c))").iterator();
    Assert.assertTrue(it.hasNext());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("a & (b | c)"), it.next());
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testNotNSE() throws ParserException {
    Iterator<Formula> it = p.parse("~(a & (b | c))").iterator();
    it.next();
    it.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testNotUSO() throws ParserException {
    Iterator<Formula> it = p.parse("~(a & (b | c))").iterator();
    it.next();
    it.remove();
  }

  @Test
  public void testImplication() throws ParserException {
    Iterator<Formula> it = p.parse("a => c | d").iterator();
    Assert.assertTrue(it.hasNext());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("a"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("c | d"), it.next());
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testImplicationNSE() throws ParserException {
    Iterator<Formula> it = p.parse("a => c | d").iterator();
    it.next();
    it.next();
    it.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testImplicationUSO() throws ParserException {
    Iterator<Formula> it = p.parse("a => c | d").iterator();
    it.next();
    it.remove();
  }

  @Test
  public void testEquivalence() throws ParserException {
    Iterator<Formula> it = p.parse("a <=> c | d").iterator();
    Assert.assertTrue(it.hasNext());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("a"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("c | d"), it.next());
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testEquivalenceNSE() throws ParserException {
    Iterator<Formula> it = p.parse("a <=> c | d").iterator();
    it.next();
    it.next();
    it.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testEquivalenceUSO() throws ParserException {
    Iterator<Formula> it = p.parse("a <=> c | d").iterator();
    it.next();
    it.remove();
  }

  @Test
  public void testAnd() throws ParserException {
    Iterator<Formula> it = p.parse("a & (c | d) & ~e").iterator();
    Assert.assertTrue(it.hasNext());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("a"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("c | d"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("~e"), it.next());
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testAndNSE() throws ParserException {
    Iterator<Formula> it = p.parse("a & (c | d) & ~e").iterator();
    it.next();
    it.next();
    it.next();
    it.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAndUSO() throws ParserException {
    Iterator<Formula> it = p.parse("a & (c | d) & ~e").iterator();
    it.next();
    it.remove();
  }

  @Test
  public void testOr() throws ParserException {
    Iterator<Formula> it = p.parse("a | (c & d) | ~e").iterator();
    Assert.assertTrue(it.hasNext());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("a"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("c & d"), it.next());
    Assert.assertTrue(it.hasNext());
    Assert.assertEquals(p.parse("~e"), it.next());
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testOrNSE() throws ParserException {
    Iterator<Formula> it = p.parse("a | (c & d) | ~e").iterator();
    it.next();
    it.next();
    it.next();
    it.next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testOrUSO() throws ParserException {
    Iterator<Formula> it = p.parse("a | (c & d) | ~e").iterator();
    it.next();
    it.remove();
  }

  @Test
  public void testPBC() throws ParserException {
    final Formula pb1 = new PseudoBooleanParser(f).parse("3*a + 4*b + 5*c <= 8");
    Iterator<Formula> it = pb1.iterator();
    Assert.assertFalse(it.hasNext());
    Assert.assertFalse(it.hasNext());
  }

  @Test(expected = NoSuchElementException.class)
  public void testPBCNSE() throws ParserException {
    final Formula pb1 = new PseudoBooleanParser(f).parse("3*a + 4*b + 5*c <= 8");
    pb1.iterator().next();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testPBCUSO() throws ParserException {
    final Formula pb1 = new PseudoBooleanParser(f).parse("3*a + 4*b + 5*c <= 8");
    pb1.iterator().remove();
  }


}
