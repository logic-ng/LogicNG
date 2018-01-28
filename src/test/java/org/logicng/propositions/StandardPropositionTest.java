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

package org.logicng.propositions;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.FType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for {@link StandardProposition}.
 * @version 1.1
 * @since 1.0
 */
public class StandardPropositionTest {

  private final PropositionalParser p;
  private final StandardProposition prop1;
  private final StandardProposition prop2;
  private final StandardProposition prop3;
  private final StandardProposition prop4;
  private final StandardProposition prop5;
  private final StandardProposition prop6;

  public StandardPropositionTest() throws ParserException {
    FormulaFactory f = new FormulaFactory();
    p = new PropositionalParser(f);
    prop1 = new StandardProposition(p.parse("a & b"));
    prop2 = new StandardProposition("prop2", p.parse("a & b"));
    prop3 = new StandardProposition("prop3", Arrays.asList(p.parse("a & b"), p.parse("~c")));
    prop4 = new StandardProposition("prop4", p.parse("a & b"), p.parse("~c"));
    prop5 = new StandardProposition("prop5", new ImmutableFormulaList(p.parse("a & b"), p.parse("~c")));
    prop6 = new StandardProposition(null, p.parse("a & b"));
  }

  @Test
  public void testGetters() throws ParserException {
    final ImmutableFormulaList list1 = new ImmutableFormulaList(FType.AND, p.parse("a & b"));
    final ImmutableFormulaList list2 = new ImmutableFormulaList(FType.AND, p.parse("a & b"), p.parse("~c"));
    Assert.assertEquals(list1, prop1.formulas());
    Assert.assertEquals(list1, prop2.formulas());
    Assert.assertEquals(list2, prop3.formulas());
    Assert.assertEquals(list2, prop4.formulas());
    Assert.assertEquals(list2, prop5.formulas());

    Assert.assertEquals("", prop1.description());
    Assert.assertEquals("prop2", prop2.description());
    Assert.assertEquals("prop3", prop3.description());
    Assert.assertEquals("prop4", prop4.description());
    Assert.assertEquals("prop5", prop5.description());
    Assert.assertEquals("", prop6.description());
  }

  @Test
  public void testFormula() throws ParserException {
    FormulaFactory f = new FormulaFactory();

    Assert.assertEquals(p.parse("a & b"), prop1.formula(f));
    Assert.assertEquals(p.parse("a & b & ~c"), prop3.formula(f));
  }

  @Test
  public void testHashCode() throws ParserException {
    StandardProposition prop11 = new StandardProposition(p.parse("a & b"));
    StandardProposition prop21 = new StandardProposition("prop2", p.parse("a & b"));
    Assert.assertEquals(prop1.hashCode(), prop1.hashCode());
    Assert.assertEquals(prop1.hashCode(), prop11.hashCode());
    Assert.assertEquals(prop2.hashCode(), prop21.hashCode());
  }

  @Test
  public void testEquals() throws ParserException {
    StandardProposition prop11 = new StandardProposition(p.parse("a & b"));
    StandardProposition prop21 = new StandardProposition("prop2", p.parse("a & b"));
    Assert.assertTrue(prop1.equals(prop1));
    Assert.assertTrue(prop1.equals(prop11));
    Assert.assertTrue(prop2.equals(prop21));
    Assert.assertFalse(prop1.equals(prop2));
    Assert.assertFalse(prop1.equals(null));
    Assert.assertFalse(prop1.equals("String"));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("StandardProposition{formulas=AND[a & b], description=}", prop1.toString());
    Assert.assertEquals("StandardProposition{formulas=AND[a & b], description=prop2}", prop2.toString());
    Assert.assertEquals("StandardProposition{formulas=AND[a & b, ~c], description=prop3}", prop3.toString());
    Assert.assertEquals("StandardProposition{formulas=AND[a & b, ~c], description=prop4}", prop4.toString());
    Assert.assertEquals("StandardProposition{formulas=AND[a & b, ~c], description=prop5}", prop5.toString());
  }
}
