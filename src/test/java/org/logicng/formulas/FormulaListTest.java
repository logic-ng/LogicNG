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
//  Copyright 2015 Christoph Zengler                                     //
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
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.collections.LNGVector;
import org.logicng.io.parser.ParserException;
import org.logicng.io.parser.PropositionalParser;
import org.logicng.datastructures.Substitution;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.TreeMap;

import static org.logicng.formulas.FType.AND;
import static org.logicng.formulas.FType.OR;

/**
 * Unit tests for {@link ImmutableFormulaList}.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class FormulaListTest {

  private FormulaFactory f = new FormulaFactory();

  @Test
  public void testConstructors() {
    ImmutableFormulaList l1 = new ImmutableFormulaList(AND);
    Assert.assertTrue(l1.empty());
    Assert.assertEquals(0, l1.size());
    ImmutableFormulaList l2 = new ImmutableFormulaList(OR, f.literal("a"));
    Assert.assertFalse(l2.empty());
    Assert.assertEquals(1, l2.size());
    Assert.assertEquals(f.literal("a"), l2.get(0));
    ImmutableFormulaList l3 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    Assert.assertFalse(l3.empty());
    Assert.assertEquals(3, l3.size());
    Assert.assertEquals(f.literal("a"), l3.get(0));
    Assert.assertEquals(f.literal("b"), l3.get(1));
    Assert.assertEquals(f.literal("c", false), l3.get(2));
    ImmutableFormulaList l4 = new ImmutableFormulaList(Arrays.asList(f.literal("a"), f.literal("b"), f.literal("c", false)));
    Assert.assertFalse(l4.empty());
    Assert.assertEquals(3, l4.size());
    Assert.assertEquals(f.literal("a"), l4.get(0));
    Assert.assertEquals(f.literal("b"), l4.get(1));
    Assert.assertEquals(f.literal("c", false), l4.get(2));
    ImmutableFormulaList l5 = new ImmutableFormulaList(new LNGVector<>(f.literal("a"), f.literal("b"), f.literal("c", false)));
    Assert.assertFalse(l5.empty());
    Assert.assertEquals(3, l5.size());
    Assert.assertEquals(f.literal("a"), l5.get(0));
    Assert.assertEquals(f.literal("b"), l5.get(1));
    Assert.assertEquals(f.literal("c", false), l5.get(2));
    ImmutableFormulaList l6 = new ImmutableFormulaList(OR, Arrays.asList(f.literal("a"), f.literal("b"), f.literal("c", false)));
    Assert.assertFalse(l6.empty());
    Assert.assertEquals(3, l6.size());
    Assert.assertEquals(f.literal("a"), l6.get(0));
    Assert.assertEquals(f.literal("b"), l6.get(1));
    Assert.assertEquals(f.literal("c", false), l6.get(2));
    ImmutableFormulaList l7 = new ImmutableFormulaList(AND, new LNGVector<>(f.literal("a"), f.literal("b"), f.literal("c", false)));
    Assert.assertEquals(3, l7.size());
    Assert.assertEquals(f.literal("a"), l7.get(0));
    Assert.assertEquals(f.literal("b"), l7.get(1));
    Assert.assertEquals(f.literal("c", false), l7.get(2));
    Assert.assertEquals(FType.AND, l1.operator());
    Assert.assertEquals(FType.OR, l2.operator());
    Assert.assertEquals(FType.NONE, l3.operator());
    Assert.assertEquals(FType.NONE, l4.operator());
    Assert.assertEquals(FType.NONE, l5.operator());
    Assert.assertEquals(FType.OR, l6.operator());
    Assert.assertEquals(FType.AND, l7.operator());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalGet1() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    l.get(-2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalGet2() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    l.get(3);
  }

  @Test
  public void testFormula() throws ParserException {
    Assert.assertEquals(f.falsum(), new ImmutableFormulaList(OR).formula(f));
    Assert.assertEquals(f.verum(), new ImmutableFormulaList(AND).formula(f));
    Assert.assertEquals(f.literal("a"), new ImmutableFormulaList(OR, f.literal("a")).formula(f));
    Assert.assertEquals(f.literal("a"), new ImmutableFormulaList(AND, f.literal("a")).formula(f));
    ImmutableFormulaList l1 = new ImmutableFormulaList(AND, f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l2 = new ImmutableFormulaList(AND, f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    ImmutableFormulaList l3 = new ImmutableFormulaList(OR, f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l4 = new ImmutableFormulaList(OR, f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    PropositionalParser parser = new PropositionalParser(f);
    Assert.assertEquals(parser.parse("a & b & ~c"), l1.formula(f));
    Assert.assertEquals(parser.parse("(a | b) & ~c"), l2.formula(f));
    Assert.assertEquals(parser.parse("a | b | ~c"), l3.formula(f));
    Assert.assertEquals(parser.parse("a | b | ~c"), l4.formula(f));
  }

  @Test(expected = IllegalStateException.class)
  public void testIllegalFormula() {
    new ImmutableFormulaList().formula(f);
  }

  @Test
  public void testContains() {
    ImmutableFormulaList l1 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l2 = new ImmutableFormulaList(f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    Assert.assertTrue(l1.contains(f.literal("a")));
    Assert.assertTrue(l1.contains(f.literal("b")));
    Assert.assertTrue(l1.contains(f.literal("c", false)));
    Assert.assertFalse(l1.contains(f.literal("c")));
    Assert.assertFalse(l1.contains(f.literal("d")));
    Assert.assertFalse(l2.contains(f.literal("a")));
    Assert.assertFalse(l2.contains(f.literal("b")));
    Assert.assertTrue(l2.contains(f.literal("c", false)));
    Assert.assertTrue(l2.contains(f.or(f.literal("a"), f.literal("b"))));
  }

  @Test
  public void testVariables() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    Assert.assertEquals(3, l.variables().size());
    Assert.assertTrue(l.variables().contains(f.literal("a")));
    Assert.assertTrue(l.variables().contains(f.literal("b")));
    Assert.assertTrue(l.variables().contains(f.literal("c")));
  }

  @Test
  public void testLiterals() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    Assert.assertEquals(4, l.literals().size());
    Assert.assertTrue(l.literals().contains(f.literal("a")));
    Assert.assertTrue(l.literals().contains(f.literal("b")));
    Assert.assertTrue(l.literals().contains(f.literal("c")));
    Assert.assertTrue(l.literals().contains(f.literal("c", false)));
  }

  @Test
  public void testVarProfile() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    SortedMap<Literal, Integer> profile = new TreeMap<>();
    profile.put(f.literal("a"), 2);
    profile.put(f.literal("b"), 2);
    profile.put(f.literal("c"), 2);
    Assert.assertEquals(profile, l.varProfile());
  }

  @Test
  public void testLitProfile() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    SortedMap<Literal, Integer> profile = new TreeMap<>();
    profile.put(f.literal("a"), 2);
    profile.put(f.literal("b"), 2);
    profile.put(f.literal("c"), 1);
    profile.put(f.literal("c", false), 1);
    Assert.assertEquals(profile, l.litProfile());
  }

  @Test
  public void testSubstitution() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    Substitution subst = new Substitution();
    subst.addMapping(f.literal("a"), f.literal("d"));
    subst.addMapping(f.literal("c"), f.and(f.literal("e"), f.literal("f")));
    ImmutableFormulaList substL = l.substitute(subst);
    Assert.assertEquals(5, substL.size());
    Assert.assertTrue(substL.contains(f.literal("d")));
    Assert.assertTrue(substL.contains(f.literal("b")));
    Assert.assertTrue(substL.contains(f.and(f.literal("e"), f.literal("f"))));
    Assert.assertTrue(substL.contains(f.or(f.literal("d"), f.literal("b"))));
    Assert.assertTrue(substL.contains(f.not(f.and(f.literal("e"), f.literal("f")))));
  }

  @Test
  public void testToArray() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c"));
    Formula[] expected = new Formula[]{f.literal("a"), f.literal("b"), f.literal("c", false),
            f.or(f.literal("a"), f.literal("b")), f.literal("c")};
    Assert.assertArrayEquals(expected, l.toArray());
  }

  @Test
  public void testHash() {
    ImmutableFormulaList l11 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l12 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l21 = new ImmutableFormulaList(f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    ImmutableFormulaList l22 = new ImmutableFormulaList(f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    Assert.assertEquals(new ImmutableFormulaList().hashCode(), new ImmutableFormulaList().hashCode());
    Assert.assertEquals(l11.hashCode(), l12.hashCode());
    Assert.assertEquals(l21.hashCode(), l22.hashCode());
  }

  @Test
  public void testEquals() {
    ImmutableFormulaList l11 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l12 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l13 = new ImmutableFormulaList(AND, f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l21 = new ImmutableFormulaList(f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    ImmutableFormulaList l22 = new ImmutableFormulaList(f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    Assert.assertEquals(new ImmutableFormulaList(), new ImmutableFormulaList());
    Assert.assertEquals(l11, l12);
    Assert.assertEquals(l21, l22);
    Assert.assertNotEquals(l11, l13);
    Assert.assertNotEquals(l11, l21);
    Assert.assertNotEquals(l21, l11);
    Assert.assertNotEquals(null, l11);
    Assert.assertNotEquals(l11, null);
    Assert.assertNotEquals("String", l11);
    Assert.assertNotEquals(l11, "String");
  }

  @Test
  public void testToString() {
    ImmutableFormulaList l1 = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    ImmutableFormulaList l2 = new ImmutableFormulaList(AND, f.or(f.literal("a"), f.literal("b")), f.literal("c", false));
    Assert.assertEquals("NONE[a, b, ~c]", l1.toString());
    Assert.assertEquals("AND[a | b, ~c]", l2.toString());
  }

  @Test
  public void testIterator() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    Formula[] array = l.toArray();
    int count = 0;
    for (final Formula formula : l)
      Assert.assertEquals(array[count++], formula);
    Iterator<Formula> it = l.iterator();
    Assert.assertTrue(it.hasNext());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorIllegalRemove() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    Iterator<Formula> it = l.iterator();
    it.remove();
  }

  @Test(expected = NoSuchElementException.class)
  public void testIteratorIteratorOverflow() {
    ImmutableFormulaList l = new ImmutableFormulaList(f.literal("a"), f.literal("b"), f.literal("c", false));
    Iterator<Formula> it = l.iterator();
    it.next();
    it.next();
    it.next();
    it.next();
  }
}
