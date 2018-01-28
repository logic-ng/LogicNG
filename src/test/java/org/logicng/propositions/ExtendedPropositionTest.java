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
import java.util.Collections;
import java.util.Objects;

/**
 * Unit tests for {@link ExtendedProposition}.
 * @version 1.1
 * @since 1.0
 */
public class ExtendedPropositionTest {

  private final PropositionalParser p;
  private final ExtendedProposition<BagPack> prop1;
  private final ExtendedProposition<BagPack> prop2;
  private final ExtendedProposition<BagPack> prop3;
  private final ExtendedProposition<BagPack> prop4;

  public ExtendedPropositionTest() throws ParserException {
    FormulaFactory f = new FormulaFactory();
    p = new PropositionalParser(f);
    prop1 = new ExtendedProposition<>(new BagPack("prop1"), p.parse("a & b"));
    prop2 = new ExtendedProposition<>(new BagPack("prop2"), Arrays.asList(p.parse("a & b"), p.parse("~c")));
    prop3 = new ExtendedProposition<>(new BagPack("prop3"), p.parse("a & b"), p.parse("~c"));
    prop4 = new ExtendedProposition<>(new BagPack("prop4"), new ImmutableFormulaList(p.parse("a & b"), p.parse("~c")));
  }

  @Test
  public void testGetters() throws ParserException {
    final ImmutableFormulaList list1 = new ImmutableFormulaList(FType.AND, p.parse("a & b"));
    final ImmutableFormulaList list2 = new ImmutableFormulaList(FType.AND, p.parse("a & b"), p.parse("~c"));
    Assert.assertEquals(list1, prop1.formulas());
    Assert.assertEquals(list2, prop2.formulas());
    Assert.assertEquals(list2, prop3.formulas());
    Assert.assertEquals(list2, prop4.formulas());

    Assert.assertEquals("prop1", prop1.bagback().description);
    Assert.assertEquals("prop2", prop2.bagback().description);
    Assert.assertEquals("prop3", prop3.bagback().description);
    Assert.assertEquals("prop4", prop4.bagback().description);
  }

  @Test
  public void testFormula() throws ParserException {
    FormulaFactory f = new FormulaFactory();

    Assert.assertEquals(p.parse("a & b"), prop1.formula(f));
    Assert.assertEquals(p.parse("a & b & ~c"), prop3.formula(f));
  }

  @Test
  public void testHashCode() throws ParserException {
    ExtendedProposition<BagPack> prop11 = new ExtendedProposition<>(new BagPack("prop1"), p.parse("a & b"));
    ExtendedProposition<BagPack> prop21 = new ExtendedProposition<>(new BagPack("prop2"), Arrays.asList(p.parse("a & b"), p.parse("~c")));
    Assert.assertEquals(prop1.hashCode(), prop1.hashCode());
    Assert.assertEquals(prop1.hashCode(), prop11.hashCode());
    Assert.assertEquals(prop2.hashCode(), prop21.hashCode());
  }

  @Test
  public void testEquals() throws ParserException {
    ExtendedProposition<BagPack> prop11 = new ExtendedProposition<>(new BagPack("prop1"), p.parse("a & b"));
    ExtendedProposition<BagPack> prop21 = new ExtendedProposition<>(new BagPack("prop2"), Arrays.asList(p.parse("a & b"), p.parse("~c")));
    ExtendedProposition<BagPack> prop31 = new ExtendedProposition<>(new BagPack("prop3"), Collections.singletonList(p.parse("a & b")));
    Assert.assertTrue(prop1.equals(prop1));
    Assert.assertTrue(prop1.equals(prop11));
    Assert.assertTrue(prop2.equals(prop21));
    Assert.assertFalse(prop1.equals(prop2));
    Assert.assertFalse(prop3.equals(prop31));
    Assert.assertFalse(prop3.equals(prop4));
    Assert.assertFalse(prop1.equals(null));
    Assert.assertFalse(prop1.equals("String"));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("ExtendedProposition{formulas=AND[a & b], bagpack=prop1}", prop1.toString());
    Assert.assertEquals("ExtendedProposition{formulas=AND[a & b, ~c], bagpack=prop2}", prop2.toString());
    Assert.assertEquals("ExtendedProposition{formulas=AND[a & b, ~c], bagpack=prop3}", prop3.toString());
    Assert.assertEquals("ExtendedProposition{formulas=AND[a & b, ~c], bagpack=prop4}", prop4.toString());
  }

  private static final class BagPack implements PropositionBagpack {
    private final String description;

    private BagPack(final String description) {
      this.description = description;
    }

    @Override
    public int hashCode() {
      return description.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
      return obj instanceof BagPack && Objects.equals(this.description, ((BagPack) obj).description);
    }

    @Override
    public String toString() {
      return description;
    }
  }
}
