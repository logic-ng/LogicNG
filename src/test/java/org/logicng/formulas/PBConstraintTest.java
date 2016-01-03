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
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.io.parsers.ParserException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link PBConstraint}.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class PBConstraintTest {

  private static final FormulaFactory f = new FormulaFactory();
  private static final FormulaFactory f2 = new FormulaFactory();

  private PBConstraint pb1;
  private PBConstraint pb2;
  private PBConstraint pb22;
  private PBConstraint cc1;
  private PBConstraint cc2;
  private PBConstraint amo1;
  private PBConstraint amo2;
  private PBConstraint exo1;
  private PBConstraint exo2;

  public PBConstraintTest() {
    final Literal[] lits1 = new Literal[]{f.literal("a")};
    final List<Literal> lits2 = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Literal> litsCC2 = Arrays.asList(f.literal("a"), f2.literal("b"), f.literal("c"));
    final int[] coeffs1 = new int[]{3};
    final List<Integer> coeffs2 = Arrays.asList(3, -2, 7);
    this.pb1 = f.pbc(CType.LE, 2, lits1, coeffs1);
    this.pb2 = f.pbc(CType.LE, 8, lits2, coeffs2);
    this.pb22 = f2.pbc(CType.LE, 8, lits2, coeffs2);
    this.cc1 = f.cc(CType.LT, 1, lits1);
    this.cc2 = f.cc(CType.GE, 2, litsCC2);
    this.amo1 = f.amo(lits1);
    this.amo2 = f.amo(litsCC2);
    this.exo1 = f.exo(lits1);
    this.exo2 = f.exo(litsCC2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalPB() {
    final List<Literal> lits2 = Arrays.asList(f.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Integer> coeffs2 = Arrays.asList(3, -2, 7, 2);
    f.pbc(CType.EQ, 3, lits2, coeffs2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC() {
    final List<Literal> lits2 = Arrays.asList(f.literal("a"), f2.literal("b", false), f.literal("c"));
    f.cc(CType.GE, 2, lits2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalAMO() {
    final List<Literal> lits2 = Arrays.asList(f.literal("a"), f.literal("b", false), f2.literal("c"));
    f.amo(lits2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalEXO() {
    final List<Literal> lits2 = Arrays.asList(f.literal("a"), f.literal("b", false), f.literal("c"));
    f.exo(lits2);
  }

  @Test
  public void testType() {
    Assert.assertEquals(FType.PBC, this.pb1.type());
    Assert.assertEquals(FType.PBC, this.pb2.type());
    Assert.assertEquals(FType.PBC, this.cc1.type());
    Assert.assertEquals(FType.PBC, this.cc2.type());
    Assert.assertEquals(FType.PBC, this.amo1.type());
    Assert.assertEquals(FType.PBC, this.amo2.type());
    Assert.assertEquals(FType.PBC, this.exo1.type());
    Assert.assertEquals(FType.PBC, this.exo2.type());
  }

  @Test
  public void testGetters() {
    final Literal[] lits1 = new Literal[]{f.literal("a")};
    final Literal[] lits2 = new Literal[]{f2.literal("a"), f.literal("b", false), f.literal("c")};
    final Literal[] litsCC2 = new Literal[]{f.literal("a"), f.literal("b"), f2.literal("c")};
    final int[] coeffs1 = new int[]{3};
    final int[] coeffs2 = new int[]{3, -2, 7};

    final int[] coeffsCC1 = new int[]{1};
    final int[] coeffsCC2 = new int[]{1, 1, 1};

    Assert.assertArrayEquals(lits1, pb1.operands());
    Assert.assertArrayEquals(coeffs1, pb1.coefficients());
    Assert.assertEquals(CType.LE, pb1.comparator());
    Assert.assertEquals(2, pb1.rhs());
    Assert.assertFalse(pb1.isCC());

    Assert.assertArrayEquals(lits2, pb2.operands());
    Assert.assertArrayEquals(coeffs2, pb2.coefficients());
    Assert.assertEquals(CType.LE, pb2.comparator());
    Assert.assertEquals(8, pb2.rhs());
    Assert.assertFalse(pb2.isCC());

    Assert.assertArrayEquals(lits1, cc1.operands());
    Assert.assertArrayEquals(coeffsCC1, cc1.coefficients());
    Assert.assertEquals(CType.LT, cc1.comparator());
    Assert.assertEquals(1, cc1.rhs());
    Assert.assertTrue(cc1.isCC());

    Assert.assertArrayEquals(litsCC2, cc2.operands());
    Assert.assertArrayEquals(coeffsCC2, cc2.coefficients());
    Assert.assertEquals(CType.GE, cc2.comparator());
    Assert.assertEquals(2, cc2.rhs());
    Assert.assertTrue(cc2.isCC());

    Assert.assertArrayEquals(lits1, amo1.operands());
    Assert.assertArrayEquals(coeffsCC1, amo1.coefficients());
    Assert.assertEquals(CType.LE, amo1.comparator());
    Assert.assertEquals(1, amo1.rhs());
    Assert.assertTrue(amo1.isCC());

    Assert.assertArrayEquals(litsCC2, amo2.operands());
    Assert.assertArrayEquals(coeffsCC2, amo2.coefficients());
    Assert.assertEquals(CType.LE, amo2.comparator());
    Assert.assertEquals(1, amo2.rhs());
    Assert.assertTrue(amo2.isCC());

    Assert.assertArrayEquals(lits1, exo1.operands());
    Assert.assertArrayEquals(coeffsCC1, exo1.coefficients());
    Assert.assertEquals(CType.EQ, exo1.comparator());
    Assert.assertEquals(1, exo1.rhs());
    Assert.assertTrue(exo1.isCC());

    Assert.assertArrayEquals(litsCC2, exo2.operands());
    Assert.assertArrayEquals(coeffsCC2, exo2.coefficients());
    Assert.assertEquals(CType.EQ, exo2.comparator());
    Assert.assertEquals(1, exo2.rhs());
    Assert.assertTrue(exo2.isCC());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(1, pb1.numberOfAtoms());
    Assert.assertEquals(1, pb2.numberOfAtoms());
    Assert.assertEquals(1, cc1.numberOfAtoms());
    Assert.assertEquals(1, cc2.numberOfAtoms());
    Assert.assertEquals(1, amo1.numberOfAtoms());
    Assert.assertEquals(1, amo2.numberOfAtoms());
    Assert.assertEquals(1, exo1.numberOfAtoms());
    Assert.assertEquals(1, exo2.numberOfAtoms());
    Assert.assertEquals(1, exo2.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(2, pb1.numberOfNodes());
    Assert.assertEquals(4, pb2.numberOfNodes());
    Assert.assertEquals(2, cc1.numberOfNodes());
    Assert.assertEquals(4, cc2.numberOfNodes());
    Assert.assertEquals(2, amo1.numberOfNodes());
    Assert.assertEquals(4, amo2.numberOfNodes());
    Assert.assertEquals(2, exo1.numberOfNodes());
    Assert.assertEquals(4, exo2.numberOfNodes());
    Assert.assertEquals(4, exo2.numberOfNodes());
  }

  @Test
  public void testVariables() {
    final SortedSet<Literal> lits1 = new TreeSet<>(Collections.singletonList(f.literal("a")));
    final SortedSet<Literal> lits2 = new TreeSet<>(Arrays.asList(f.literal("a"), f.literal("b"), f.literal("c")));
    Assert.assertEquals(lits1, pb1.variables());
    Assert.assertEquals(lits2, pb2.variables());
    Assert.assertEquals(lits1, cc1.variables());
    Assert.assertEquals(lits2, cc2.variables());
    Assert.assertEquals(lits1, amo1.variables());
    Assert.assertEquals(lits2, amo2.variables());
    Assert.assertEquals(lits1, exo1.variables());
    Assert.assertEquals(lits2, exo2.variables());
  }

  @Test
  public void testLiterals() {
    final SortedSet<Literal> lits1 = new TreeSet<>(Collections.singletonList(f.literal("a")));
    final SortedSet<Literal> lits2 = new TreeSet<>(Arrays.asList(f.literal("a"), f.literal("b", false), f.literal("c")));
    final SortedSet<Literal> litsCC2 = new TreeSet<>(Arrays.asList(f.literal("a"), f.literal("b"), f.literal("c")));
    Assert.assertEquals(lits1, pb1.literals());
    Assert.assertEquals(lits2, pb2.literals());
    Assert.assertEquals(lits1, cc1.literals());
    Assert.assertEquals(litsCC2, cc2.literals());
    Assert.assertEquals(lits1, amo1.literals());
    Assert.assertEquals(litsCC2, amo2.literals());
    Assert.assertEquals(lits1, exo1.literals());
    Assert.assertEquals(litsCC2, exo2.literals());
  }

  @Test
  public void testConstains() {
    Assert.assertTrue(pb1.contains(f.literal("a")));
    Assert.assertFalse(pb1.contains(f.literal("a", false)));
    Assert.assertTrue(pb2.contains(f.literal("b", false)));
    Assert.assertTrue(pb2.contains(f.literal("b")));
    Assert.assertFalse(pb2.contains(f.literal("d")));
  }

  @Test
  public void testEvaluate() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final Assignment a1 = new Assignment();
    a1.addLiteral(f.literal("a"));
    a1.addLiteral(f.literal("b"));
    a1.addLiteral(f.literal("c", false));
    final Assignment a2 = new Assignment();
    a2.addLiteral(f.literal("a"));
    a2.addLiteral(f.literal("b", false));
    a2.addLiteral(f.literal("c", false));
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    final PBConstraint pb3 = f.pbc(CType.GE, 1, lits, coeffs);
    final PBConstraint pb4 = f.pbc(CType.GT, 0, lits, coeffs);
    final PBConstraint pb5 = f.pbc(CType.LE, 1, lits, coeffs);
    final PBConstraint pb6 = f.pbc(CType.LT, 2, lits, coeffs);
    Assert.assertTrue(pb1.evaluate(a1));
    Assert.assertFalse(pb1.evaluate(a2));
    Assert.assertTrue(pb3.evaluate(a1));
    Assert.assertFalse(pb3.evaluate(a2));
    Assert.assertTrue(pb4.evaluate(a1));
    Assert.assertFalse(pb4.evaluate(a2));
    Assert.assertFalse(pb5.evaluate(a1));
    Assert.assertTrue(pb5.evaluate(a2));
    Assert.assertFalse(pb6.evaluate(a1));
    Assert.assertTrue(pb6.evaluate(a2));
  }

  @Test
  public void testRestrict() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Literal> litsA1 = Arrays.asList(f.literal("b", false), f.literal("c"));
    final List<Literal> litsA2 = Collections.singletonList(f.literal("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final List<Integer> coeffA1 = Arrays.asList(-2, 3);
    final List<Integer> coeffA2 = Collections.singletonList(3);
    final Assignment a1 = new Assignment();
    a1.addLiteral(f.literal("a"));
    final Assignment a2 = new Assignment();
    a2.addLiteral(f.literal("a"));
    a2.addLiteral(f.literal("b", false));
    final Assignment a3 = new Assignment();
    a3.addLiteral(f.literal("a"));
    a3.addLiteral(f.literal("b", false));
    a3.addLiteral(f.literal("c"));
    final Assignment a4 = new Assignment();
    a4.addLiteral(f.literal("a", false));
    a4.addLiteral(f.literal("b"));
    a4.addLiteral(f.literal("c", false));
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    Assert.assertEquals(f.pbc(CType.EQ, 0, litsA1, coeffA1), pb1.restrict(a1));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsA2, coeffA2), pb1.restrict(a2));
    Assert.assertEquals(f.falsum(), pb1.restrict(a3));
    Assert.assertEquals(f.falsum(), pb1.restrict(a4));
  }

  @Test
  public void testContainsSubformula() {
    Assert.assertTrue(pb1.containsSubformula(f.literal("a")));
    Assert.assertFalse(pb1.containsSubformula(f.literal("a", false)));
    Assert.assertTrue(pb2.containsSubformula(f.literal("b", false)));
    Assert.assertTrue(pb2.containsSubformula(f.literal("b")));
    Assert.assertFalse(pb2.containsSubformula(f.literal("d")));
    Assert.assertTrue(pb1.containsSubformula(pb1));
    Assert.assertTrue(pb2.containsSubformula(pb2));
    Assert.assertTrue(pb2.containsSubformula(pb22));
  }

  @Test
  public void testSubstitute() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Literal> litsS1 = Arrays.asList(f.literal("b", false), f.literal("c"));
    final List<Literal> litsS2 = Collections.singletonList(f.literal("c"));
    final List<Literal> litsS5 = Arrays.asList(f.literal("a2"), f.literal("b2", false), f.literal("c2"));
    final List<Literal> litsS6 = Arrays.asList(f.literal("a2"), f.literal("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final List<Integer> coeffS1 = Arrays.asList(-2, 3);
    final List<Integer> coeffS2 = Collections.singletonList(3);
    final List<Integer> coeffS6 = Arrays.asList(2, 3);
    final Substitution s1 = new Substitution();
    s1.addMapping(f.literal("a"), f.verum());
    final Substitution s2 = new Substitution();
    s2.addMapping(f.literal("a"), f.verum());
    s2.addMapping(f.literal("b"), f.falsum());
    final Substitution s3 = new Substitution();
    s3.addMapping(f.literal("a"), f.verum());
    s3.addMapping(f.literal("b"), f.falsum());
    s3.addMapping(f.literal("c"), f.verum());
    final Substitution s4 = new Substitution();
    s4.addMapping(f.literal("a"), f.falsum());
    s4.addMapping(f.literal("b"), f.verum());
    s4.addMapping(f.literal("c"), f.falsum());
    final Substitution s5 = new Substitution();
    s5.addMapping(f.literal("a"), f.literal("a2"));
    s5.addMapping(f.literal("b"), f.literal("b2"));
    s5.addMapping(f.literal("c"), f.literal("c2"));
    s5.addMapping(f.literal("d"), f.literal("d2"));
    final Substitution s6 = new Substitution();
    s6.addMapping(f.literal("a"), f.literal("a2"));
    s6.addMapping(f.literal("b"), f.falsum());
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    Assert.assertEquals(f.pbc(CType.EQ, 0, litsS1, coeffS1), pb1.substitute(s1));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsS2, coeffS2), pb1.substitute(s2));
    Assert.assertEquals(f.falsum(), pb1.substitute(s3));
    Assert.assertEquals(f.verum(), pb2.substitute(s3));
    Assert.assertEquals(f.falsum(), pb1.substitute(s4));
    Assert.assertEquals(f.verum(), pb2.substitute(s4));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsS5, coeffs), pb1.substitute(s5));
    Assert.assertEquals(f.pbc(CType.EQ, 4, litsS6, coeffS6), pb1.substitute(s6));
  }

  @Test
  public void testNegation() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    final PBConstraint pb3 = f.pbc(CType.GE, 1, lits, coeffs);
    final PBConstraint pb4 = f.pbc(CType.GT, 0, lits, coeffs);
    final PBConstraint pb5 = f.pbc(CType.LE, 1, lits, coeffs);
    final PBConstraint pb6 = f.pbc(CType.LT, 2, lits, coeffs);
    Assert.assertEquals(f.or(f.pbc(CType.LT, 2, lits, coeffs), f.pbc(CType.GT, 2, lits, coeffs)), pb1.negate());
    Assert.assertEquals(f.pbc(CType.LT, 1, lits, coeffs), pb3.negate());
    Assert.assertEquals(f.pbc(CType.LE, 0, lits, coeffs), pb4.negate());
    Assert.assertEquals(f.pbc(CType.GT, 1, lits, coeffs), pb5.negate());
    Assert.assertEquals(f.pbc(CType.GE, 2, lits, coeffs), pb6.negate());
  }

  @Test
  public void testNNF() {
    Assert.assertEquals(f.literal("a", false), pb1.nnf());
    Assert.assertEquals(f.literal("a", false), cc1.nnf());
    Assert.assertEquals(f.verum(), amo1.nnf());
    Assert.assertEquals(f.literal("a"), exo1.nnf());
  }

  @Test
  public void testNormalization() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"), f.literal("d"),
            f.literal("b", false));
    final List<Integer> coeffs = Arrays.asList(2, -3, 3, 0, 1);
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    final PBConstraint pb2 = f.pbc(CType.GE, 1, lits, coeffs);
    final PBConstraint pb3 = f.pbc(CType.GT, 0, lits, coeffs);
    final PBConstraint pb4 = f.pbc(CType.LE, 1, lits, coeffs);
    final PBConstraint pb5 = f.pbc(CType.LT, 2, lits, coeffs);
    Assert.assertEquals("(2*a + 2*b + 3*c <= 4) & (2*~a + 2*~b + 3*~c <= 3)", pb1.normalize().toString());
    Assert.assertEquals("2*~a + 2*~b + 3*~c <= 4", pb2.normalize().toString());
    Assert.assertEquals("2*~a + 2*~b + 3*~c <= 4", pb3.normalize().toString());
    Assert.assertEquals("2*a + 2*b + 3*c <= 3", pb4.normalize().toString());
    Assert.assertEquals("2*a + 2*b + 3*c <= 3", pb5.normalize().toString());
  }

  @Test
  public void testNormalizationTrivial() {
    final List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"), f.literal("d"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3, 0);
    final PBConstraint pb1 = f.pbc(CType.LE, 4, lits, coeffs);
    final PBConstraint pb2 = f.pbc(CType.LE, 5, lits, coeffs);
    final PBConstraint pb3 = f.pbc(CType.LE, 7, lits, coeffs);
    final PBConstraint pb4 = f.pbc(CType.LE, 10, lits, coeffs);
    final PBConstraint pb5 = f.pbc(CType.LE, -3, lits, coeffs);
    Assert.assertEquals("2*a + 2*b + 3*c <= 6", pb1.normalize().toString());
    Assert.assertEquals(f.verum(), pb2.normalize());
    Assert.assertEquals(f.verum(), pb3.normalize());
    Assert.assertEquals(f.verum(), pb4.normalize());
    Assert.assertEquals(f.falsum(), pb5.normalize());
  }

  @Test
  public void testNormalizationSimplifications() {
    List<Literal> lits = Arrays.asList(f2.literal("a"), f.literal("a"), f.literal("c"), f.literal("d"));
    List<Integer> coeffs = Arrays.asList(2, -2, 4, 4);
    final PBConstraint pb1 = f.pbc(CType.LE, 4, lits, coeffs);
    Assert.assertEquals("c + d <= 1", pb1.normalize().toString());
    lits = Arrays.asList(f2.literal("a"), f.literal("a", false), f.literal("c"), f.literal("d"));
    coeffs = Arrays.asList(2, 2, 4, 2);
    final PBConstraint pb2 = f.pbc(CType.LE, 4, lits, coeffs);
    Assert.assertEquals("2*c + d <= 1", pb2.normalize().toString());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("3*a <= 2", this.pb1.toString());
    Assert.assertEquals("3*a + -2*~b + 7*c <= 8", this.pb2.toString());
    Assert.assertEquals("3*a + -2*~b + 7*c <= 8", this.pb22.toString());
    Assert.assertEquals("a < 1", this.cc1.toString());
    Assert.assertEquals("a + b + c >= 2", this.cc2.toString());
    Assert.assertEquals("a <= 1", this.amo1.toString());
    Assert.assertEquals("a + b + c <= 1", this.amo2.toString());
    Assert.assertEquals("a = 1", this.exo1.toString());
    Assert.assertEquals("a + b + c = 1", this.exo2.toString());
  }

  @Test
  public void testEquals() {
    final List<Literal> lits2 = Arrays.asList(f2.literal("a"), f.literal("b", false), f.literal("c"));
    final List<Integer> coeffs2 = Arrays.asList(3, -2, 7);
    final List<Literal> lits2alt1 = Arrays.asList(f2.literal("a"), f.literal("b", false));
    final List<Integer> coeffs2alt1 = Arrays.asList(3, -2);
    final List<Literal> lits2alt2 = Arrays.asList(f2.literal("a"), f.literal("b"), f.literal("c"));
    final List<Integer> coeffs2alt2 = Arrays.asList(3, -2, 8);
    Assert.assertEquals(pb1, pb1);
    Assert.assertEquals(pb2, pb22);
    Assert.assertEquals(pb2, f.pbc(CType.LE, 8, lits2, coeffs2));
    Assert.assertNotEquals(cc1, cc2);
    Assert.assertNotEquals(cc1, null);
    Assert.assertNotEquals(null, cc1);
    Assert.assertNotEquals("String", cc2);
    Assert.assertNotEquals(cc2, "String");
    Assert.assertNotEquals(pb2, f.pbc(CType.LE, 8, lits2alt1, coeffs2alt1));
    Assert.assertNotEquals(pb2, f.pbc(CType.LE, 8, lits2alt2, coeffs2));
    Assert.assertNotEquals(pb2, f.pbc(CType.LE, 8, lits2, coeffs2alt2));
    Assert.assertNotEquals(pb2, f.pbc(CType.LT, 8, lits2, coeffs2));
    Assert.assertNotEquals(pb2, f.pbc(CType.LE, 7, lits2, coeffs2));
  }

  @Test
  public void testHash() {
    Assert.assertEquals(pb1.hashCode(), pb1.hashCode());
    Assert.assertEquals(pb2.hashCode(), pb2.hashCode());
    Assert.assertEquals(pb2.hashCode(), pb22.hashCode());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    Assert.assertEquals(1, pb2.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(0, pb1.numberOfOperands());
    Assert.assertEquals(0, pb2.numberOfOperands());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertTrue(pb2.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertTrue(pb2.contains(F.f.literal("a")));
    Assert.assertTrue(pb2.contains(F.f.literal("b")));
    Assert.assertTrue(pb2.contains(F.f.literal("c")));
    Assert.assertFalse(pb2.contains(F.f.literal("x")));
    Assert.assertFalse(pb2.contains(F.f.literal("a", false)));
  }
}
