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
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.datastructures.Tristate;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit Tests for the class {@link PBConstraint}.
 * @version 1.3
 * @since 1.0
 */
public class PBConstraintTest {

  private static final FormulaFactory f = new FormulaFactory();
  private static final FormulaFactory f2 = new FormulaFactory();

  private final PBConstraint pb1;
  private final PBConstraint pb2;
  private final PBConstraint pb22;
  private final PBConstraint cc1;
  private final PBConstraint cc2;
  private final PBConstraint amo1;
  private final PBConstraint amo2;
  private final PBConstraint exo1;
  private final PBConstraint exo2;

  public PBConstraintTest() {
    final Variable[] lits1 = new Variable[]{f.variable("a")};
    final List<Literal> lits2 = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Variable> litsCC2 = Arrays.asList(f.variable("a"), f2.variable("b"), f.variable("c"));
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
    final List<Literal> lits2 = Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Integer> coeffs2 = Arrays.asList(3, -2, 7, 2);
    f.pbc(CType.EQ, 3, lits2, coeffs2);
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
    final Literal[] lits1 = new Literal[]{f.variable("a")};
    final Literal[] lits2 = new Literal[]{f2.variable("a"), f.literal("b", false), f.variable("c")};
    final Literal[] litsCC2 = new Literal[]{f.variable("a"), f.variable("b"), f2.variable("c")};
    final int[] coeffs1 = new int[]{3};
    final int[] coeffs2 = new int[]{3, -2, 7};

    final int[] coeffsCC1 = new int[]{1};
    final int[] coeffsCC2 = new int[]{1, 1, 1};

    Assert.assertArrayEquals(lits1, this.pb1.operands());
    Assert.assertArrayEquals(coeffs1, this.pb1.coefficients());
    Assert.assertEquals(CType.LE, this.pb1.comparator());
    Assert.assertEquals(2, this.pb1.rhs());
    Assert.assertFalse(this.pb1.isCC());
    Assert.assertEquals(3, this.pb1.maxWeight());

    Assert.assertArrayEquals(lits2, this.pb2.operands());
    Assert.assertArrayEquals(coeffs2, this.pb2.coefficients());
    Assert.assertEquals(CType.LE, this.pb2.comparator());
    Assert.assertEquals(8, this.pb2.rhs());
    Assert.assertFalse(this.pb2.isCC());
    Assert.assertEquals(7, this.pb2.maxWeight());

    Assert.assertArrayEquals(lits1, this.cc1.operands());
    Assert.assertArrayEquals(coeffsCC1, this.cc1.coefficients());
    Assert.assertEquals(CType.LT, this.cc1.comparator());
    Assert.assertEquals(1, this.cc1.rhs());
    Assert.assertTrue(this.cc1.isCC());
    Assert.assertEquals(1, this.cc1.maxWeight());

    Assert.assertArrayEquals(litsCC2, this.cc2.operands());
    Assert.assertArrayEquals(coeffsCC2, this.cc2.coefficients());
    Assert.assertEquals(CType.GE, this.cc2.comparator());
    Assert.assertEquals(2, this.cc2.rhs());
    Assert.assertTrue(this.cc2.isCC());

    Assert.assertArrayEquals(lits1, this.amo1.operands());
    Assert.assertArrayEquals(coeffsCC1, this.amo1.coefficients());
    Assert.assertEquals(CType.LE, this.amo1.comparator());
    Assert.assertEquals(1, this.amo1.rhs());
    Assert.assertTrue(this.amo1.isCC());

    Assert.assertArrayEquals(litsCC2, this.amo2.operands());
    Assert.assertArrayEquals(coeffsCC2, this.amo2.coefficients());
    Assert.assertEquals(CType.LE, this.amo2.comparator());
    Assert.assertEquals(1, this.amo2.rhs());
    Assert.assertTrue(this.amo2.isCC());

    Assert.assertArrayEquals(lits1, this.exo1.operands());
    Assert.assertArrayEquals(coeffsCC1, this.exo1.coefficients());
    Assert.assertEquals(CType.EQ, this.exo1.comparator());
    Assert.assertEquals(1, this.exo1.rhs());
    Assert.assertTrue(this.exo1.isCC());

    Assert.assertArrayEquals(litsCC2, this.exo2.operands());
    Assert.assertArrayEquals(coeffsCC2, this.exo2.coefficients());
    Assert.assertEquals(CType.EQ, this.exo2.comparator());
    Assert.assertEquals(1, this.exo2.rhs());
    Assert.assertTrue(this.exo2.isCC());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(1, this.pb1.numberOfAtoms());
    Assert.assertEquals(1, this.pb2.numberOfAtoms());
    Assert.assertEquals(1, this.cc1.numberOfAtoms());
    Assert.assertEquals(1, this.cc2.numberOfAtoms());
    Assert.assertEquals(1, this.amo1.numberOfAtoms());
    Assert.assertEquals(1, this.amo2.numberOfAtoms());
    Assert.assertEquals(1, this.exo1.numberOfAtoms());
    Assert.assertEquals(1, this.exo2.numberOfAtoms());
    Assert.assertEquals(1, this.exo2.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(2, this.pb1.numberOfNodes());
    Assert.assertEquals(4, this.pb2.numberOfNodes());
    Assert.assertEquals(2, this.cc1.numberOfNodes());
    Assert.assertEquals(4, this.cc2.numberOfNodes());
    Assert.assertEquals(2, this.amo1.numberOfNodes());
    Assert.assertEquals(4, this.amo2.numberOfNodes());
    Assert.assertEquals(2, this.exo1.numberOfNodes());
    Assert.assertEquals(4, this.exo2.numberOfNodes());
    Assert.assertEquals(4, this.exo2.numberOfNodes());
  }

  @Test
  public void testVariables() {
    final SortedSet<Variable> lits1 = new TreeSet<>(Collections.singletonList(f.variable("a")));
    final SortedSet<Variable> lits2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.variable("b"), f.variable("c")));
    Assert.assertEquals(lits1, this.pb1.variables());
    Assert.assertEquals(lits1, this.pb1.variables());
    Assert.assertEquals(lits2, this.pb2.variables());
    Assert.assertEquals(lits1, this.cc1.variables());
    Assert.assertEquals(lits2, this.cc2.variables());
    Assert.assertEquals(lits1, this.amo1.variables());
    Assert.assertEquals(lits2, this.amo2.variables());
    Assert.assertEquals(lits1, this.exo1.variables());
    Assert.assertEquals(lits2, this.exo2.variables());
  }

  @Test
  public void testLiterals() {
    final SortedSet<Variable> lits1 = new TreeSet<>(Collections.singletonList(f.variable("a")));
    final SortedSet<Literal> lits2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c")));
    final SortedSet<Variable> litsCC2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.variable("b"), f.variable("c")));
    Assert.assertEquals(lits1, this.pb1.literals());
    Assert.assertEquals(lits2, this.pb2.literals());
    Assert.assertEquals(lits1, this.cc1.literals());
    Assert.assertEquals(litsCC2, this.cc2.literals());
    Assert.assertEquals(lits1, this.amo1.literals());
    Assert.assertEquals(litsCC2, this.amo2.literals());
    Assert.assertEquals(lits1, this.exo1.literals());
    Assert.assertEquals(litsCC2, this.exo2.literals());
  }

  @Test
  public void testContains() {
    Assert.assertTrue(this.pb2.containsVariable(F.f.variable("a")));
    Assert.assertTrue(this.pb2.containsVariable(F.f.variable("b")));
    Assert.assertTrue(this.pb2.containsVariable(F.f.variable("c")));
    Assert.assertFalse(this.pb2.containsVariable(F.f.variable("d")));
    Assert.assertFalse(this.pb2.containsVariable(F.f.variable("x")));
  }

  @Test
  public void testEvaluate() {
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final Assignment a1 = new Assignment();
    a1.addLiteral(f.variable("a"));
    a1.addLiteral(f.variable("b"));
    a1.addLiteral(f.literal("c", false));
    final Assignment a2 = new Assignment();
    a2.addLiteral(f.variable("a"));
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
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Literal> litsA1 = Arrays.asList(f.literal("b", false), f.variable("c"));
    final List<Variable> litsA2 = Collections.singletonList(f.variable("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final List<Integer> coeffA1 = Arrays.asList(-2, 3);
    final List<Integer> coeffA2 = Collections.singletonList(3);
    final Assignment a1 = new Assignment();
    a1.addLiteral(f.variable("a"));
    final Assignment a2 = new Assignment();
    a2.addLiteral(f.variable("a"));
    a2.addLiteral(f.literal("b", false));
    final Assignment a3 = new Assignment();
    a3.addLiteral(f.variable("a"));
    a3.addLiteral(f.literal("b", false));
    a3.addLiteral(f.variable("c"));
    final Assignment a4 = new Assignment();
    a4.addLiteral(f.literal("a", false));
    a4.addLiteral(f.variable("b"));
    a4.addLiteral(f.literal("c", false));
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    Assert.assertEquals(f.pbc(CType.EQ, 0, litsA1, coeffA1), pb1.restrict(a1));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsA2, coeffA2), pb1.restrict(a2));
    Assert.assertEquals(f.falsum(), pb1.restrict(a3));
    Assert.assertEquals(f.falsum(), pb1.restrict(a4));
  }

  @Test
  public void testRestrictInequality() {
    final List<Literal> lits = Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"), f.variable("e"), f.literal("f", false));
    final List<Integer> coeffs = Arrays.asList(75, 50, 201, -3, -24, 1);
    final PBConstraint pb1 = f.pbc(CType.GE, -24, lits, coeffs);
    final PBConstraint pb2 = f.pbc(CType.LE, 150, lits, coeffs);
    final Assignment a1 = new Assignment();
    a1.addLiteral(f.literal("b", false));
    a1.addLiteral(f.variable("c"));
    final Assignment a2 = new Assignment();
    a2.addLiteral(f.literal("a", false));
    a2.addLiteral(f.variable("b"));
    a2.addLiteral(f.literal("c", false));
    a2.addLiteral(f.variable("d"));
    a2.addLiteral(f.variable("e"));
    final Assignment a3 = new Assignment();
    a3.addLiteral(f.literal("c", false));

    Assert.assertEquals(f.verum(), pb1.restrict(a1));
    Assert.assertEquals(f.falsum(), pb2.restrict(a1));
    Assert.assertEquals(f.falsum(), pb1.restrict(a2));
    Assert.assertEquals(f.verum(), pb2.restrict(a3));
  }

  @Test
  public void testContainsSubformula() {
    Assert.assertTrue(this.pb1.containsNode(f.variable("a")));
    Assert.assertFalse(this.pb1.containsNode(f.literal("a", false)));
    Assert.assertTrue(this.pb2.containsNode(f.literal("b", false)));
    Assert.assertTrue(this.pb2.containsNode(f.variable("b")));
    Assert.assertFalse(this.pb2.containsNode(f.variable("d")));
    Assert.assertTrue(this.pb1.containsNode(this.pb1));
    Assert.assertTrue(this.pb2.containsNode(this.pb2));
    Assert.assertTrue(this.pb2.containsNode(this.pb22));
  }

  @Test
  public void testSubstitute() {
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Literal> litsS1 = Arrays.asList(f.literal("b", false), f.variable("c"));
    final List<Variable> litsS2 = Collections.singletonList(f.variable("c"));
    final List<Literal> litsS5 = Arrays.asList(f.variable("a2"), f.literal("b2", false), f.variable("c2"));
    final List<Variable> litsS6 = Arrays.asList(f.variable("a2"), f.variable("c"));
    final List<Integer> coeffs = Arrays.asList(2, -2, 3);
    final List<Integer> coeffS1 = Arrays.asList(-2, 3);
    final List<Integer> coeffS2 = Collections.singletonList(3);
    final List<Integer> coeffS6 = Arrays.asList(2, 3);
    final Substitution s1 = new Substitution();
    s1.addMapping(f.variable("a"), f.verum());
    final Substitution s2 = new Substitution();
    s2.addMapping(f.variable("a"), f.verum());
    s2.addMapping(f.variable("b"), f.falsum());
    final Substitution s3 = new Substitution();
    s3.addMapping(f.variable("a"), f.verum());
    s3.addMapping(f.variable("b"), f.falsum());
    s3.addMapping(f.variable("c"), f.verum());
    final Substitution s4 = new Substitution();
    s4.addMapping(f.variable("a"), f.falsum());
    s4.addMapping(f.variable("b"), f.verum());
    s4.addMapping(f.variable("c"), f.falsum());
    final Substitution s5 = new Substitution();
    s5.addMapping(f.variable("a"), f.variable("a2"));
    s5.addMapping(f.variable("b"), f.variable("b2"));
    s5.addMapping(f.variable("c"), f.variable("c2"));
    s5.addMapping(f.variable("d"), f.variable("d2"));
    final Substitution s6 = new Substitution();
    s6.addMapping(f.variable("a"), f.variable("a2"));
    s6.addMapping(f.variable("b"), f.falsum());
    final PBConstraint pb1 = f.pbc(CType.EQ, 2, lits, coeffs);
    Assert.assertEquals(f.pbc(CType.EQ, 0, litsS1, coeffS1), pb1.substitute(s1));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsS2, coeffS2), pb1.substitute(s2));
    Assert.assertEquals(f.falsum(), pb1.substitute(s3));
    Assert.assertEquals(f.verum(), this.pb2.substitute(s3));
    Assert.assertEquals(f.falsum(), pb1.substitute(s4));
    Assert.assertEquals(f.verum(), this.pb2.substitute(s4));
    Assert.assertEquals(f.pbc(CType.EQ, 2, litsS5, coeffs), pb1.substitute(s5));
    Assert.assertEquals(f.pbc(CType.EQ, 4, litsS6, coeffS6), pb1.substitute(s6));
  }

  @Test
  public void testNegation() {
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
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
    Assert.assertEquals(f.literal("a", false), this.pb1.nnf());
    Assert.assertEquals(f.literal("a", false), this.cc1.nnf());
    Assert.assertEquals(f.verum(), this.amo1.nnf());
    Assert.assertEquals(f.variable("a"), this.exo1.nnf());
  }

  @Test
  public void testNormalization() {
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"),
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
    final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"));
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
    List<? extends Literal> lits = Arrays.asList(f2.variable("a"), f.variable("a"), f.variable("c"), f.variable("d"));
    List<Integer> coeffs = Arrays.asList(2, -2, 4, 4);
    final PBConstraint pb1 = f.pbc(CType.LE, 4, lits, coeffs);
    Assert.assertEquals("c + d <= 1", pb1.normalize().toString());
    lits = Arrays.asList(f2.variable("a"), f.literal("a", false), f.variable("c"), f.variable("d"));
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
    final List<Literal> lits2 = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
    final List<Integer> coeffs2 = Arrays.asList(3, -2, 7);
    final List<Literal> lits2alt1 = Arrays.asList(f2.variable("a"), f.literal("b", false));
    final List<Integer> coeffs2alt1 = Arrays.asList(3, -2);
    final List<Variable> lits2alt2 = Arrays.asList(f2.variable("a"), f.variable("b"), f.variable("c"));
    final List<Integer> coeffs2alt2 = Arrays.asList(3, -2, 8);
    Assert.assertEquals(this.pb1, this.pb1);
    Assert.assertEquals(this.pb2, this.pb22);
    Assert.assertEquals(this.pb2, f.pbc(CType.LE, 8, lits2, coeffs2));
    Assert.assertNotEquals(this.cc1, this.cc2);
    Assert.assertNotEquals(this.cc1, null);
    Assert.assertNotEquals(null, this.cc1);
    Assert.assertNotEquals("String", this.cc2);
    Assert.assertNotEquals(this.cc2, "String");
    Assert.assertNotEquals(this.pb2, f.pbc(CType.LE, 8, lits2alt1, coeffs2alt1));
    Assert.assertNotEquals(this.pb2, f.pbc(CType.LE, 8, lits2alt2, coeffs2));
    Assert.assertNotEquals(this.pb2, f.pbc(CType.LE, 8, lits2, coeffs2alt2));
    Assert.assertNotEquals(this.pb2, f.pbc(CType.LT, 8, lits2, coeffs2));
    Assert.assertNotEquals(this.pb2, f.pbc(CType.LE, 7, lits2, coeffs2));
  }

  @Test
  public void testHash() {
    Assert.assertEquals(this.pb1.hashCode(), this.pb1.hashCode());
    Assert.assertEquals(this.pb2.hashCode(), this.pb2.hashCode());
    Assert.assertEquals(this.pb2.hashCode(), this.pb22.hashCode());
  }

  @Test
  public void testNumberOfInternalNodes() {
    Assert.assertEquals(1, this.pb2.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(0, this.pb1.numberOfOperands());
    Assert.assertEquals(0, this.pb2.numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertFalse(this.pb1.isConstantFormula());
    Assert.assertFalse(this.pb2.isConstantFormula());
    Assert.assertFalse(this.pb22.isConstantFormula());
    Assert.assertFalse(this.cc1.isConstantFormula());
    Assert.assertFalse(this.cc2.isConstantFormula());
    Assert.assertFalse(this.amo1.isConstantFormula());
    Assert.assertFalse(this.amo2.isConstantFormula());
    Assert.assertFalse(this.exo1.isConstantFormula());
    Assert.assertFalse(this.exo2.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertTrue(this.pb2.isAtomicFormula());
  }

  @Test
  public void testEvaluateCoeffs() {
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, -3, CType.EQ));
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, 3, CType.EQ));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, -2, CType.EQ));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 2, CType.EQ));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 0, CType.EQ));

    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, -3, CType.GE));
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, 3, CType.GE));
    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, -2, CType.GE));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 2, CType.GE));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 0, CType.GE));

    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, -3, CType.GT));
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, 3, CType.GT));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, -2, CType.GT));
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, 2, CType.GT));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 0, CType.GT));

    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, -3, CType.LE));
    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, 3, CType.LE));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, -2, CType.LE));
    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, 2, CType.LE));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 0, CType.LE));

    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, -3, CType.LT));
    Assert.assertEquals(Tristate.TRUE, PBConstraint.evaluateCoeffs(-2, 2, 3, CType.LT));
    Assert.assertEquals(Tristate.FALSE, PBConstraint.evaluateCoeffs(-2, 2, -2, CType.LT));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 2, CType.LT));
    Assert.assertEquals(Tristate.UNDEF, PBConstraint.evaluateCoeffs(-2, 2, 0, CType.LT));
  }

  @Test
  public void testTrivialTrue() {
    Assert.assertTrue(f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());

    Assert.assertFalse(f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertTrue(f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());

    Assert.assertTrue(f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertTrue(f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());

    Assert.assertFalse(f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertTrue(f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());

    Assert.assertTrue(f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertTrue(f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
    Assert.assertFalse(f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialTrue());
  }

  @Test
  public void testTrivialFalse() {
    Assert.assertFalse(f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());

    Assert.assertTrue(f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertFalse(f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());

    Assert.assertFalse(f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertFalse(f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());

    Assert.assertTrue(f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertFalse(f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());

    Assert.assertFalse(f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertFalse(f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
    Assert.assertTrue(f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).isTrivialFalse());
  }

  @Test
  public void testSimplifiedToString() {
    Assert.assertEquals(f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
    Assert.assertEquals(f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$true");
    Assert.assertEquals(f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).toString(), "$false");
  }

  @Test
  public void testSimplifiedCompareToConstant1() {
    Assert.assertEquals(f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
    Assert.assertEquals(f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.verum());
    Assert.assertEquals(f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()), f.falsum());
  }

  @Test
  public void testSimplifiedCompareToConstant2() {
    Assert.assertEquals(f.verum(), f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.verum(), f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()));
    Assert.assertEquals(f.falsum(), f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()));
  }

  @Test
  public void testSimplifiedType() {
    Assert.assertEquals(f.pbc(CType.EQ, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.EQ, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.EQ, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.GT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.GT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.GT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.GE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.GE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.GE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.LT, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.LT, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.LT, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
    Assert.assertEquals(f.pbc(CType.LE, 0, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.LE, 1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.TRUE);
    Assert.assertEquals(f.pbc(CType.LE, -1, new ArrayList<Literal>(), new ArrayList<Integer>()).type(), FType.FALSE);
  }

  @Test
  public void testCommutativeLaw() throws ParserException {
    final FormulaFactory ff = new FormulaFactory();
    final PseudoBooleanParser p = new PseudoBooleanParser(ff);
    final Formula parse = p.parse("2*a + 3*c + -4*b + -12*~d + 3*e <= 6");
    assertThat(parse.toString()).isEqualTo("2*a + 3*c + -4*b + -12*~d + 3*e <= 6");
    assertThat(p.parse("-4*b + 2*a + 3*e + 3*c + -12*~d <= 6").toString()).isEqualTo("2*a + 3*c + -4*b + -12*~d + 3*e <= 6");
    assertThat(p.parse("3*e + -12*~d + -4*b + 3*c + 2*a <= 6").toString()).isEqualTo("2*a + 3*c + -4*b + -12*~d + 3*e <= 6");
    assertThat(ff.statistics().pbcs()).isEqualTo(1);
  }
}
