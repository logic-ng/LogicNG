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

package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDConstant;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for {@link BDDFactory}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class SimpleBDDTest {

    @Test
    public void testTrue() {
        final FormulaFactory f = new FormulaFactory();
        final BDD bdd = BDDFactory.build(f.verum());
        assertThat(bdd.isTautology()).isTrue();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(f.verum());
        assertThat(bdd.dnf()).isEqualTo(f.verum());
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactly(new Assignment());
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ZERO);
        assertThat(bdd.toLngBdd()).isEqualTo(BDDConstant.getVerumNode(f));
    }

    @Test
    public void testFalse() {
        final FormulaFactory f = new FormulaFactory();
        final BDDKernel kernel = new BDDKernel(f, 0, 100, 100);
        final BDD bdd = BDDFactory.build(f.falsum(), kernel, null);
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isTrue();
        assertThat(bdd.cnf()).isEqualTo(f.falsum());
        assertThat(bdd.dnf()).isEqualTo(f.falsum());
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.ZERO);
        assertThat(bdd.underlyingKernel()).isSameAs(kernel);
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).isEmpty();
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.toLngBdd()).isEqualTo(BDDConstant.getFalsumNode(f));
    }

    @Test
    public void testPositiveLiteral() {
        final FormulaFactory f = new FormulaFactory();
        final BDD bdd = BDDFactory.build(f.literal("A", true));
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(f.literal("A", true));
        assertThat(bdd.dnf()).isEqualTo(f.literal("A", true));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactly(new Assignment(f.literal("A", true)));
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<$false> high=<$true>>");
    }

    @Test
    public void testNegativeLiteral() {
        final FormulaFactory f = new FormulaFactory();
        final BDD bdd = BDDFactory.build(f.literal("A", false));
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(f.literal("A", false));
        assertThat(bdd.dnf()).isEqualTo(f.literal("A", false));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactly(new Assignment(f.literal("A", false)));
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<$true> high=<$false>>");
    }

    @Test
    public void testImplication() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final BDD bdd = BDDFactory.build(parser.parse("A => ~B"));
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(parser.parse("~A | ~B"));
        assertThat(bdd.dnf()).isEqualTo(parser.parse("~A & ~B | ~A & B | A & ~B"));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(3));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactlyInAnyOrder(
                new Assignment(f.literal("A", false), f.literal("B", false)),
                new Assignment(f.literal("A", true), f.literal("B", false)),
                new Assignment(f.literal("A", false), f.literal("B", true))
        );
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<$true> high=<B | low=<$true> high=<$false>>>");
    }

    @Test
    public void testEquivalence() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final BDD bdd = BDDFactory.build(parser.parse("A <=> ~B"));
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(parser.parse("(A | B) & (~A | ~B)"));
        assertThat(bdd.dnf()).isEqualTo(parser.parse("~A & B | A & ~B"));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(2));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactlyInAnyOrder(
                new Assignment(f.literal("A", true), f.literal("B", false)),
                new Assignment(f.literal("A", false), f.literal("B", true))
        );
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.valueOf(2));
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<B | low=<$false> high=<$true>> high=<B | low=<$true> high=<$false>>>");
    }

    @Test
    public void testOr() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final BDD bdd = BDDFactory.build(parser.parse("A | B | ~C"));
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(parser.parse("A | B | ~C"));
        assertThat(bdd.dnf()).isEqualTo(parser.parse("~A & ~B & ~C | ~A & B & ~C | ~A & B & C | A & ~B & ~C | A & ~B & C | A & B & ~C | A & B & C"));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(7));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactlyInAnyOrder(
                new Assignment(f.literal("A", false), f.literal("B", false), f.literal("C", false)),
                new Assignment(f.literal("A", false), f.literal("B", true), f.literal("C", false)),
                new Assignment(f.literal("A", false), f.literal("B", true), f.literal("C", true)),
                new Assignment(f.literal("A", true), f.literal("B", false), f.literal("C", false)),
                new Assignment(f.literal("A", true), f.literal("B", false), f.literal("C", true)),
                new Assignment(f.literal("A", true), f.literal("B", true), f.literal("C", false)),
                new Assignment(f.literal("A", true), f.literal("B", true), f.literal("C", true))
        );
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.ONE);
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<B | low=<C | low=<$true> high=<$false>> high=<$true>> high=<$true>>");
    }

    @Test
    public void testAnd() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        final BDD bdd = BDDFactory.build(parser.parse("A & B & ~C"), kernel, null);
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(parser.parse("A & (~A | B) & (~A | ~B | ~C)"));
        assertThat(bdd.dnf()).isEqualTo(parser.parse("A & B & ~C"));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(1));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactlyInAnyOrder(
                new Assignment(f.literal("A", true), f.literal("B", true), f.literal("C", false))
        );
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.valueOf(3));
        assertThat(bdd.toLngBdd().toString()).isEqualTo("<A | low=<$false> high=<B | low=<$false> high=<C | low=<$true> high=<$false>>>>");
    }

    @Test
    public void testFormula() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        final BDD bdd = BDDFactory.build(parser.parse("(A => ~C) | (B & ~C)"), kernel, null);
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(6));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).hasSize(6);
        assertThat(bdd.enumerateAllModels(f.variable("A"))).hasSize(2);
        assertThat(bdd.hashCode()).isEqualTo(BDDFactory.build(parser.parse("(A => ~C) | (B & ~C)"), kernel, null).hashCode());
        assertThat(bdd.toString()).isEqualTo("BDD{8}");
    }

    @Test
    public void testCC() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser parser = new PseudoBooleanParser(f);
        final BDDKernel kernel = new BDDKernel(f, 3, 1000, 1000);
        final BDD bdd = BDDFactory.build(parser.parse("A + B + C = 1"), kernel, null);
        assertThat(bdd.isTautology()).isFalse();
        assertThat(bdd.isContradiction()).isFalse();
        assertThat(bdd.cnf()).isEqualTo(parser.parse("(A | B | C) & (A | ~B | ~C) & (~A | B | ~C) & (~A | ~B)"));
        assertThat(bdd.dnf()).isEqualTo(parser.parse("~A & ~B & C | ~A & B & ~C | A & ~B & ~C"));
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(3));
        assertThat(bdd.underlyingKernel().factory()).isSameAs(f);
        assertThat(bdd.enumerateAllModels()).containsExactlyInAnyOrder(
                new Assignment(f.literal("A", true), f.literal("B", false), f.literal("C", false)),
                new Assignment(f.literal("A", false), f.literal("B", true), f.literal("C", false)),
                new Assignment(f.literal("A", false), f.literal("B", false), f.literal("C", true))
        );
        assertThat(bdd.numberOfClausesCNF()).isEqualTo(BigInteger.valueOf(4));
    }
}
