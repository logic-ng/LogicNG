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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.math.BigInteger;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

/**
 * Unit tests for {@link BDDFactory} operations.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDOperationsTest {

    private FormulaFactory f;
    private PropositionalParser parser;
    private BDDKernel kernel;
    private BDD bddVerum;
    private BDD bddFalsum;
    private BDD bddPosLit;
    private BDD bddNegLit;
    private BDD bddImpl;
    private BDD bddEquiv;
    private BDD bddOr;
    private BDD bddAnd;

    @BeforeEach
    public void init() throws ParserException {
        this.f = new FormulaFactory();
        this.parser = new PropositionalParser(this.f);
        this.kernel = new BDDKernel(this.f, 3, 1000, 1000);
        this.bddVerum = BDDFactory.build(this.f.verum(), this.kernel);
        this.bddFalsum = BDDFactory.build(this.f.falsum(), this.kernel);
        this.bddPosLit = BDDFactory.build(this.f.literal("A", true), this.kernel);
        this.bddNegLit = BDDFactory.build(this.f.literal("A", false), this.kernel);
        this.bddImpl = BDDFactory.build(this.parser.parse("A => ~B"), this.kernel);
        this.bddEquiv = BDDFactory.build(this.parser.parse("A <=> ~B"), this.kernel);
        this.bddOr = BDDFactory.build(this.parser.parse("A | B | ~C"), this.kernel);
        this.bddAnd = BDDFactory.build(this.parser.parse("A & B & ~C"), this.kernel);
    }

    @Test
    public void testRestriction() throws ParserException {
        final Literal a = this.f.literal("A", true);
        final List<Literal> resNotA = Collections.singletonList(this.f.literal("A", false));
        final List<Literal> resAB = Arrays.asList(this.f.literal("A", true), this.f.literal("B", true));
        assertThat(this.bddPosLit.construction.restrict(0, 1)).isEqualTo(0);
        assertThat(this.bddPosLit.construction.restrict(1, 1)).isEqualTo(1);
        assertThat(this.bddVerum.restrict(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddVerum.restrict(resNotA)).isEqualTo(this.bddVerum);
        assertThat(this.bddVerum.restrict(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddFalsum.restrict(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddFalsum.restrict(resNotA)).isEqualTo(this.bddFalsum);
        assertThat(this.bddFalsum.restrict(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddPosLit.restrict(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddPosLit.restrict(resNotA)).isEqualTo(this.bddFalsum);
        assertThat(this.bddPosLit.restrict(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddNegLit.restrict(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddNegLit.restrict(resNotA)).isEqualTo(this.bddVerum);
        assertThat(this.bddNegLit.restrict(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddImpl.restrict(a)).isEqualTo(BDDFactory.build(this.f.literal("B", false), this.kernel));
        assertThat(this.bddImpl.restrict(resNotA)).isEqualTo(this.bddVerum);
        assertThat(this.bddImpl.restrict(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddEquiv.restrict(a)).isEqualTo(BDDFactory.build(this.f.literal("B", false), this.kernel));
        assertThat(this.bddEquiv.restrict(resNotA)).isEqualTo(BDDFactory.build(this.f.literal("B", true), this.kernel));
        assertThat(this.bddEquiv.restrict(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddOr.restrict(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddOr.restrict(resNotA)).isEqualTo(BDDFactory.build(this.parser.parse("B | ~C"), this.kernel));
        assertThat(this.bddOr.restrict(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddAnd.restrict(a)).isEqualTo(BDDFactory.build(this.parser.parse("B & ~C"), this.kernel));
        assertThat(this.bddAnd.restrict(resNotA)).isEqualTo(this.bddFalsum);
        assertThat(this.bddAnd.restrict(resAB)).isEqualTo(BDDFactory.build(this.f.literal("C", false), this.kernel));
    }

    @Test
    public void testExistentialQuantification() throws ParserException {
        final Variable a = this.f.variable("A");
        final List<Variable> resAB = Arrays.asList(this.f.variable("A"), this.f.variable("B"));
        assertThat(this.bddPosLit.construction.exists(0, 1)).isEqualTo(0);
        assertThat(this.bddPosLit.construction.exists(1, 1)).isEqualTo(1);
        assertThat(this.bddVerum.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddVerum.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddFalsum.exists(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddFalsum.exists(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddPosLit.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddPosLit.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddNegLit.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddNegLit.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddImpl.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddImpl.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddEquiv.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddEquiv.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddOr.exists(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddOr.exists(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddAnd.exists(a)).isEqualTo(BDDFactory.build(this.parser.parse("B & ~C"), this.kernel));
        assertThat(this.bddAnd.exists(resAB)).isEqualTo(BDDFactory.build(this.parser.parse("~C"), this.kernel));
    }

    @Test
    public void testUniversalQuantification() throws ParserException {
        final Variable a = this.f.variable("A");
        final List<Variable> resAB = Arrays.asList(this.f.variable("A"), this.f.variable("B"));
        assertThat(this.bddPosLit.construction.forAll(0, 1)).isEqualTo(0);
        assertThat(this.bddPosLit.construction.forAll(1, 1)).isEqualTo(1);
        assertThat(this.bddVerum.forall(a)).isEqualTo(this.bddVerum);
        assertThat(this.bddVerum.forall(resAB)).isEqualTo(this.bddVerum);
        assertThat(this.bddFalsum.forall(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddFalsum.forall(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddPosLit.forall(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddPosLit.forall(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddNegLit.forall(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddNegLit.forall(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddImpl.forall(a)).isEqualTo(BDDFactory.build(this.parser.parse("~B"), this.kernel));
        assertThat(this.bddImpl.forall(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddEquiv.forall(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddEquiv.forall(resAB)).isEqualTo(this.bddFalsum);
        assertThat(this.bddOr.forall(a)).isEqualTo(BDDFactory.build(this.parser.parse("B | ~C"), this.kernel));
        assertThat(this.bddOr.forall(resAB)).isEqualTo(BDDFactory.build(this.parser.parse("~C"), this.kernel));
        assertThat(this.bddAnd.forall(a)).isEqualTo(this.bddFalsum);
        assertThat(this.bddAnd.forall(resAB)).isEqualTo(this.bddFalsum);
    }

    @Test
    public void testModel() {
        assertThat(this.bddVerum.model()).isEqualTo(new Assignment());
        assertThat(this.bddFalsum.model()).isEqualTo(null);
        assertThat(this.bddPosLit.model()).isEqualTo(new Assignment(this.f.literal("A", true)));
        assertThat(this.bddNegLit.model()).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddImpl.model()).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddEquiv.model()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddOr.model()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddAnd.model()).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
    }

    @Test
    public void testModelWithGivenVars() {
        final Variable a = this.f.variable("A");
        final List<Variable> ab = Arrays.asList(this.f.variable("A"), this.f.variable("B"));
        assertThat(this.bddVerum.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", true)));
        assertThat(this.bddVerum.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true)));
        assertThat(this.bddVerum.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddVerum.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false)));
        assertThat(this.bddFalsum.model(true, a)).isEqualTo(null);
        assertThat(this.bddFalsum.model(true, ab)).isEqualTo(null);
        assertThat(this.bddFalsum.model(false, a)).isEqualTo(null);
        assertThat(this.bddFalsum.model(false, ab)).isEqualTo(null);
        assertThat(this.bddPosLit.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", true)));
        assertThat(this.bddPosLit.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true)));
        assertThat(this.bddPosLit.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", true)));
        assertThat(this.bddPosLit.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", false)));
        assertThat(this.bddNegLit.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddNegLit.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddNegLit.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddNegLit.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false)));
        assertThat(this.bddImpl.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddImpl.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddImpl.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", false)));
        assertThat(this.bddImpl.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false)));
        assertThat(this.bddEquiv.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddEquiv.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddEquiv.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddEquiv.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true)));
        assertThat(this.bddOr.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddOr.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddOr.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddOr.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddAnd.model(true, a)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
        assertThat(this.bddAnd.model(true, ab)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
        assertThat(this.bddAnd.model(false, a)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
        assertThat(this.bddAnd.model(false, ab)).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
    }

    @Test
    public void testFullModel() {
        assertThat(this.bddVerum.fullModel()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddFalsum.fullModel()).isEqualTo(null);
        assertThat(this.bddPosLit.fullModel()).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddNegLit.fullModel()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddImpl.fullModel()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddEquiv.fullModel()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", true), this.f.literal("C", false)));
        assertThat(this.bddOr.fullModel()).isEqualTo(new Assignment(this.f.literal("A", false), this.f.literal("B", false), this.f.literal("C", false)));
        assertThat(this.bddAnd.fullModel()).isEqualTo(new Assignment(this.f.literal("A", true), this.f.literal("B", true), this.f.literal("C", false)));
    }

    @Test
    public void testPathCount() {
        assertThat(this.bddVerum.pathCountOne()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddVerum.pathCountZero()).isEqualTo(BigInteger.ZERO);
        assertThat(this.bddFalsum.pathCountOne()).isEqualTo(BigInteger.ZERO);
        assertThat(this.bddFalsum.pathCountZero()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddPosLit.pathCountOne()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddPosLit.pathCountZero()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddNegLit.pathCountOne()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddNegLit.pathCountZero()).isEqualTo(BigInteger.ONE);
        assertThat(this.bddImpl.pathCountOne()).isEqualTo(BigInteger.valueOf(2));
        assertThat(this.bddImpl.pathCountZero()).isEqualTo(BigInteger.valueOf(1));
        assertThat(this.bddEquiv.pathCountOne()).isEqualTo(BigInteger.valueOf(2));
        assertThat(this.bddEquiv.pathCountZero()).isEqualTo(BigInteger.valueOf(2));
        assertThat(this.bddOr.pathCountOne()).isEqualTo(BigInteger.valueOf(3));
        assertThat(this.bddOr.pathCountZero()).isEqualTo(BigInteger.valueOf(1));
        assertThat(this.bddAnd.pathCountOne()).isEqualTo(BigInteger.valueOf(1));
        assertThat(this.bddAnd.pathCountZero()).isEqualTo(BigInteger.valueOf(3));
    }

    @Test
    public void testSupport() {
        assertThat(this.bddVerum.support()).isEqualTo(new TreeSet<>());
        assertThat(this.bddFalsum.support()).isEqualTo(new TreeSet<>());
        assertThat(this.bddPosLit.support()).isEqualTo(new TreeSet<>(Collections.singletonList(this.f.variable("A"))));
        assertThat(this.bddNegLit.support()).isEqualTo(new TreeSet<>(Collections.singletonList(this.f.variable("A"))));
        assertThat(this.bddImpl.support()).isEqualTo(new TreeSet<>(Arrays.asList(this.f.variable("A"), this.f.variable("B"))));
        assertThat(this.bddEquiv.support()).isEqualTo(new TreeSet<>(Arrays.asList(this.f.variable("A"), this.f.variable("B"))));
        assertThat(this.bddOr.support()).isEqualTo(new TreeSet<>(Arrays.asList(this.f.variable("A"), this.f.variable("B"), this.f.variable("C"))));
        assertThat(this.bddAnd.support()).isEqualTo(new TreeSet<>(Arrays.asList(this.f.variable("A"), this.f.variable("B"), this.f.variable("C"))));
    }

    @Test
    public void testNodeCount() {
        assertThat(this.bddVerum.nodeCount()).isEqualTo(0);
        assertThat(this.bddFalsum.nodeCount()).isEqualTo(0);
        assertThat(this.bddPosLit.nodeCount()).isEqualTo(1);
        assertThat(this.bddNegLit.nodeCount()).isEqualTo(1);
        assertThat(this.bddImpl.nodeCount()).isEqualTo(2);
        assertThat(this.bddEquiv.nodeCount()).isEqualTo(3);
        assertThat(this.bddOr.nodeCount()).isEqualTo(3);
        assertThat(this.bddAnd.nodeCount()).isEqualTo(3);
    }

    @Test
    public void testVariableProfile() {
        final Variable a = this.f.variable("A");
        final Variable b = this.f.variable("B");
        final Variable c = this.f.variable("C");
        final Map.Entry<Variable, Integer> a0 = new AbstractMap.SimpleEntry<>(a, 0);
        final Map.Entry<Variable, Integer> a1 = new AbstractMap.SimpleEntry<>(a, 1);
        final Map.Entry<Variable, Integer> b0 = new AbstractMap.SimpleEntry<>(b, 0);
        final Map.Entry<Variable, Integer> b1 = new AbstractMap.SimpleEntry<>(b, 1);
        final Map.Entry<Variable, Integer> b2 = new AbstractMap.SimpleEntry<>(b, 2);
        final Map.Entry<Variable, Integer> c0 = new AbstractMap.SimpleEntry<>(c, 0);
        final Map.Entry<Variable, Integer> c1 = new AbstractMap.SimpleEntry<>(c, 1);
        assertThat(this.bddVerum.variableProfile()).containsExactly(a0, b0, c0);
        assertThat(this.bddFalsum.variableProfile()).containsExactly(a0, b0, c0);
        assertThat(this.bddPosLit.variableProfile()).containsExactly(a1, b0, c0);
        assertThat(this.bddNegLit.variableProfile()).containsExactly(a1, b0, c0);
        assertThat(this.bddImpl.variableProfile()).containsExactly(a1, b1, c0);
        assertThat(this.bddEquiv.variableProfile()).containsExactly(a1, b2, c0);
        assertThat(this.bddOr.variableProfile()).containsExactly(a1, b1, c1);
        assertThat(this.bddAnd.variableProfile()).containsExactly(a1, b1, c1);
    }
}
