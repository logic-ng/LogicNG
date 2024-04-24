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

package org.logicng.predicates;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.HashMap;
import java.util.stream.Collectors;

/**
 * Unit Tests for the class {@link EvaluatesToConstantPredicate}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class EvaluatesToConstantPredicateTest extends TestWithExampleFormulas {

    private final EvaluatesToConstantPredicate emptyToFalse;
    private final EvaluatesToConstantPredicate aToFalse;
    private final EvaluatesToConstantPredicate aNotBToFalse;

    private final EvaluatesToConstantPredicate emptyToTrue;
    private final EvaluatesToConstantPredicate aToTrue;
    private final EvaluatesToConstantPredicate aNotBToTrue;

    public EvaluatesToConstantPredicateTest() {
        this.emptyToFalse = new EvaluatesToConstantPredicate(false, new HashMap<>());
        this.emptyToTrue = new EvaluatesToConstantPredicate(true, new HashMap<>());

        final HashMap<Variable, Boolean> aMap = new HashMap<>();
        aMap.put(this.A, true);
        this.aToFalse = new EvaluatesToConstantPredicate(false, aMap);
        this.aToTrue = new EvaluatesToConstantPredicate(true, aMap);

        final HashMap<Variable, Boolean> aNotBMap = new HashMap<>();
        aNotBMap.put(this.A, true);
        aNotBMap.put(this.B, false);
        this.aNotBToFalse = new EvaluatesToConstantPredicate(false, aNotBMap);
        this.aNotBToTrue = new EvaluatesToConstantPredicate(true, aNotBMap);
    }

    @Test
    public void getMapping() {
        assertThat(this.emptyToFalse.getMapping()).containsExactly();
        assertThat(this.aToFalse.getMapping()).containsExactly(entry(this.A, true));
        assertThat(this.aNotBToFalse.getMapping()).containsExactly(entry(this.A, true), entry(this.B, false));

        assertThat(this.emptyToTrue.getMapping()).containsExactly();
        assertThat(this.aToTrue.getMapping()).containsExactly(entry(this.A, true));
        assertThat(this.aNotBToTrue.getMapping()).containsExactly(entry(this.A, true), entry(this.B, false));
    }

    @Test
    public void testConstantsToFalse() {
        assertThat(this.f.falsum().holds(this.emptyToFalse)).isTrue();
        assertThat(this.f.falsum().holds(this.aToFalse)).isTrue();
        assertThat(this.f.falsum().holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.verum().holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.verum().holds(this.aToFalse)).isFalse();
        assertThat(this.f.verum().holds(this.aNotBToFalse)).isFalse();
    }

    @Test
    public void testLiteralsToFalse() {
        assertThat(this.A.holds(this.emptyToFalse)).isFalse();
        assertThat(this.A.holds(this.aToFalse)).isFalse();
        assertThat(this.A.holds(this.aNotBToFalse)).isFalse();

        assertThat(this.NA.holds(this.emptyToFalse)).isFalse();
        assertThat(this.NA.holds(this.aToFalse)).isTrue();
        assertThat(this.NA.holds(this.aNotBToFalse)).isTrue();

        assertThat(this.B.holds(this.emptyToFalse)).isFalse();
        assertThat(this.B.holds(this.aToFalse)).isFalse();
        assertThat(this.B.holds(this.aNotBToFalse)).isTrue();

        assertThat(this.NB.holds(this.emptyToFalse)).isFalse();
        assertThat(this.NB.holds(this.aToFalse)).isFalse();
        assertThat(this.NB.holds(this.aNotBToFalse)).isFalse();
    }

    @Test
    public void testNotToFalse() throws ParserException {
        assertThat(this.f.parse("~~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~~a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~~a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~~~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~~~a").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~~~a").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~(a & b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~(a & b)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~(a & b)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~(~a & b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~(~a & b)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~(~a & b)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~(a & ~b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~(a & ~b)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~(a & ~b)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~(~a & ~b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~(~a & ~b)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~(~a & ~b)").holds(this.aNotBToFalse)).isFalse();
    }

    @Test
    public void testAndToFalse() throws ParserException {
        assertThat(this.f.parse("a & ~a").holds(this.emptyToFalse)).isTrue();
        assertThat(this.f.parse("a & ~a").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("a & ~a").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("a & b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a & b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & b").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("a & ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a & ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & ~b").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & ~b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.aNotBToFalse)).isTrue();
    }

    @Test
    public void testOrToFalse() throws ParserException {
        assertThat(this.f.parse("a | b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a | b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a | b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a | b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a | b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a | b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("a | ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a | ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a | ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a | ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a | ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a | ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.aNotBToFalse)).isFalse();
    }

    @Test
    public void testImplicationToFalse() throws ParserException {
        assertThat(this.f.parse("a => a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a => a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a => a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("b => b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b => b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b => b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a => b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a => b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a => b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a => b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a => b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a => b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a => ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a => ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a => ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a => ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a => ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a => ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("b => a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b => a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b => a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~b => a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~b => a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~b => a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("b => ~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b => ~a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b => ~a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~b => ~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~b => ~a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~b => ~a").holds(this.aNotBToFalse)).isTrue();
    }

    @Test
    public void testEquivalenceToFalse() throws ParserException {
        assertThat(this.f.parse("a <=> a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a <=> a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a <=> a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("b <=> b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b <=> b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b <=> b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a <=> b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a <=> b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a <=> b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a <=> b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a <=> b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a <=> b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a <=> ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a <=> ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a <=> ~b").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~a <=> ~b").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a <=> ~b").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~a <=> ~b").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("b <=> a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b <=> a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b <=> a").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~b <=> a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~b <=> a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~b <=> a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("b <=> ~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("b <=> ~a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("b <=> ~a").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("~b <=> ~a").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~b <=> ~a").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("~b <=> ~a").holds(this.aNotBToFalse)).isTrue();
    }

    @Test
    public void testPBCToFalse() {
        final PBConstraint pbc01 = (PBConstraint) this.f.pbc(CType.EQ, 2, new Literal[]{this.A, this.B}, new int[]{2, -4});
        assertThat(pbc01.holds(this.emptyToFalse)).isFalse();
        assertThat(pbc01.holds(this.aToFalse)).isFalse();
        assertThat(pbc01.holds(this.aNotBToFalse)).isFalse();

        final PBConstraint pbc02 = (PBConstraint) this.f.pbc(CType.GT, 2, new Literal[]{this.B, this.C}, new int[]{2, 1});
        assertThat(pbc02.holds(this.emptyToFalse)).isFalse();
        assertThat(pbc02.holds(this.aToFalse)).isFalse();
        assertThat(pbc02.holds(this.aNotBToFalse)).isTrue();

        assertThat(this.PBC1.holds(this.emptyToFalse)).isFalse();
        assertThat(this.PBC1.holds(this.aToFalse)).isFalse();
        assertThat(this.PBC1.holds(this.aNotBToFalse)).isFalse();

        assertThat(this.PBC2.holds(this.emptyToFalse)).isFalse();
        assertThat(this.PBC2.holds(this.aToFalse)).isFalse();
        assertThat(this.PBC2.holds(this.aNotBToFalse)).isFalse();
    }

    @Test
    public void testMixedToFalse() throws ParserException {
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~b & (b | ~a)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.aToFalse)).isTrue();
        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.aNotBToFalse)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.aNotBToFalse)).isTrue();

        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.emptyToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.aToFalse)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.aNotBToFalse)).isTrue();
    }

    @Test
    public void testConstantsToTrue() {
        assertThat(this.f.falsum().holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.falsum().holds(this.aToTrue)).isFalse();
        assertThat(this.f.falsum().holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.verum().holds(this.emptyToTrue)).isTrue();
        assertThat(this.f.verum().holds(this.aToTrue)).isTrue();
        assertThat(this.f.verum().holds(this.aNotBToTrue)).isTrue();
    }

    @Test
    public void testLiteralsToTrue() {
        assertThat(this.A.holds(this.emptyToTrue)).isFalse();
        assertThat(this.A.holds(this.aToTrue)).isTrue();
        assertThat(this.A.holds(this.aNotBToTrue)).isTrue();

        assertThat(this.NA.holds(this.emptyToTrue)).isFalse();
        assertThat(this.NA.holds(this.aToTrue)).isFalse();
        assertThat(this.NA.holds(this.aNotBToTrue)).isFalse();

        assertThat(this.B.holds(this.emptyToTrue)).isFalse();
        assertThat(this.B.holds(this.aToTrue)).isFalse();
        assertThat(this.B.holds(this.aNotBToTrue)).isFalse();

        assertThat(this.NB.holds(this.emptyToTrue)).isFalse();
        assertThat(this.NB.holds(this.aToTrue)).isFalse();
        assertThat(this.NB.holds(this.aNotBToTrue)).isTrue();
    }

    @Test
    public void testNotToTrue() throws ParserException {
        assertThat(this.f.parse("~~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~~a").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~~a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~~~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~~~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~~~a").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~(a & b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~(a & b)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~(a & b)").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~(~a & b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~(~a & b)").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~(~a & b)").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~(a & ~b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~(a & ~b)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~(a & ~b)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~(~a & ~b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~(~a & ~b)").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~(~a & ~b)").holds(this.aNotBToTrue)).isTrue();
    }

    @Test
    public void testAndToTrue() throws ParserException {
        assertThat(this.f.parse("a & ~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & ~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & ~a").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a & b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a & ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~b & c & ~d").holds(this.aNotBToTrue)).isFalse();
    }

    @Test
    public void testOrToTrue() throws ParserException {
        assertThat(this.f.parse("a | b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a | b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("a | b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a | b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a | b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a | b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a | ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a | ~b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("a | ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a | ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a | ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a | ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a | ~b | c | ~d").holds(this.aNotBToTrue)).isTrue();
    }

    @Test
    public void testImplicationToTrue() throws ParserException {
        assertThat(this.f.parse("a => a").holds(this.emptyToTrue)).isTrue();
        assertThat(this.f.parse("a => a").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("a => a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("b => b").holds(this.emptyToTrue)).isTrue();
        assertThat(this.f.parse("b => b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("b => b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("a => b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a => b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a => b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a => b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a => b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~a => b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("a => ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a => ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a => ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a => ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a => ~b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~a => ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("b => a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("b => a").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("b => a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~b => a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b => a").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("~b => a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("b => ~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("b => ~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("b => ~a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~b => ~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b => ~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~b => ~a").holds(this.aNotBToTrue)).isFalse();
    }

    @Test
    public void testEquivalenceToTrue() throws ParserException {
        assertThat(this.f.parse("a <=> a").holds(this.emptyToTrue)).isTrue();
        assertThat(this.f.parse("a <=> a").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("a <=> a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("b <=> b").holds(this.emptyToTrue)).isTrue();
        assertThat(this.f.parse("b <=> b").holds(this.aToTrue)).isTrue();
        assertThat(this.f.parse("b <=> b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("a <=> b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a <=> b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a <=> b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a <=> b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a <=> b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a <=> b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("a <=> ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a <=> ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a <=> ~b").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~a <=> ~b").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a <=> ~b").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a <=> ~b").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("b <=> a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("b <=> a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("b <=> a").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~b <=> a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b <=> a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~b <=> a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("b <=> ~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("b <=> ~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("b <=> ~a").holds(this.aNotBToTrue)).isTrue();

        assertThat(this.f.parse("~b <=> ~a").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b <=> ~a").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~b <=> ~a").holds(this.aNotBToTrue)).isFalse();
    }

    @Test
    public void testPBCToTrue() {
        final PBConstraint pbc01 = (PBConstraint) this.f.pbc(CType.EQ, 2, new Literal[]{this.A, this.B}, new int[]{2, -4});
        assertThat(pbc01.holds(this.emptyToTrue)).isFalse();
        assertThat(pbc01.holds(this.aToTrue)).isFalse();
        assertThat(pbc01.holds(this.aNotBToTrue)).isTrue();

        final PBConstraint pbc02 = (PBConstraint) this.f.pbc(CType.GT, 2, new Literal[]{this.B, this.C}, new int[]{2, 1});
        assertThat(pbc02.holds(this.emptyToTrue)).isFalse();
        assertThat(pbc02.holds(this.aToTrue)).isFalse();
        assertThat(pbc02.holds(this.aNotBToTrue)).isFalse();

        assertThat(this.PBC1.holds(this.emptyToTrue)).isFalse();
        assertThat(this.PBC1.holds(this.aToTrue)).isFalse();
        assertThat(this.PBC1.holds(this.aNotBToTrue)).isFalse();

        assertThat(this.PBC2.holds(this.emptyToTrue)).isFalse();
        assertThat(this.PBC2.holds(this.aToTrue)).isFalse();
        assertThat(this.PBC2.holds(this.aNotBToTrue)).isFalse();
    }

    @Test
    public void testMixedToTrue() throws ParserException {
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~b & (b | ~a)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a & (a | ~b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~b & (b | ~a)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~b & (b | ~a)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & (a | ~b) & c & (a => b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a => b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b | e)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (a <=> b)").holds(this.aNotBToTrue)).isFalse();

        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.emptyToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.aToTrue)).isFalse();
        assertThat(this.f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(this.aNotBToTrue)).isFalse();
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            final Assignment assignment = new Assignment();
            assignment.addLiteral(f.literal("v0", false));
            assignment.addLiteral(f.literal("v1", false));
            assignment.addLiteral(f.literal("v2", true));
            assignment.addLiteral(f.literal("v3", true));
            final EvaluatesToConstantPredicate falseEvaluation = new EvaluatesToConstantPredicate(false,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final EvaluatesToConstantPredicate trueEvaluation = new EvaluatesToConstantPredicate(true,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final Formula restricted = formula.restrict(assignment);
            assertThat(restricted.type() == FType.FALSE).isEqualTo(formula.holds(falseEvaluation));
            assertThat(restricted.type() == FType.TRUE).isEqualTo(formula.holds(trueEvaluation));
        }
    }

    @Test
    @RandomTag
    public void testRandom() {
        for (int i = 0; i < 1000; i++) {
            final FormulaFactory f = new FormulaFactory();
            final Assignment assignment = new Assignment();
            assignment.addLiteral(f.literal("v0", false));
            assignment.addLiteral(f.literal("v1", false));
            assignment.addLiteral(f.literal("v2", true));
            assignment.addLiteral(f.literal("v3", true));
            final EvaluatesToConstantPredicate falseEvaluation = new EvaluatesToConstantPredicate(false,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final EvaluatesToConstantPredicate trueEvaluation = new EvaluatesToConstantPredicate(true,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(1).seed(i * 42).build());
            final Formula formula = randomizer.formula(6);
            final Formula restricted = formula.restrict(assignment);
            assertThat(restricted.type() == FType.FALSE).isEqualTo(formula.holds(falseEvaluation));
            assertThat(restricted.type() == FType.TRUE).isEqualTo(formula.holds(trueEvaluation));
        }
    }
}
