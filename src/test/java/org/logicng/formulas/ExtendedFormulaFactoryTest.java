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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.data.MapEntry.entry;

import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.io.parsers.ParserException;
import org.logicng.testutils.PigeonHoleGenerator;
import org.logicng.transformations.cnf.CNFFactorization;
import org.logicng.transformations.cnf.PlaistedGreenbaumTransformation;
import org.logicng.transformations.dnf.DNFFactorization;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.TreeMap;

/**
 * Unit tests for the class {@link ExtendedFormulaFactory}.
 * @version 2.0.0
 * @since 1.2
 */
public class ExtendedFormulaFactoryTest {

    @Test
    public void testShrinkMapWithWrongArguments() {
        assertThatThrownBy(() -> ExtendedFormulaFactory.shrinkMap(new TreeMap<>(), 4))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot shrink a map which is not of type LinkedHashMap");
        assertThatThrownBy(() -> ExtendedFormulaFactory.shrinkMap(new LinkedHashMap<>(), 4))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot shrink a map of size 0 to new size 4");
    }

    @Test
    public void testShrinkMap() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("a", "A");
        map.put("b", "B");
        map.put("c", "C");
        ExtendedFormulaFactory.shrinkMap(map, 2);
        assertThat(map).containsExactly(entry("a", "A"), entry("b", "B"));
        map.put("c", "C");
        map.put("d", "D");
        ExtendedFormulaFactory.shrinkMap(map, 1);
        assertThat(map).containsExactly(entry("a", "A"));
        ExtendedFormulaFactory.shrinkMap(map, 0);
        assertThat(map).isEmpty();
    }

    @Test
    public void testShrinkSetWithWrongArguments() {
        assertThatThrownBy(() -> ExtendedFormulaFactory.shrinkSet(new HashSet<>(), 4))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot shrink a set which is not of type LinkedHashSet");
        assertThatThrownBy(() -> ExtendedFormulaFactory.shrinkSet(new LinkedHashSet<>(), 4))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot shrink a set of size 0 to new size 4");
    }

    @Test
    public void testShrinkSet() {
        LinkedHashSet<String> set = new LinkedHashSet<>();
        set.add("a");
        set.add("b");
        set.add("c");
        ExtendedFormulaFactory.shrinkSet(set, 2);
        assertThat(set).containsExactly("a", "b");
        set.add("c");
        set.add("d");
        ExtendedFormulaFactory.shrinkSet(set, 1);
        assertThat(set).containsExactly("a");
        ExtendedFormulaFactory.shrinkSet(set, 0);
        assertThat(set).isEmpty();
    }

    @Test
    public void testLoad01() {
        final SoftAssertions softly = new SoftAssertions();
        final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
        final Variable a = eff.variable("A");
        final int preSaveFormulaAmount = eff.statistics().formulas();
        final FormulaFactoryState state = eff.save();
        final Variable b = eff.variable("B");
        final And and = (And) eff.and(a, b);
        softly.assertThat(eff.posLiterals).containsValue(b);
        softly.assertThat(eff.ands2).containsValue(and);

        eff.load(state);

        softly.assertThat(eff.statistics().formulas()).isEqualTo(preSaveFormulaAmount);
        softly.assertThat(eff.posLiterals).containsValue(a);
        softly.assertThat(eff.posLiterals).doesNotContainValue(b);
        softly.assertThat(eff.negLiterals).isEmpty();
        softly.assertThat(eff.ands2).isEmpty();
        softly.assertAll();
    }

    @Test
    public void testLoad02() throws ParserException {
        final SoftAssertions softly = new SoftAssertions();
        final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
        final Variable a = eff.variable("A");
        final int preSaveFormulaAmount = eff.statistics().formulas();
        final FormulaFactoryState state01 = eff.save();
        final Variable b = eff.variable("B");
        final Variable c = eff.variable("C");
        final Variable d = eff.variable("D");
        final Variable e = eff.variable("E");
        final And and = (And) eff.and(a, b);
        final And and3 = (And) eff.and(a, b, c);
        final And and4 = (And) eff.and(a, b, c, d);
        final And and5 = (And) eff.and(a, b, c, d, e);
        final Or or2 = (Or) eff.or(a, b);
        final Or or3 = (Or) eff.or(a, b, c);
        final Or or4 = (Or) eff.or(a, b, c, d);
        final Or or5 = (Or) eff.or(a, b, c, d, e);
        eff.parse("A | B & C").transform(new PlaistedGreenbaumTransformation(0));
        softly.assertThat(eff.posLiterals).containsValue(b);
        softly.assertThat(eff.ands2).containsValue(and);
        softly.assertThat(eff.ands3).containsValue(and3);
        softly.assertThat(eff.ands4).containsValue(and4);
        softly.assertThat(eff.andsN).containsValue(and5);
        softly.assertThat(eff.ors2).containsValue(or2);
        softly.assertThat(eff.ors3).containsValue(or3);
        softly.assertThat(eff.ors4).containsValue(or4);
        softly.assertThat(eff.orsN).containsValue(or5);

        final FormulaFactoryState state02 = eff.save();
        eff.load(state02);

        softly.assertThat(eff.posLiterals).containsValue(b);
        softly.assertThat(eff.ands2).containsValue(and);
        softly.assertThat(eff.ands3).containsValue(and3);
        softly.assertThat(eff.ands4).containsValue(and4);
        softly.assertThat(eff.andsN).containsValue(and5);
        softly.assertThat(eff.ors2).containsValue(or2);
        softly.assertThat(eff.ors3).containsValue(or3);
        softly.assertThat(eff.ors4).containsValue(or4);
        softly.assertThat(eff.orsN).containsValue(or5);

        eff.load(state01);

        softly.assertThat(eff.statistics().formulas()).isEqualTo(preSaveFormulaAmount);
        softly.assertThat(eff.posLiterals).containsValue(a);
        softly.assertThat(eff.posLiterals).doesNotContainValue(b);
        softly.assertThat(eff.negLiterals).isEmpty();
        softly.assertThat(eff.ands2).isEmpty();
        softly.assertThat(eff.ands3).isEmpty();
        softly.assertThat(eff.ands4).isEmpty();
        softly.assertThat(eff.andsN).isEmpty();
        softly.assertThat(eff.ors2).isEmpty();
        softly.assertThat(eff.ors3).isEmpty();
        softly.assertThat(eff.ors4).isEmpty();
        softly.assertThat(eff.orsN).isEmpty();
        softly.assertAll();
    }

    @Test
    public void testPigenholeSize() {
        final SoftAssertions softly = new SoftAssertions();
        final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
        final PigeonHoleGenerator phg = new PigeonHoleGenerator(eff);
        phg.generate(50, "a");
        final int aFormulaAmount = eff.statistics().formulas();
        final FormulaFactoryState stateA = eff.save();

        phg.generate(75, "b");
        eff.load(stateA);
        softly.assertThat(eff.statistics().formulas()).isEqualTo(aFormulaAmount);

        phg.generate(60, "c");
        final int cFormulaAmount = eff.statistics().formulas();
        final FormulaFactoryState stateC = eff.save();

        phg.generate(30, "d");
        eff.load(stateC);
        softly.assertThat(eff.statistics().formulas()).isEqualTo(cFormulaAmount);

        eff.load(stateA);
        softly.assertThat(eff.statistics().formulas()).isEqualTo(aFormulaAmount);
        softly.assertAll();
    }

    @Test
    public void testIllegalLoad() {
        final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
        eff.variable("A");
        final FormulaFactoryState state0 = eff.save();
        eff.variable("B");
        final FormulaFactoryState state1 = eff.save();
        eff.load(state0);
        assertThatThrownBy(() -> eff.load(state1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testCNFFactorization() {
        testCacheClearance(new CNFFactorization(), PredicateCacheEntry.IS_CNF, TransformationCacheEntry.FACTORIZED_CNF);
    }

    @Test
    @LongRunningTag
    public void testDNFFactorization() {
        testCacheClearance(new DNFFactorization(), PredicateCacheEntry.IS_DNF, TransformationCacheEntry.FACTORIZED_DNF);
    }

    private void testCacheClearance(final FormulaTransformation transformation, final PredicateCacheEntry predicateCacheEntry, final TransformationCacheEntry transformationCacheEntry) {
        final SoftAssertions softly = new SoftAssertions();
        final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
        final List<Formula> formulas = initializeFormulaFactoryWithFormulas(eff);
        final FormulaFactoryState state = eff.save();
        assertThat(state.toString()).isEqualTo("FormulaFactoryState{id=0, state=[4, 4, 0, 5, 4, 5, 3, 0, 0, 0, 3, 0, 0, 0, 5, 2, 0, 0, 0]}");
        for (final Formula formula : formulas) {
            transformation.apply(formula, true);
            softly.assertThat((formula.predicateCacheEntry(predicateCacheEntry) != null && formula.predicateCacheEntry(predicateCacheEntry).equals(Tristate.TRUE)) || formula.transformationCacheEntry(transformationCacheEntry) != null).as("CacheClearanceTest for " + formula.toString() + " type: " + transformationCacheEntry).isTrue();
        }
        eff.load(state);
        for (final Formula formula : formulas) {
            softly.assertThat(formula.transformationCacheEntry(transformationCacheEntry)).isNull();
            softly.assertThat(formula.transformationCache).isEmpty();
        }
        softly.assertAll();
    }

    private List<Formula> initializeFormulaFactoryWithFormulas(final FormulaFactory f) {
        final List<Formula> result = new ArrayList<>();

        // Literals
        final Variable A = f.variable("a");
        final Variable B = f.variable("b");
        final Variable X = f.variable("x");
        final Variable Y = f.variable("y");
        final Literal NA = f.literal("a", false);
        final Literal NB = f.literal("b", false);
        final Literal NX = f.literal("x", false);
        final Literal NY = f.literal("y", false);

        // Disjunctions
        final Formula OR1 = f.or(X, Y);
        result.add(OR1);
        final Formula OR2 = f.or(NX, NY);
        result.add(OR2);
        result.add(f.or(f.and(A, B), f.and(NA, NB)));

        // Conjunctions
        final Formula AND1 = f.and(A, B);
        result.add(AND1);
        result.add(f.and(NA, NB));
        result.add(f.and(OR1, OR2));

        // Negations
        result.add(f.not(AND1));
        result.add(f.not(OR1));

        // Implications
        final Formula IMP1 = f.implication(A, B);
        result.add(IMP1);
        final Formula IMP2 = f.implication(NA, NB);
        result.add(IMP2);
        result.add(f.implication(AND1, OR1));
        result.add(f.implication(f.equivalence(A, B), f.equivalence(NX, NY)));

        // Equivalences
        result.add(f.equivalence(A, B));
        result.add(f.equivalence(NA, NB));
        result.add(f.equivalence(AND1, OR1));
        result.add(f.equivalence(IMP1, IMP2));

        // PBCs
        final Literal[] literals = new Literal[]{A, B, X};
        final int[] coefficients = new int[]{2, -4, 3};
        result.add(f.pbc(CType.EQ, 2, literals, coefficients));
        result.add(f.pbc(CType.GT, 2, literals, coefficients));
        result.add(f.pbc(CType.GE, 2, literals, coefficients));
        result.add(f.pbc(CType.LT, 2, literals, coefficients));
        result.add(f.pbc(CType.LE, 2, literals, coefficients));

        // CCs
        final Variable[] variables = new Variable[]{A, X};
        result.add(f.cc(CType.EQ, 2, variables));
        result.add(f.cc(CType.LT, 2, variables));

        return result;
    }
}
