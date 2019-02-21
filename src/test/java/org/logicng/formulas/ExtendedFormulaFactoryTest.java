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

import org.assertj.core.api.JUnitSoftAssertions;
import org.junit.Rule;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.testutils.PigeonHoleGenerator;
import org.logicng.transformations.cnf.CNFFactorization;
import org.logicng.transformations.dnf.DNFFactorization;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for the class {@link ExtendedFormulaFactory}.
 * @version 1.3
 * @since 1.2
 */
public class ExtendedFormulaFactoryTest {

  @Rule
  public final JUnitSoftAssertions softly = new JUnitSoftAssertions();


  @Test
  public void testLoad() {
    ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    Variable a = eff.variable("A");
    int preSaveFormulaAmount = eff.statistics().formulas();
    FormulaFactoryState state = eff.save();
    Variable b = eff.variable("B");
    And and = (And) eff.and(a, b);
    softly.assertThat(eff.posLiterals).containsValue(b);
    softly.assertThat(eff.ands2).containsValue(and);

    eff.load(state);

    softly.assertThat(eff.statistics().formulas()).isEqualTo(preSaveFormulaAmount);
    softly.assertThat(eff.posLiterals).containsValue(a);
    softly.assertThat(eff.posLiterals).doesNotContainValue(b);
    softly.assertThat(eff.negLiterals).isEmpty();
    softly.assertThat(eff.ands2).isEmpty();
  }

  @Test
  public void testPigenholeSize() {
    ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    PigeonHoleGenerator phg = new PigeonHoleGenerator(eff);
    phg.generate(50, "a");
    int aFormulaAmount = eff.statistics().formulas();
    FormulaFactoryState stateA = eff.save();

    phg.generate(75, "b");
    eff.load(stateA);
    softly.assertThat(eff.statistics().formulas()).isEqualTo(aFormulaAmount);

    phg.generate(60, "c");
    int cFormulaAmount = eff.statistics().formulas();
    FormulaFactoryState stateC = eff.save();

    phg.generate(30, "d");
    int dFormulaAmount = eff.statistics().formulas();
    eff.load(stateC);
    softly.assertThat(eff.statistics().formulas()).isEqualTo(cFormulaAmount);

    eff.load(stateA);
    softly.assertThat(eff.statistics().formulas()).isEqualTo(aFormulaAmount);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalLoad() {
    ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    eff.variable("A");
    FormulaFactoryState state0 = eff.save();
    eff.variable("B");
    FormulaFactoryState state1 = eff.save();
    eff.load(state0);
    eff.load(state1);
  }

  @Test
  public void testCNFFactorization() {
    testCacheClearance(new CNFFactorization(), PredicateCacheEntry.IS_CNF, TransformationCacheEntry.FACTORIZED_CNF);
  }

  @Test
  public void testDNFFactorization() {
    testCacheClearance(new DNFFactorization(), PredicateCacheEntry.IS_DNF, TransformationCacheEntry.FACTORIZED_DNF);
  }

  private void testCacheClearance(FormulaTransformation transformation, PredicateCacheEntry predicateCacheEntry, TransformationCacheEntry transformationCacheEntry) {
    final ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    final List<Formula> formulas = initializeFormulaFactoryWithFormulas(eff);
    final FormulaFactoryState state = eff.save();
    assertThat(state.toString()).isEqualTo("FormulaFactoryState{id=0, state=[4, 4, 0, 6, 4, 5, 3, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0]}");
    for (Formula formula : formulas) {
      transformation.apply(formula, true);
      softly.assertThat((formula.predicateCacheEntry(predicateCacheEntry) != null && formula.predicateCacheEntry(predicateCacheEntry).equals(Tristate.TRUE)) || formula.transformationCacheEntry(transformationCacheEntry) != null).as("CacheClearanceTest for " + formula.toString() + " type: " + transformationCacheEntry).isTrue();
    }
    eff.load(state);
    for (Formula formula : formulas) {
      softly.assertThat(formula.transformationCacheEntry(transformationCacheEntry)).isNull();
      softly.assertThat(formula.transformationCache).isEmpty();
    }
  }

  private List<Formula> initializeFormulaFactoryWithFormulas(FormulaFactory f) {
    List<Formula> result = new ArrayList<>();

    // Literals
    Variable A = f.variable("a");
    Variable B = f.variable("b");
    Variable X = f.variable("x");
    Variable Y = f.variable("y");
    Literal NA = f.literal("a", false);
    Literal NB = f.literal("b", false);
    Literal NX = f.literal("x", false);
    Literal NY = f.literal("y", false);

    // Disjunctions
    Formula OR1 = f.or(X, Y);
    result.add(OR1);
    Formula OR2 = f.or(NX, NY);
    result.add(OR2);
    result.add(f.or(f.and(A, B), f.and(NA, NB)));

    // Conjunctions
    Formula AND1 = f.and(A, B);
    result.add(AND1);
    result.add(f.and(NA, NB));
    result.add(f.and(OR1, OR2));

    // Negations
    result.add(f.not(AND1));
    result.add(f.not(OR1));

    // Implications
    Formula IMP1 = f.implication(A, B);
    result.add(IMP1);
    Formula IMP2 = f.implication(NA, NB);
    result.add(IMP2);
    result.add(f.implication(AND1, OR1));
    result.add(f.implication(f.equivalence(A, B), f.equivalence(NX, NY)));

    // Equivalences
    result.add(f.equivalence(A, B));
    result.add(f.equivalence(NA, NB));
    result.add(f.equivalence(AND1, OR1));
    result.add(f.equivalence(IMP1, IMP2));

    // PBCs
    Literal[] literals = new Literal[]{A, B, X};
    int[] coefficients = new int[]{2, -4, 3};
    result.add(f.pbc(CType.EQ, 2, literals, coefficients));
    result.add(f.pbc(CType.GT, 2, literals, coefficients));
    result.add(f.pbc(CType.GE, 2, literals, coefficients));
    result.add(f.pbc(CType.LT, 2, literals, coefficients));
    result.add(f.pbc(CType.LE, 2, literals, coefficients));

    return result;
  }
}
