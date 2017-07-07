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
//  Copyright 2015-2017 Christoph Zengler                                //
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
import org.logicng.transformations.cnf.CNFFactorization;
import org.logicng.transformations.dnf.DNFFactorization;

import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for the class {@link ExtendedFormulaFactory}.
 * @version 1.2
 * @since 1.2
 */
public class ExtendedFormulaFactoryTest {

  @Rule
  public final JUnitSoftAssertions softly = new JUnitSoftAssertions();


  @Test
  public void testLoad() {
    ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    Variable a = eff.variable("A");
    FormulaFactoryState state = eff.save();
    Variable b = eff.variable("B");
    And and = (And) eff.and(a, b);
    softly.assertThat(eff.posLiterals).containsValue(b);
    softly.assertThat(eff.ands2).containsValue(and);

    eff.load(state);

    softly.assertThat(eff.posLiterals).containsValue(a);
    softly.assertThat(eff.posLiterals).doesNotContainValue(b);
    softly.assertThat(eff.negLiterals).isEmpty();
    softly.assertThat(eff.ands2).isEmpty();
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
    ExtendedFormulaFactory eff = new ExtendedFormulaFactory();
    List<Formula> formulas = initializeFormulaFactoryWithFormulas(eff);
    FormulaFactoryState state = eff.save();
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
    // Constants
    //    result.add(f.verum());
    //    result.add(f.falsum());

    // Literals
    Variable A = f.variable("a");
    //    result.add(A);
    Variable B = f.variable("b");
    //    result.add(B);
    //    result.add(f.variable("c"));
    Variable X = f.variable("x");
    //    result.add(X);
    Variable Y = f.variable("y");
    //    result.add(Y);
    Literal NA = f.literal("a", false);
    //    result.add(NA);
    Literal NB = f.literal("b", false);
    //    result.add(NB);
    Literal NX = f.literal("x", false);
    //    result.add(NX);
    Literal NY = f.literal("y", false);
    //    result.add(NY);

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
