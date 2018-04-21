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

package org.logicng.transformations.qmc;

import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.DNFPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.SortedMap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.chooseSatBased;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.computePrimeImplicants;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.convertToTerm;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.generateInitialTermClasses;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.uniteInTermClasses;

/**
 * Unit tests for {@link QuineMcCluskeyAlgorithm}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskeyTest {

  @Test
  public void testSimple1() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula formula = p.parse("(~a & ~b & ~c) | (~a & ~b & c) | (~a & b & ~c) | (a & ~b & c) | (a & b & ~c) | (a & b & c)");
    final Formula dnf = QuineMcCluskeyAlgorithm.compute(formula);
    assertThat(dnf.holds(new DNFPredicate())).isTrue();
    assertThat(f.equivalence(formula, dnf).holds(new TautologyPredicate(f))).isTrue();
  }

  @Test
  public void testSimple2() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula formula = p.parse("(~a & ~b & ~c) | (~a & b & ~c) | (a & ~b & c) | (a & b & c)");
    final Formula dnf = QuineMcCluskeyAlgorithm.compute(formula);
    assertThat(dnf.holds(new DNFPredicate())).isTrue();
    assertThat(f.equivalence(formula, dnf).holds(new TautologyPredicate(f))).isTrue();
  }

  @Test
  public void testLong1() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula formula = p.parse("A => B & C & ~((D | E | F | G | H | I | J) & ~K) & L");
    final Formula dnf = QuineMcCluskeyAlgorithm.compute(formula);
    assertThat(dnf.holds(new DNFPredicate())).isTrue();
    assertThat(f.equivalence(formula, dnf).holds(new TautologyPredicate(f))).isTrue();
  }

  @Test
  public void testConvertToTerm() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final List<Literal> minterm1 = Arrays.asList(f.literal("A", true), f.literal("B", true), f.literal("C", true));
    final List<Literal> minterm2 = Arrays.asList(f.literal("A", true), f.literal("B", false), f.literal("C", true));
    final List<Literal> minterm3 = Arrays.asList(f.literal("A", false), f.literal("B", false), f.literal("C", false));

    assertThat(convertToTerm(minterm1, f).bits()).isEqualTo(new Tristate[]{Tristate.TRUE, Tristate.TRUE, Tristate.TRUE});
    assertThat(convertToTerm(minterm2, f).bits()).isEqualTo(new Tristate[]{Tristate.TRUE, Tristate.FALSE, Tristate.TRUE});
    assertThat(convertToTerm(minterm3, f).bits()).isEqualTo(new Tristate[]{Tristate.FALSE, Tristate.FALSE, Tristate.FALSE});

    assertThat(convertToTerm(minterm1, f).minterms()).isEqualTo(Collections.singletonList(p.parse("A & B & C")));
    assertThat(convertToTerm(minterm2, f).minterms()).isEqualTo(Collections.singletonList(p.parse("A & ~B & C")));
    assertThat(convertToTerm(minterm3, f).minterms()).isEqualTo(Collections.singletonList(p.parse("~A & ~B & ~C")));

    assertThat(convertToTerm(minterm1, f).termClass()).isEqualTo(3);
    assertThat(convertToTerm(minterm2, f).termClass()).isEqualTo(2);
    assertThat(convertToTerm(minterm3, f).termClass()).isEqualTo(0);
  }

  @Test
  public void testGenerateInitialTermClasses() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term minterm1 = getTerm("A B C", f);
    final Term minterm2 = getTerm("A ~B C", f);
    final Term minterm3 = getTerm("~A ~B ~C", f);
    final Term minterm4 = getTerm("A B ~C", f);
    final Term minterm5 = getTerm("~A B C", f);
    final Term minterm6 = getTerm("~A ~B C", f);

    final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = generateInitialTermClasses(Arrays.asList(minterm1, minterm2, minterm3, minterm4, minterm5, minterm6));
    assertThat(termsInClasses).isNotNull();
    assertThat(termsInClasses).isNotEmpty();
    assertThat(termsInClasses.get(0)).containsExactly(minterm3);
    assertThat(termsInClasses.get(1)).containsExactly(minterm6);
    assertThat(termsInClasses.get(2)).containsExactly(minterm2, minterm4, minterm5);
    assertThat(termsInClasses.get(3)).containsExactly(minterm1);
  }

  /**
   * original
   * 0  -A -B -C (mt3)
   * 1  -A -B  C (mt6)
   * 2   A -B  C (mt2)
   * 2   A  B -C (mt4)
   * 2  -A  B  C (mt5)
   * 3   A  B  C (mt1)
   *
   * first round
   * 01 -A -B  x -> 1 (mt3, mt6)
   * 12  x -B  C -> 2 (mt6, mt2)
   * 12 -A  x  C -> 2 (mt6, mt5)
   * 23  A  x  C -> 3 (mt2, mt1)
   * 23  A  B  x -> 3 (mt4, mt1)
   * 23  x  B  C -> 3 (mt5, mt1)
   */
  @Test
  public void testUniteTermsInClasses() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term minterm1 = getTerm("A B C", f);
    final Term minterm2 = getTerm("A ~B C", f);
    final Term minterm3 = getTerm("~A ~B ~C", f);
    final Term minterm4 = getTerm("A B ~C", f);
    final Term minterm5 = getTerm("~A B C", f);
    final Term minterm6 = getTerm("~A ~B C", f);
    final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = generateInitialTermClasses(Arrays.asList(minterm1, minterm2, minterm3, minterm4, minterm5, minterm6));

    final SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = uniteInTermClasses(termsInClasses);
    assertThat(newTermsInClasses).isNotNull();
    assertThat(newTermsInClasses).isNotEmpty();
    assertThat(newTermsInClasses.get(0)).isNull();
    assertThat(newTermsInClasses.get(1)).containsExactly(minterm3.unite(minterm6));
    assertThat(newTermsInClasses.get(2)).containsExactly(minterm6.unite(minterm2), minterm6.unite(minterm5));
    assertThat(newTermsInClasses.get(3)).containsExactly(minterm2.unite(minterm1), minterm4.unite(minterm1), minterm5.unite(minterm1));

    assertThat(minterm1.isUsed()).isTrue();
    assertThat(minterm2.isUsed()).isTrue();
    assertThat(minterm3.isUsed()).isTrue();
    assertThat(minterm4.isUsed()).isTrue();
    assertThat(minterm5.isUsed()).isTrue();
    assertThat(minterm6.isUsed()).isTrue();
  }

  /**
   * original
   * 0 -A -B -C (mt3) *
   * 1 -A -B  C (mt6) *
   * 2  A -B  C (mt2) *
   * 2  A  B -C (mt4) *
   * 2 -A  B  C (mt5) *
   * 3  A  B  C (mt1) *
   *
   * first round
   * 0-1 -A -B  x -> 1 (mt3, mt6)
   * 1-2  x -B  C -> 2 (mt6, mt2) *
   * 1-2 -A  x  C -> 2 (mt6, mt5) *
   * 2-3  A  x  C -> 3 (mt2, mt1) *
   * 2-3  A  B  x -> 3 (mt4, mt1)
   * 2-3  x  B  C -> 3 (mt5, mt1) *
   *
   * second round
   * 01-12 /
   * 12-23 x x C (mt6, mt2, mt5, mt1)
   * 12-23 x x C (mt6, mt2, mt2, mt1)
   *
   * result
   * 0-1 -A -B  x -> 1 (mt3, mt6)
   * 2-3  A  B  x -> 3 (mt4, mt1)
   * 12-23 x x C (mt6, mt2, mt5, mt1)
   */
  @Test
  public void testComputePrimeImplicantsSimple() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term minterm1 = getTerm("A B C", f);
    final Term minterm2 = getTerm("A ~B C", f);
    final Term minterm3 = getTerm("~A ~B ~C", f);
    final Term minterm4 = getTerm("A B ~C", f);
    final Term minterm5 = getTerm("~A B C", f);
    final Term minterm6 = getTerm("~A ~B C", f);

    final LinkedHashSet<Term> primeImplicants = computePrimeImplicants(Arrays.asList(minterm1, minterm2, minterm3, minterm4, minterm5, minterm6));
    assertThat(primeImplicants).isNotNull();
    assertThat(primeImplicants).isNotEmpty();
    assertThat(primeImplicants).containsExactly(
            minterm3.unite(minterm6),
            minterm4.unite(minterm1),
            minterm6.unite(minterm2).unite(minterm5.unite(minterm1))
    );
  }

  /**
   * The example from {@link https://de.wikipedia.org/wiki/Verfahren_nach_Quine_und_McCluskey}
   */
  @Test
  public void testComputePrimeImplicantsWiki() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term m0 = getTerm("~x0 ~x1 ~x2 ~x3", f);

    final Term m1 = getTerm("~x0 ~x1 ~x2 x3", f);
    final Term m4 = getTerm("~x0 x1 ~x2 ~x3", f);
    final Term m8 = getTerm("x0 ~x1 ~x2 ~x3", f);

    final Term m5 = getTerm("~x0 x1 ~x2 x3", f);
    final Term m6 = getTerm("~x0 x1 x2 ~x3", f);
    final Term m9 = getTerm("x0 ~x1 ~x2 x3", f);

    final Term m7 = getTerm("~x0 x1 x2 x3", f);
    final Term m11 = getTerm("x0 ~x1 x2 x3", f);

    final Term m15 = getTerm("x0 x1 x2 x3", f);

    final LinkedHashSet<Term> primeImplicants = computePrimeImplicants(Arrays.asList(m0, m1, m4, m8, m5, m6, m9, m7, m11, m15));
    assertThat(primeImplicants).isNotNull();
    assertThat(primeImplicants).isNotEmpty();
    assertThat(primeImplicants).containsExactly(
            m9.unite(m11),
            m7.unite(m15),
            m11.unite(m15),
            m0.unite(m1).unite(m4.unite(m5)),
            m0.unite(m1).unite(m8.unite(m9)),
            m4.unite(m6).unite(m5.unite(m7))
    );

  }

  @Test
  public void testSatBasedSelection() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term m0 = getTerm("~a ~b ~c", f);
    final Term m1 = getTerm("~a ~b c", f);
    final Term m2 = getTerm("~a b ~c", f);
    final Term m3 = getTerm("a ~b c", f);
    final Term m4 = getTerm("a b ~c", f);
    final Term m5 = getTerm("a b c", f);
    final LinkedHashSet<Term> primeImplicants = computePrimeImplicants(Arrays.asList(m0, m1, m2, m3, m4, m5));
    final TermTable table = new TermTable(primeImplicants);
    final List<Term> terms = chooseSatBased(table, f);
    assertThat(terms).hasSize(3);
  }

  static Term getTerm(final String string, final FormulaFactory f) throws ParserException {
    final List<Literal> literals = new ArrayList<>();
    final PropositionalParser p = new PropositionalParser(f);
    for (final String var : string.split(" "))
      literals.add((Literal) p.parse(var));
    return convertToTerm(literals, f);
  }
}
