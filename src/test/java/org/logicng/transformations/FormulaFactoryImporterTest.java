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

package org.logicng.transformations;

import org.junit.Before;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.transformations.cnf.TseitinTransformation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.F.*;

/**
 * Unit tests for {@link FormulaFactoryImporter}.
 * @version 1.3.1
 * @since 1.3.1
 */
public class FormulaFactoryImporterTest {

  private FormulaFactory f;
  private FormulaFactory g;
  private FormulaFactoryImporter importer;

  @Before
  public void initialize() {
    this.f = F.f;
    this.g = new FormulaFactory("Factory G");
    this.importer = new FormulaFactoryImporter(this.g);
  }

  @Test
  public void testSameFactory() {
    final FormulaFactoryImporter sameImporter = new FormulaFactoryImporter(this.f);
    assertThat(NOT1.factory()).isSameAs(this.f);
    assertThat(NOT2.factory()).isSameAs(this.f);
    assertThat(sameImporter.apply(NOT1, false).factory()).isSameAs(this.f);
    assertThat(sameImporter.apply(NOT2, false).factory()).isSameAs(this.f);
    assertThat(sameImporter.apply(NOT1, false)).isEqualTo(NOT1);
    assertThat(sameImporter.apply(NOT2, false)).isEqualTo(NOT2);
  }

  @Test
  public void testConstants() {
    assertThat(TRUE.factory()).isSameAs(this.f);
    assertThat(FALSE.factory()).isSameAs(this.f);
    assertThat(imp(TRUE).factory()).isSameAs(this.g);
    assertThat(imp(FALSE).factory()).isSameAs(this.g);
    assertThat(imp(TRUE)).isEqualTo(TRUE);
    assertThat(imp(FALSE)).isEqualTo(FALSE);
  }

  @Test
  public void testLiteral() {
    assertThat(A.factory()).isSameAs(this.f);
    assertThat(NA.factory()).isSameAs(this.f);
    assertThat(imp(A).factory()).isSameAs(this.g);
    assertThat(imp(NA).factory()).isSameAs(this.g);
    assertThat(imp(A)).isEqualTo(A);
    assertThat(imp(NA)).isEqualTo(NA);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(1);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(1);
  }

  @Test
  public void testNot() {
    assertThat(NOT1.factory()).isSameAs(this.f);
    assertThat(NOT2.factory()).isSameAs(this.f);
    assertThat(imp(NOT1).factory()).isSameAs(this.g);
    assertThat(imp(NOT2).factory()).isSameAs(this.g);
    assertThat(imp(NOT1)).isEqualTo(NOT1);
    assertThat(imp(NOT2)).isEqualTo(NOT2);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().negations()).isEqualTo(2);
  }

  @Test
  public void testImplication() {
    assertThat(IMP1.factory()).isSameAs(this.f);
    assertThat(IMP2.factory()).isSameAs(this.f);
    assertThat(IMP3.factory()).isSameAs(this.f);
    assertThat(IMP4.factory()).isSameAs(this.f);
    assertThat(imp(IMP1).factory()).isSameAs(this.g);
    assertThat(imp(IMP2).factory()).isSameAs(this.g);
    assertThat(imp(IMP3).factory()).isSameAs(this.g);
    assertThat(imp(IMP4).factory()).isSameAs(this.g);
    assertThat(imp(IMP1)).isEqualTo(IMP1);
    assertThat(imp(IMP2)).isEqualTo(IMP2);
    assertThat(imp(IMP3)).isEqualTo(IMP3);
    assertThat(imp(IMP4)).isEqualTo(IMP4);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().negations()).isEqualTo(0);
    assertThat(this.g.statistics().implications()).isEqualTo(4);
    assertThat(this.g.statistics().equivalences()).isEqualTo(2);
  }

  @Test
  public void testEquivalence() {
    assertThat(EQ1.factory()).isSameAs(this.f);
    assertThat(EQ2.factory()).isSameAs(this.f);
    assertThat(EQ3.factory()).isSameAs(this.f);
    assertThat(EQ4.factory()).isSameAs(this.f);
    assertThat(imp(EQ1).factory()).isSameAs(this.g);
    assertThat(imp(EQ2).factory()).isSameAs(this.g);
    assertThat(imp(EQ3).factory()).isSameAs(this.g);
    assertThat(imp(EQ4).factory()).isSameAs(this.g);
    assertThat(imp(EQ1)).isEqualTo(EQ1);
    assertThat(imp(EQ2)).isEqualTo(EQ2);
    assertThat(imp(EQ3)).isEqualTo(EQ3);
    assertThat(imp(EQ4)).isEqualTo(EQ4);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(1);
    assertThat(this.g.statistics().negations()).isEqualTo(2);
    assertThat(this.g.statistics().implications()).isEqualTo(2);
    assertThat(this.g.statistics().equivalences()).isEqualTo(4);
  }

  @Test
  public void testOr() {
    assertThat(OR1.factory()).isSameAs(this.f);
    assertThat(OR2.factory()).isSameAs(this.f);
    assertThat(OR3.factory()).isSameAs(this.f);
    assertThat(imp(OR1).factory()).isSameAs(this.g);
    assertThat(imp(OR2).factory()).isSameAs(this.g);
    assertThat(imp(OR3).factory()).isSameAs(this.g);
    assertThat(imp(OR1)).isEqualTo(OR1);
    assertThat(imp(OR2)).isEqualTo(OR2);
    assertThat(imp(OR3)).isEqualTo(OR3);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(2);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(3);
    assertThat(this.g.statistics().negations()).isEqualTo(2);
    assertThat(this.g.statistics().implications()).isEqualTo(0);
    assertThat(this.g.statistics().equivalences()).isEqualTo(0);
  }

  @Test
  public void testAnd() {
    assertThat(AND1.factory()).isSameAs(this.f);
    assertThat(AND2.factory()).isSameAs(this.f);
    assertThat(AND3.factory()).isSameAs(this.f);
    assertThat(imp(AND1).factory()).isSameAs(this.g);
    assertThat(imp(AND2).factory()).isSameAs(this.g);
    assertThat(imp(AND3).factory()).isSameAs(this.g);
    assertThat(imp(AND1)).isEqualTo(AND1);
    assertThat(imp(AND2)).isEqualTo(AND2);
    assertThat(imp(AND3)).isEqualTo(AND3);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(4);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(3);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(2);
    assertThat(this.g.statistics().negations()).isEqualTo(2);
    assertThat(this.g.statistics().implications()).isEqualTo(0);
    assertThat(this.g.statistics().equivalences()).isEqualTo(0);
  }

  @Test
  public void testPBC() {
    assertThat(PBC1.factory()).isSameAs(this.f);
    assertThat(PBC2.factory()).isSameAs(this.f);
    assertThat(PBC3.factory()).isSameAs(this.f);
    assertThat(PBC4.factory()).isSameAs(this.f);
    assertThat(PBC5.factory()).isSameAs(this.f);
    assertThat(imp(PBC1).factory()).isSameAs(this.g);
    assertThat(imp(PBC2).factory()).isSameAs(this.g);
    assertThat(imp(PBC3).factory()).isSameAs(this.g);
    assertThat(imp(PBC4).factory()).isSameAs(this.g);
    assertThat(imp(PBC5).factory()).isSameAs(this.g);
    assertThat(imp(PBC1)).isEqualTo(PBC1);
    assertThat(imp(PBC2)).isEqualTo(PBC2);
    assertThat(imp(PBC3)).isEqualTo(PBC3);
    assertThat(imp(PBC4)).isEqualTo(PBC4);
    assertThat(imp(PBC5)).isEqualTo(PBC5);
    assertThat(this.g.statistics().positiveLiterals()).isEqualTo(3);
    assertThat(this.g.statistics().negativeLiterals()).isEqualTo(0);
    assertThat(this.g.statistics().conjunctions2()).isEqualTo(0);
    assertThat(this.g.statistics().disjunctions2()).isEqualTo(0);
    assertThat(this.g.statistics().negations()).isEqualTo(0);
    assertThat(this.g.statistics().implications()).isEqualTo(0);
    assertThat(this.g.statistics().equivalences()).isEqualTo(0);
  }

  @Test
  public void testAdjustCounters() throws ParserException {
    final FormulaFactory f = new FormulaFactory("Factory");
    final PseudoBooleanParser p = new PseudoBooleanParser(f);
    final Formula cc = p.parse("A + B + C + D + E <= 2").cnf();
    final Formula pbc = p.parse("2*A + -2*B + 3*C + D + 2*E <= 3").cnf();
    final Formula cnf = p.parse("A & B & C | C & D & ~A").transform(new TseitinTransformation(0));

    final FormulaFactory g = new FormulaFactory();
    g.newCNFVariable();
    g.newCNFVariable();
    g.newCCVariable();
    g.newCCVariable();
    g.newCCVariable();
    g.newPBVariable();
    g.newPBVariable();
    g.newPBVariable();
    g.importFormula(cc);
    g.importFormula(pbc);
    g.importFormula(cnf);
    assertThat(g.statistics().cnfCounter()).isEqualTo(2);
    assertThat(g.statistics().ccCounter()).isEqualTo(13);
    assertThat(g.statistics().pbCounter()).isEqualTo(25);
  }

  /**
   * Imports the given formula in the new formula factory.
   * @param formula the formula to import
   * @return the imported formula
   */
  private Formula imp(final Formula formula) {
    return this.importer.apply(formula, false);
  }
}
