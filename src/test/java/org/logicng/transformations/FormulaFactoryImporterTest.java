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

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.transformations.cnf.TseitinTransformation;

/**
 * Unit tests for {@link FormulaFactoryImporter}.
 * @version 2.0.0
 * @since 1.3.1
 */
public class FormulaFactoryImporterTest extends TestWithExampleFormulas {

    private FormulaFactory myF;
    private FormulaFactory myG;
    private FormulaFactoryImporter importer;

    @BeforeEach
    public void initialize() {
        this.myF = this.f;
        this.myG = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory G").build());
        this.importer = new FormulaFactoryImporter(this.myG);
    }

    @Test
    public void testSameFactory() {
        final FormulaFactoryImporter sameImporter = new FormulaFactoryImporter(this.myF);
        assertThat(this.NOT1.factory()).isSameAs(this.myF);
        assertThat(this.NOT2.factory()).isSameAs(this.myF);
        assertThat(sameImporter.apply(this.NOT1, false).factory()).isSameAs(this.myF);
        assertThat(sameImporter.apply(this.NOT2, false).factory()).isSameAs(this.myF);
        assertThat(sameImporter.apply(this.NOT1, false)).isEqualTo(this.NOT1);
        assertThat(sameImporter.apply(this.NOT2, false)).isEqualTo(this.NOT2);
    }

    @Test
    public void testConstants() {
        assertThat(this.TRUE.factory()).isSameAs(this.myF);
        assertThat(this.FALSE.factory()).isSameAs(this.myF);
        assertThat(imp(this.TRUE).factory()).isSameAs(this.myG);
        assertThat(imp(this.FALSE).factory()).isSameAs(this.myG);
        assertThat(imp(this.TRUE)).isEqualTo(this.TRUE);
        assertThat(imp(this.FALSE)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiteral() {
        assertThat(this.A.factory()).isSameAs(this.myF);
        assertThat(this.NA.factory()).isSameAs(this.myF);
        assertThat(imp(this.A).factory()).isSameAs(this.myG);
        assertThat(imp(this.NA).factory()).isSameAs(this.myG);
        assertThat(imp(this.A)).isEqualTo(this.A);
        assertThat(imp(this.NA)).isEqualTo(this.NA);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(1);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(1);
    }

    @Test
    public void testNot() {
        assertThat(this.NOT1.factory()).isSameAs(this.myF);
        assertThat(this.NOT2.factory()).isSameAs(this.myF);
        assertThat(imp(this.NOT1).factory()).isSameAs(this.myG);
        assertThat(imp(this.NOT2).factory()).isSameAs(this.myG);
        assertThat(imp(this.NOT1)).isEqualTo(this.NOT1);
        assertThat(imp(this.NOT2)).isEqualTo(this.NOT2);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().negations()).isEqualTo(2);
    }

    @Test
    public void testImplication() {
        assertThat(this.IMP1.factory()).isSameAs(this.myF);
        assertThat(this.IMP2.factory()).isSameAs(this.myF);
        assertThat(this.IMP3.factory()).isSameAs(this.myF);
        assertThat(this.IMP4.factory()).isSameAs(this.myF);
        assertThat(imp(this.IMP1).factory()).isSameAs(this.myG);
        assertThat(imp(this.IMP2).factory()).isSameAs(this.myG);
        assertThat(imp(this.IMP3).factory()).isSameAs(this.myG);
        assertThat(imp(this.IMP4).factory()).isSameAs(this.myG);
        assertThat(imp(this.IMP1)).isEqualTo(this.IMP1);
        assertThat(imp(this.IMP2)).isEqualTo(this.IMP2);
        assertThat(imp(this.IMP3)).isEqualTo(this.IMP3);
        assertThat(imp(this.IMP4)).isEqualTo(this.IMP4);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().negations()).isEqualTo(0);
        assertThat(this.myG.statistics().implications()).isEqualTo(4);
        assertThat(this.myG.statistics().equivalences()).isEqualTo(2);
    }

    @Test
    public void testEquivalence() {
        assertThat(this.EQ1.factory()).isSameAs(this.myF);
        assertThat(this.EQ2.factory()).isSameAs(this.myF);
        assertThat(this.EQ3.factory()).isSameAs(this.myF);
        assertThat(this.EQ4.factory()).isSameAs(this.myF);
        assertThat(imp(this.EQ1).factory()).isSameAs(this.myG);
        assertThat(imp(this.EQ2).factory()).isSameAs(this.myG);
        assertThat(imp(this.EQ3).factory()).isSameAs(this.myG);
        assertThat(imp(this.EQ4).factory()).isSameAs(this.myG);
        assertThat(imp(this.EQ1)).isEqualTo(this.EQ1);
        assertThat(imp(this.EQ2)).isEqualTo(this.EQ2);
        assertThat(imp(this.EQ3)).isEqualTo(this.EQ3);
        assertThat(imp(this.EQ4)).isEqualTo(this.EQ4);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(1);
        assertThat(this.myG.statistics().negations()).isEqualTo(2);
        assertThat(this.myG.statistics().implications()).isEqualTo(2);
        assertThat(this.myG.statistics().equivalences()).isEqualTo(4);
    }

    @Test
    public void testOr() {
        assertThat(this.OR1.factory()).isSameAs(this.myF);
        assertThat(this.OR2.factory()).isSameAs(this.myF);
        assertThat(this.OR3.factory()).isSameAs(this.myF);
        assertThat(imp(this.OR1).factory()).isSameAs(this.myG);
        assertThat(imp(this.OR2).factory()).isSameAs(this.myG);
        assertThat(imp(this.OR3).factory()).isSameAs(this.myG);
        assertThat(imp(this.OR1)).isEqualTo(this.OR1);
        assertThat(imp(this.OR2)).isEqualTo(this.OR2);
        assertThat(imp(this.OR3)).isEqualTo(this.OR3);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(2);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(3);
        assertThat(this.myG.statistics().negations()).isEqualTo(2);
        assertThat(this.myG.statistics().implications()).isEqualTo(0);
        assertThat(this.myG.statistics().equivalences()).isEqualTo(0);
    }

    @Test
    public void testAnd() {
        assertThat(this.AND1.factory()).isSameAs(this.myF);
        assertThat(this.AND2.factory()).isSameAs(this.myF);
        assertThat(this.AND3.factory()).isSameAs(this.myF);
        assertThat(imp(this.AND1).factory()).isSameAs(this.myG);
        assertThat(imp(this.AND2).factory()).isSameAs(this.myG);
        assertThat(imp(this.AND3).factory()).isSameAs(this.myG);
        assertThat(imp(this.AND1)).isEqualTo(this.AND1);
        assertThat(imp(this.AND2)).isEqualTo(this.AND2);
        assertThat(imp(this.AND3)).isEqualTo(this.AND3);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(4);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(3);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(2);
        assertThat(this.myG.statistics().negations()).isEqualTo(2);
        assertThat(this.myG.statistics().implications()).isEqualTo(0);
        assertThat(this.myG.statistics().equivalences()).isEqualTo(0);
    }

    @Test
    public void testPBC() {
        assertThat(this.PBC1.factory()).isSameAs(this.myF);
        assertThat(this.PBC2.factory()).isSameAs(this.myF);
        assertThat(this.PBC3.factory()).isSameAs(this.myF);
        assertThat(this.PBC4.factory()).isSameAs(this.myF);
        assertThat(this.PBC5.factory()).isSameAs(this.myF);
        assertThat(imp(this.PBC1).factory()).isSameAs(this.myG);
        assertThat(imp(this.PBC2).factory()).isSameAs(this.myG);
        assertThat(imp(this.PBC3).factory()).isSameAs(this.myG);
        assertThat(imp(this.PBC4).factory()).isSameAs(this.myG);
        assertThat(imp(this.PBC5).factory()).isSameAs(this.myG);
        assertThat(imp(this.PBC1)).isEqualTo(this.PBC1);
        assertThat(imp(this.PBC2)).isEqualTo(this.PBC2);
        assertThat(imp(this.PBC3)).isEqualTo(this.PBC3);
        assertThat(imp(this.PBC4)).isEqualTo(this.PBC4);
        assertThat(imp(this.PBC5)).isEqualTo(this.PBC5);
        assertThat(this.myG.statistics().positiveLiterals()).isEqualTo(3);
        assertThat(this.myG.statistics().negativeLiterals()).isEqualTo(0);
        assertThat(this.myG.statistics().conjunctions2()).isEqualTo(0);
        assertThat(this.myG.statistics().disjunctions2()).isEqualTo(0);
        assertThat(this.myG.statistics().negations()).isEqualTo(0);
        assertThat(this.myG.statistics().implications()).isEqualTo(0);
        assertThat(this.myG.statistics().equivalences()).isEqualTo(0);
    }

    @Test
    public void testAdjustCounters() throws ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory").build());
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
