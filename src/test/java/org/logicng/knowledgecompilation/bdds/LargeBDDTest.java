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
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.NumberOfNodesBDDHandler;
import org.logicng.handlers.TimeoutBDDHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.predicates.CNFPredicate;
import org.logicng.testutils.NQueensGenerator;
import org.logicng.testutils.PigeonHoleGenerator;

import java.math.BigInteger;

/**
 * Some more extensive tests for BDDs
 * @version 2.0.0
 * @since 1.4.0
 */
public class LargeBDDTest {

    @Test
    public void testPigeonHole() {
        final FormulaFactory f = new FormulaFactory();
        final PigeonHoleGenerator generator = new PigeonHoleGenerator(f);
        testPigeonHole(f, generator, 2);
        testPigeonHole(f, generator, 3);
        testPigeonHole(f, generator, 4);
        testPigeonHole(f, generator, 5);
        testPigeonHole(f, generator, 6);
        testPigeonHole(f, generator, 7);
        testPigeonHole(f, generator, 8);
        testPigeonHole(f, generator, 9);
    }

    private void testPigeonHole(final FormulaFactory f, final PigeonHoleGenerator generator, final int size) {
        final Formula pigeon = generator.generate(size);
        final BDDKernel kernel = new BDDKernel(f, pigeon.variables().size(), 10000, 10000);
        final BDD bdd = BDDFactory.build(pigeon, kernel);
        assertThat(bdd.isContradiction()).isTrue();
    }

    @Test
    public void testQueens() {
        final FormulaFactory f = new FormulaFactory();
        final NQueensGenerator generator = new NQueensGenerator(f);
        testQueens(f, generator, 4, 2);
        testQueens(f, generator, 5, 10);
        testQueens(f, generator, 6, 4);
        testQueens(f, generator, 7, 40);
        testQueens(f, generator, 8, 92);
    }

    private void testQueens(final FormulaFactory f, final NQueensGenerator generator, final int size, final int models) {
        final Formula queens = generator.generate(size);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final BDD bdd = BDDFactory.build(queens, kernel);
        final Formula cnf = bdd.cnf();
        assertThat(cnf.holds(CNFPredicate.get())).isTrue();
        final BDD cnfBDD = BDDFactory.build(cnf, kernel);
        assertThat(cnfBDD).isEqualTo(bdd);
        assertThat(bdd.support()).isEqualTo(queens.variables());
        assertThat(bdd.modelCount()).isEqualTo(BigInteger.valueOf(models));
    }

    @Test
    public void testTimeoutBDDHandlerSmall() {
        final FormulaFactory f = new FormulaFactory();
        final NQueensGenerator generator = new NQueensGenerator(f);
        final Formula queens = generator.generate(4);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final TimeoutBDDHandler handler = new TimeoutBDDHandler(2000L);
        final BDD bdd = BDDFactory.build(queens, kernel, handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(bdd.index()).isNotEqualTo(BDDKernel.BDD_ABORT);
    }

    @Test
    public void testTimeoutBDDHandlerLarge() {
        final FormulaFactory f = new FormulaFactory();
        final NQueensGenerator generator = new NQueensGenerator(f);
        final Formula queens = generator.generate(10);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final TimeoutBDDHandler handler = new TimeoutBDDHandler(1000L);
        final BDD bdd = BDDFactory.build(queens, kernel, handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(bdd.index()).isEqualTo(BDDKernel.BDD_ABORT);
    }

    @Test
    public void testNumberOfNodesHandlerSmall() {
        final FormulaFactory f = new FormulaFactory();
        final NQueensGenerator generator = new NQueensGenerator(f);
        final Formula queens = generator.generate(4);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final NumberOfNodesBDDHandler handler = new NumberOfNodesBDDHandler(1000);
        final BDD bdd = BDDFactory.build(queens, kernel, handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(bdd.index()).isNotEqualTo(BDDKernel.BDD_ABORT);
    }

    @Test
    public void testNumberOfNodesHandlerLarge() {
        final FormulaFactory f = new FormulaFactory();
        final NQueensGenerator generator = new NQueensGenerator(f);
        final Formula queens = generator.generate(10);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final NumberOfNodesBDDHandler handler = new NumberOfNodesBDDHandler(5);
        final BDD bdd = BDDFactory.build(queens, kernel, handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(bdd.index()).isEqualTo(BDDKernel.BDD_ABORT);
    }

    @Test
    public void testNumberOfNodesHandler() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.parse("A <=> ~(B => C & F & G & ~H | A & D & ~E)");
        final BDDKernel kernel = new BDDKernel(f, formula.variables().size(), 10000, 10000);
        final NumberOfNodesBDDHandler handler = new NumberOfNodesBDDHandler(5);
        final BDD bdd = BDDFactory.build(formula, kernel, handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(bdd.index()).isEqualTo(BDDKernel.BDD_ABORT);
    }
}
