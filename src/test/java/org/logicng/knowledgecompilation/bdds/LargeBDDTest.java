// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.NumberOfNodesBDDHandler;
import org.logicng.handlers.TimeoutBDDHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
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

    private void testQueens(final FormulaFactory f, final NQueensGenerator generator, final int size,
                            final int models) {
        final Formula queens = generator.generate(size);
        final BDDKernel kernel = new BDDKernel(f, queens.variables().size(), 10000, 10000);
        final BDD bdd = BDDFactory.build(queens, kernel);
        final Formula cnf = bdd.cnf();
        assertThat(cnf.isCNF()).isTrue();
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
