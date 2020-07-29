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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDConstant;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDInnerNode;
import org.logicng.knowledgecompilation.bdds.functions.LngBDDFunction;
import org.logicng.knowledgecompilation.bdds.io.BDDDotFileWriter;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDReordering;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDReorderingMethod;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDVerification;
import org.logicng.predicates.satisfiability.SATPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Unit Tests for the reordering algorithms of BDDs.
 * @version 2.0.0
 * @since 2.0.0
 */
public class BDDReorderingTest extends TestWithExampleFormulas {

    private final SwapStats stats = new SwapStats();
    private static final List<BDDReorderingMethod> REORDER_METHODS =
            Arrays.asList(BDDReorderingMethod.BDD_REORDER_WIN2, BDDReorderingMethod.BDD_REORDER_WIN2ITE, BDDReorderingMethod.BDD_REORDER_WIN3, BDDReorderingMethod.BDD_REORDER_WIN3ITE,
                    BDDReorderingMethod.BDD_REORDER_SIFT,
                    BDDReorderingMethod.BDD_REORDER_SIFTITE, BDDReorderingMethod.BDD_REORDER_RANDOM);

    @Test
    public void testExceptionalBehavior() throws ParserException {
        assertThatThrownBy(() -> {
            final BDDKernel kernel = new BDDKernel(this.f, Arrays.asList(this.A, this.B), 100, 100);
            final BDDReordering reordering = new BDDReordering(kernel);
            final Formula formula = this.f.parse("a | b");
            BDDFactory.build(formula, kernel);
            reordering.swapVariables(0, 2);
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Unknown variable number: " + 2);
        assertThatThrownBy(() -> {
            final BDDKernel kernel = new BDDKernel(this.f, Arrays.asList(this.A, this.B), 100, 100);
            final BDDReordering reordering = new BDDReordering(kernel);
            final Formula formula = this.f.parse("a | b");
            BDDFactory.build(formula, kernel);
            reordering.swapVariables(3, 0);
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Unknown variable number: " + 3);
    }

    @Test
    public void testSwapping() throws ParserException {
        final BDDKernel kernel = new BDDKernel(this.f, Arrays.asList(this.A, this.B, this.C), 100, 100);
        final Formula formula = this.f.parse("a | b | c");
        final BDD bdd = BDDFactory.build(formula, kernel);
        assertThat(bdd.getVariableOrder()).containsExactly(this.A, this.B, this.C);
        bdd.swapVariables(this.A, this.B);
        assertThat(bdd.getVariableOrder()).containsExactly(this.B, this.A, this.C);
        bdd.swapVariables(this.A, this.B);
        assertThat(bdd.getVariableOrder()).containsExactly(this.A, this.B, this.C);
        bdd.swapVariables(this.A, this.A);
        assertThat(bdd.getVariableOrder()).containsExactly(this.A, this.B, this.C);
        bdd.swapVariables(this.A, this.C);
        assertThat(bdd.getVariableOrder()).containsExactly(this.C, this.B, this.A);
        bdd.swapVariables(this.B, this.C);
        assertThat(bdd.getVariableOrder()).containsExactly(this.B, this.C, this.A);
        assertThat(this.f.equivalence(formula, bdd.cnf()).holds(new TautologyPredicate(this.f))).isTrue();
        assertThat(bdd.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(this.B,
                        new BDDInnerNode(this.C,
                                new BDDInnerNode(this.A, BDDConstant.getFalsumNode(this.f), BDDConstant.getVerumNode(this.f)),
                                BDDConstant.getVerumNode(this.f)),
                        BDDConstant.getVerumNode(this.f)));
        assertThatThrownBy(() -> bdd.swapVariables(this.B, this.X)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testSwappingMultipleBdds() throws ParserException {
        final BDDKernel kernel = new BDDKernel(this.f, Arrays.asList(this.A, this.B, this.C), 100, 100);
        final Formula formula1 = this.f.parse("a | b | c");
        final Formula formula2 = this.f.parse("a & b");
        final BDD bdd1 = BDDFactory.build(formula1, kernel);
        final BDD bdd2 = BDDFactory.build(formula2, kernel);
        assertThat(bdd1.getVariableOrder()).containsExactly(this.A, this.B, this.C);
        assertThat(bdd2.getVariableOrder()).containsExactly(this.A, this.B, this.C);
        assertThat(bdd2.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(this.A, BDDConstant.getFalsumNode(this.f),
                        new BDDInnerNode(this.B, BDDConstant.getFalsumNode(this.f), BDDConstant.getVerumNode(this.f))));
        bdd1.swapVariables(this.A, this.B);
        assertThat(bdd1.getVariableOrder()).containsExactly(this.B, this.A, this.C);
        assertThat(bdd2.getVariableOrder()).containsExactly(this.B, this.A, this.C);
        assertThat(bdd2.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(this.B, BDDConstant.getFalsumNode(this.f),
                        new BDDInnerNode(this.A, BDDConstant.getFalsumNode(this.f), BDDConstant.getVerumNode(this.f))));
    }

    @Test
    public void testRandomReorderingQuick() {
        testRandomReordering(25, 30, false);
    }

    @Test
    @LongRunningTag
    public void testRandomReorderingLongRunning() {
        testRandomReordering(25, 50, true);
    }

    @Test
    public void testReorderOnBuildQuick() {
        testReorderOnBuild(25, 30, false);
    }

    @Test
    @LongRunningTag
    public void testReorderOnBuildLongRunning() {
        testReorderOnBuild(25, 50, true);
    }

    private void testRandomReordering(final int minVars, final int maxVars, final boolean verbose) {
        for (int vars = minVars; vars <= maxVars; vars++) {
            for (int depth = 4; depth <= 6; depth++) {
                final FormulaFactory f = new FormulaFactory();
                final Formula formula = randomFormula(vars, depth, f);
                if (verbose) {
                    System.out.println(String.format("vars = %2d, depth = %2d, nodes = %5d", vars, depth, formula.numberOfNodes()));
                }
                for (final BDDReorderingMethod method : REORDER_METHODS) {
                    performReorder(f, formula, method, true, verbose);
                }
                for (final BDDReorderingMethod method : REORDER_METHODS) {
                    performReorder(f, formula, method, false, verbose);
                }
            }
        }
    }

    private Formula randomFormula(final int vars, final int depth, final FormulaFactory f) {
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder()
                .numVars(vars).seed(vars * depth * 42)
                .weightEquiv(0).weightImpl(0).weightNot(0).build());
        return Stream.generate(() -> randomizer.and(depth))
                .filter(fm -> fm.variables().size() == vars && fm.holds(new SATPredicate(f)))
                .findAny().get();
    }

    private void performReorder(final FormulaFactory f, final Formula formula, final BDDReorderingMethod reorderMethod, final boolean withBlocks, final boolean verbose) {
        final BDDKernel kernel = new BDDKernel(f, new ArrayList<>(formula.variables()), 1000, 10000);
        final BDD bdd = BDDFactory.build(formula, kernel);
        final BigInteger count = bdd.modelCount();
        final int usedBefore = new BDDOperations(kernel).nodeCount(bdd.index());
        final long start = System.currentTimeMillis();
        addVariableBlocks(formula.variables().size(), withBlocks, kernel);
        kernel.getReordering().reorder(reorderMethod);
        final long duration = System.currentTimeMillis() - start;
        final int usedAfter = new BDDOperations(kernel).nodeCount(bdd.index());
        assertThat(verifyBddConsistency(f, formula, bdd, count)).isTrue();
        verifyVariableBlocks(f, formula, withBlocks, bdd);
        if (reorderMethod != BDDReorderingMethod.BDD_REORDER_RANDOM) {
            assertThat(usedAfter).isLessThanOrEqualTo(usedBefore);
        }
        final double reduction = (usedBefore - usedAfter) / (double) usedBefore * 100;
        if (verbose) {
            System.out.println(String.format("%-20s: Reduced %7s blocks in %5dms by %.2f%% from %d to %d", reorderMethod, withBlocks ? "with" : "without", duration, reduction, usedBefore, usedAfter));
        }
    }

    private void addVariableBlocks(final int numVars, final boolean withBlocks, final BDDKernel kernel) {
        final BDDReordering reordering = kernel.getReordering();
        if (withBlocks) {
            reordering.addVariableBlockAll();
            reordering.addVariableBlock(0, 20, true);
            reordering.addVariableBlock(0, 10, false);
            reordering.addVariableBlock(11, 20, false);
            reordering.addVariableBlock(15, 19, false);
            reordering.addVariableBlock(15, 17, true);
            reordering.addVariableBlock(18, 19, false);
            reordering.addVariableBlock(21, numVars - 1, false);
            if (numVars > 33) {
                reordering.addVariableBlock(30, 33, false);
            }
        } else {
            reordering.addVariableBlockAll();
        }
    }

    private void testReorderOnBuild(final int minVars, final int maxVars, final boolean verbose) {
        for (int vars = minVars; vars <= maxVars; vars++) {
            for (int depth = 4; depth <= 6; depth++) {
                final FormulaFactory f = new FormulaFactory();
                final Formula formula = randomFormula(vars, depth, f);
                if (verbose) {
                    System.out.println(String.format("vars = %2d, depth = %2d, nodes = %5d", vars, depth, formula.numberOfNodes()));
                }
                final BDDKernel kernel = new BDDKernel(f, new ArrayList<>(formula.variables()), 1000, 10000);
                final BDD bdd = BDDFactory.build(formula, kernel);
                final int nodeCount = new BDDOperations(kernel).nodeCount(bdd.index());
                final BigInteger modelCount = bdd.modelCount();
                for (final BDDReorderingMethod method : REORDER_METHODS) {
                    reorderOnBuild(f, formula, method, modelCount, nodeCount, true, verbose);
                }
                for (final BDDReorderingMethod method : REORDER_METHODS) {
                    reorderOnBuild(f, formula, method, modelCount, nodeCount, false, verbose);
                }
            }
        }
    }

    private void reorderOnBuild(final FormulaFactory f, final Formula formula, final BDDReorderingMethod method, final BigInteger originalCount, final int originalUsedNodes, final boolean withBlocks,
                                final boolean verbose) {
        final BDDKernel kernel = new BDDKernel(f, new ArrayList<>(formula.variables()), 1000, 10000);
        addVariableBlocks(formula.variables().size(), withBlocks, kernel);
        kernel.getReordering().setReorderDuringConstruction(method, 10000);
        final long start = System.currentTimeMillis();
        final BDD bdd = BDDFactory.build(formula, kernel);
        final long duration = System.currentTimeMillis() - start;
        final int usedAfter = new BDDOperations(kernel).nodeCount(bdd.index());
        verifyVariableBlocks(f, formula, withBlocks, bdd);
        verifyBddConsistency(f, formula, bdd, originalCount);
        final double reduction = (originalUsedNodes - usedAfter) / (double) originalUsedNodes * 100;
        if (verbose) {
            System.out.println(String.format("%-20s: Built in %5d ms, reduction by %6.2f%% from %6d to %6d", method, duration, reduction, originalUsedNodes, usedAfter));
        }
    }

    private boolean verifyBddConsistency(final FormulaFactory f, final Formula f1, final BDD bdd, final BigInteger modelCount) {
        final BDDVerification verification = new BDDVerification(bdd.underlyingKernel());
        if (!verification.verify(bdd.index())) {
            return false;
        }
        final long nodes = verification.verifyTree(bdd.index());
        if (nodes < 0) {
            return false;
        }
        this.stats.newBddSize(nodes);
        if (modelCount != null && !modelCount.equals(bdd.modelCount())) {
            System.out.println("Nodecount changed!");
            return false;
        }
        if (modelCount == null && !f.equivalence(f1, bdd.cnf()).holds(new TautologyPredicate(f))) {
            System.out.println("Not equal");
            return false;
        }
        return true;
    }

    private void verifyVariableBlocks(final FormulaFactory f, final Formula formula, final boolean withBlocks, final BDD bdd) {
        if (withBlocks) {
            assertThat(findSequence(bdd, IntStream.range(0, 21).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(0, 11).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(11, 21).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(15, 20).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(15, 18).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(18, 20).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            assertThat(findSequence(bdd, IntStream.range(21, formula.variables().size()).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            if (formula.variables().size() > 33) {
                assertThat(findSequence(bdd, IntStream.range(30, 34).mapToObj(i -> String.format("v%02d", i)).collect(Collectors.toSet()))).isTrue();
            }
            final List<Variable> order = bdd.getVariableOrder();
            assertThat(order.indexOf(f.variable("v00"))).isLessThan(order.indexOf(f.variable("v11")));
            assertThat(order.indexOf(f.variable("v16"))).isEqualTo(order.indexOf(f.variable("v15")) + 1);
            assertThat(order.indexOf(f.variable("v17"))).isEqualTo(order.indexOf(f.variable("v16")) + 1);
        }
    }

    private boolean findSequence(final BDD bdd, final Set<String> vars) {
        final Iterator<Variable> it = bdd.getVariableOrder().iterator();
        while (it.hasNext()) {
            if (vars.contains(it.next().name())) {
                int numFound = 1;
                while (numFound < vars.size()) {
                    if (!vars.contains(it.next().name())) {
                        return false;
                    } else {
                        numFound++;
                    }
                }
                return true;
            }
        }
        return false;
    }

    private void drawBdd(final BDD bdd, final String name) {
        try {
            BDDDotFileWriter.write("dot/" + name + ".dot", bdd);
            Runtime.getRuntime().exec("dot -Tpng -O dot/" + name + ".dot").waitFor();
        } catch (final IOException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    private static class SwapStats {
        private final int testedFormulas = 0;
        private final int numSwaps = 0;
        private long maxFormulaSize = 0;
        private long maxBddNodes = 0; // physical nodes
        private long maxBddSize = 0;  // num nodes without caching

        public void newFormula(final Formula formula) {
            this.maxFormulaSize = Math.max(this.maxFormulaSize, formula.numberOfNodes());
        }

        public void newBdd(final BDD bdd) {
            this.maxBddNodes = Math.max(this.maxBddNodes, bdd.nodeCount());
        }

        public void newBddSize(final long size) {
            this.maxBddSize = Math.max(this.maxBddSize, size);
        }

        @Override
        public String toString() {
            return "SwapStats{" +
                    "testedFormulas=" + this.testedFormulas +
                    ", numSwaps=" + this.numSwaps +
                    ", maxFormulaSize=" + this.maxFormulaSize +
                    ", maxBddNodes=" + this.maxBddNodes +
                    ", maxBddSize=" + this.maxBddSize +
                    '}';
        }
    }
}
