package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.datastructures.BDD;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDConstant;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDInnerNode;
import org.logicng.knowledgecompilation.bdds.functions.LngBDDFunction;
import org.logicng.knowledgecompilation.bdds.io.BDDDotFileWriter;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDReordering;
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
public class BDDReorderingTest {

    private final SwapStats stats = new SwapStats();
    private static final List<BDDReordering> REORDER_METHODS =
            Arrays.asList(BDDReordering.BDD_REORDER_WIN2, BDDReordering.BDD_REORDER_WIN2ITE, BDDReordering.BDD_REORDER_WIN3, BDDReordering.BDD_REORDER_WIN3ITE, BDDReordering.BDD_REORDER_SIFT,
                    BDDReordering.BDD_REORDER_SIFTITE, BDDReordering.BDD_REORDER_RANDOM);

    @Test
    public void testSwapping() throws ParserException {
        final FormulaFactory f = F.f;
        final BDDKernel kernel = new BDDKernel(f, Arrays.asList(F.A, F.B, F.C), 100, 100);
        final Formula formula = f.parse("a | b | c");
        final BDD bdd = BDDFactory.build(formula, kernel);
        assertThat(bdd.getVariableOrder()).containsExactly(F.A, F.B, F.C);
        bdd.swapVariables(F.A, F.B);
        assertThat(bdd.getVariableOrder()).containsExactly(F.B, F.A, F.C);
        bdd.swapVariables(F.A, F.B);
        assertThat(bdd.getVariableOrder()).containsExactly(F.A, F.B, F.C);
        bdd.swapVariables(F.A, F.C);
        assertThat(bdd.getVariableOrder()).containsExactly(F.C, F.B, F.A);
        bdd.swapVariables(F.B, F.C);
        assertThat(bdd.getVariableOrder()).containsExactly(F.B, F.C, F.A);
        assertThat(f.equivalence(formula, bdd.cnf()).holds(new TautologyPredicate(f))).isTrue();
        assertThat(bdd.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(F.B,
                        new BDDInnerNode(F.C,
                                new BDDInnerNode(F.A, BDDConstant.getFalsumNode(f), BDDConstant.getVerumNode(f)),
                                BDDConstant.getVerumNode(f)),
                        BDDConstant.getVerumNode(f)));
        assertThatThrownBy(() -> bdd.swapVariables(F.B, F.X)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testSwappingMultipleBdds() throws ParserException {
        final FormulaFactory f = F.f;
        final BDDKernel kernel = new BDDKernel(f, Arrays.asList(F.A, F.B, F.C), 100, 100);
        final Formula formula1 = f.parse("a | b | c");
        final Formula formula2 = f.parse("a & b");
        final BDD bdd1 = BDDFactory.build(formula1, kernel);
        final BDD bdd2 = BDDFactory.build(formula2, kernel);
        assertThat(bdd1.getVariableOrder()).containsExactly(F.A, F.B, F.C);
        assertThat(bdd2.getVariableOrder()).containsExactly(F.A, F.B, F.C);
        assertThat(bdd2.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(F.A, BDDConstant.getFalsumNode(f),
                        new BDDInnerNode(F.B, BDDConstant.getFalsumNode(f), BDDConstant.getVerumNode(f))));
        bdd1.swapVariables(F.A, F.B);
        assertThat(bdd1.getVariableOrder()).containsExactly(F.B, F.A, F.C);
        assertThat(bdd2.getVariableOrder()).containsExactly(F.B, F.A, F.C);
        assertThat(bdd2.apply(new LngBDDFunction())).isEqualTo(
                new BDDInnerNode(F.B, BDDConstant.getFalsumNode(f),
                        new BDDInnerNode(F.A, BDDConstant.getFalsumNode(f), BDDConstant.getVerumNode(f))));
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
                for (final BDDReordering method : REORDER_METHODS) {
                    performReorder(f, formula, method, true, verbose);
                }
                for (final BDDReordering method : REORDER_METHODS) {
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

    private void performReorder(final FormulaFactory f, final Formula formula, final BDDReordering reorderMethod, final boolean withBlocks, final boolean verbose) {
        final BDDKernel kernel = new BDDKernel(f, new ArrayList<>(formula.variables()), 1000, 10000);
        final BDD bdd = BDDFactory.build(formula, kernel);
        final BigInteger count = bdd.modelCount();
        final int usedBefore = kernel.nodeCount(bdd.index());
        final long start = System.currentTimeMillis();
        addVariableBlocks(formula.variables().size(), withBlocks, kernel);
        kernel.bdd_reorder(reorderMethod);
        final long duration = System.currentTimeMillis() - start;
        final int usedAfter = kernel.nodeCount(bdd.index());
        assertThat(verifyBddConsistency(f, formula, bdd, count)).isTrue();
        verifyVariableBlocks(f, formula, withBlocks, bdd);
        if (reorderMethod != BDDReordering.BDD_REORDER_RANDOM) {
            assertThat(usedAfter).isLessThanOrEqualTo(usedBefore);
        }
        final double reduction = (usedBefore - usedAfter) / (double) usedBefore * 100;
        if (verbose) {
            System.out.println(String.format("%-20s: Reduced %7s blocks in %5dms by %.2f%% from %d to %d", reorderMethod, withBlocks ? "with" : "without", duration, reduction, usedBefore, usedAfter));
        }
    }

    private void addVariableBlocks(final int numVars, final boolean withBlocks, final BDDKernel kernel) {
        if (withBlocks) {
            kernel.addVariableBlockAll();
            kernel.addVariableBlock(0, 20, true);
            kernel.addVariableBlock(0, 10, false);
            kernel.addVariableBlock(11, 20, false);
            kernel.addVariableBlock(15, 19, false);
            kernel.addVariableBlock(15, 17, true);
            kernel.addVariableBlock(18, 19, false);
            kernel.addVariableBlock(21, numVars - 1, false);
            if (numVars > 33) {
                kernel.addVariableBlock(30, 33, false);
            }
        } else {
            kernel.addVariableBlockAll();
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
                final int nodeCount = kernel.nodeCount(bdd.index());
                final BigInteger modelCount = bdd.modelCount();
                for (final BDDReordering method : REORDER_METHODS) {
                    reorderOnBuild(f, formula, method, modelCount, nodeCount, true, verbose);
                }
                for (final BDDReordering method : REORDER_METHODS) {
                    reorderOnBuild(f, formula, method, modelCount, nodeCount, false, verbose);
                }
            }
        }
    }

    private void reorderOnBuild(final FormulaFactory f, final Formula formula, final BDDReordering method, final BigInteger originalCount, final int originalUsedNodes, final boolean withBlocks,
                                final boolean verbose) {
        final BDDKernel kernel = new BDDKernel(f, new ArrayList<>(formula.variables()), 1000, 10000);
        addVariableBlocks(formula.variables().size(), withBlocks, kernel);
        kernel.bdd_autoreorder_times(method, 10000);
        final long start = System.currentTimeMillis();
        final BDD bdd = BDDFactory.build(formula, kernel);
        final long duration = System.currentTimeMillis() - start;
        final int usedAfter = kernel.nodeCount(bdd.index());
        verifyVariableBlocks(f, formula, withBlocks, bdd);
        verifyBddConsistency(f, formula, bdd, originalCount);
        final double reduction = (originalUsedNodes - usedAfter) / (double) originalUsedNodes * 100;
        if (verbose) {
            System.out.println(String.format("%-20s: Built in %5d ms, reduction by %6.2f%% from %6d to %6d", method, duration, reduction, originalUsedNodes, usedAfter));
        }
    }

    private boolean verifyBddConsistency(final FormulaFactory f, final Formula f1, final BDD bdd, final BigInteger modelCount) {
        if (!bdd.underlyingKernel().verify(bdd.index())) {
            return false;
        }
        final long nodes = bdd.underlyingKernel().verifyTree(bdd.index());
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
