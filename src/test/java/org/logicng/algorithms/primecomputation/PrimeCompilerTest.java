package org.logicng.algorithms.primecomputation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for the class {@link PrimeCompiler}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class PrimeCompilerTest {

    private final FormulaFactory f = F.f;

    @Test
    public void testSimple() {
        computeAndVerify(F.TRUE);
        computeAndVerify(F.FALSE);
        computeAndVerify(F.A);
        computeAndVerify(F.NA);
        computeAndVerify(F.AND1);
        computeAndVerify(F.AND2);
        computeAndVerify(F.AND3);
        computeAndVerify(F.OR1);
        computeAndVerify(F.OR2);
        computeAndVerify(F.OR3);
        computeAndVerify(F.NOT1);
        computeAndVerify(F.NOT2);
        computeAndVerify(F.IMP1);
        computeAndVerify(F.IMP2);
        computeAndVerify(F.IMP3);
        computeAndVerify(F.IMP4);
        computeAndVerify(F.EQ1);
        computeAndVerify(F.EQ2);
        computeAndVerify(F.EQ3);
        computeAndVerify(F.EQ4);
        computeAndVerify(F.PBC1);
        computeAndVerify(F.PBC2);
        computeAndVerify(F.PBC3);
        computeAndVerify(F.PBC4);
        computeAndVerify(F.PBC5);
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        cornerCases.cornerCases().forEach(this::computeAndVerify);
    }

    @Test
    @RandomTag
    public void testRandomized() {
        for (int i = 0; i < 200; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(2).seed(42).build());
            final Formula formula = randomizer.formula(4);
            computeAndVerify(formula);
        }
    }

    @Test
    public void testOriginalFormulas() throws IOException {
        Files.lines(Paths.get("src/test/resources/formulas/simplify_formulas.txt"))
                .filter(s -> !s.isEmpty())
                .forEach(s -> {
                    try {
                        final Formula formula = this.f.parse(s);
                        final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
                        verify(resultImplicantsMin, formula);
                        final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
                        verify(resultImplicatesMin, formula);
                    } catch (final ParserException e) {
                        fail(e.toString());
                    }
                });
    }

    private void computeAndVerify(final Formula formula) {
        final PrimeResult resultImplicantsMax = PrimeCompiler.getWithMaximization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        verify(resultImplicantsMax, formula);
        final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        verify(resultImplicantsMin, formula);
        assertThat(resultImplicantsMax.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMin.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMax.getPrimeImplicants()).containsExactlyInAnyOrderElementsOf(resultImplicantsMin.getPrimeImplicants());

        final PrimeResult resultImplicatesMax = PrimeCompiler.getWithMaximization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        verify(resultImplicatesMax, formula);
        final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        verify(resultImplicatesMin, formula);
        assertThat(resultImplicatesMax.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        assertThat(resultImplicatesMin.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        assertThat(resultImplicatesMax.getPrimeImplicates()).containsExactlyInAnyOrderElementsOf(resultImplicatesMin.getPrimeImplicates());
    }

    private void verify(final PrimeResult result, final Formula formula) {
        verifyImplicants(result.getPrimeImplicants(), formula);
        verifyImplicates(result.getPrimeImplicates(), formula);
    }

    private void verifyImplicants(final List<SortedSet<Literal>> implicantSets, final Formula formula) {
        final List<Formula> implicants = new ArrayList<>();
        for (final SortedSet<Literal> implicant : implicantSets) {
            implicants.add(this.f.and(implicant));
            PrimeImplicantReductionTest.testPrimeImplicantProperty(formula, implicant);
        }
        assertThat(this.f.equivalence(this.f.or(implicants), formula).holds(new TautologyPredicate(this.f)))
                .as("Disjunction of implicants should be equivalent to the original formula.")
                .isTrue();
    }

    private void verifyImplicates(final List<SortedSet<Literal>> implicateSets, final Formula formula) {
        final List<Formula> implicates = new ArrayList<>();
        for (final SortedSet<Literal> implicate : implicateSets) {
            implicates.add(this.f.or(implicate));
            PrimeImplicateReductionTest.testPrimeImplicantProperty(formula, implicate);
        }
        assertThat(this.f.equivalence(this.f.and(implicates), formula).holds(new TautologyPredicate(this.f)))
                .as("Conjunction of implicates should be equivalent to the original formula.")
                .isTrue();
    }
}
