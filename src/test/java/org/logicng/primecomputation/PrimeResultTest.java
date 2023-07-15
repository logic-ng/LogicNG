// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.primecomputation;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link PrimeResult}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class PrimeResultTest extends TestWithExampleFormulas {

    private final PrimeResult result1;
    private final PrimeResult result2;
    private final PrimeResult result3;

    public PrimeResultTest() {
        final List<SortedSet<Literal>> primeImplicants1 = new ArrayList<>();
        primeImplicants1.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        primeImplicants1.add(new TreeSet<>(Arrays.asList(this.A, this.C)));
        final List<SortedSet<Literal>> primeImplicates1 = new ArrayList<>();
        primeImplicates1.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        this.result1 = new PrimeResult(primeImplicants1, primeImplicates1, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);

        final List<SortedSet<Literal>> primeImplicants2 = new ArrayList<>();
        primeImplicants2.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        primeImplicants2.add(new TreeSet<>(Collections.singletonList(this.C)));
        final List<SortedSet<Literal>> primeImplicates2 = new ArrayList<>();
        this.result2 = new PrimeResult(primeImplicants2, primeImplicates2, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);

        final List<SortedSet<Literal>> primeImplicants3 = new ArrayList<>();
        primeImplicants3.add(new TreeSet<>());
        final List<SortedSet<Literal>> primeImplicates3 = new ArrayList<>();
        primeImplicates3.add(new TreeSet<>(Collections.singletonList(this.NB)));
        this.result3 = new PrimeResult(primeImplicants3, primeImplicates3, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
    }

    @Test
    public void testGetters() {
        assertThat(this.result1.getPrimeImplicants()).hasSize(2);
        assertThat(this.result1.getPrimeImplicants().get(0)).containsExactly(this.A, this.NB);
        assertThat(this.result1.getPrimeImplicants().get(1)).containsExactly(this.A, this.C);
        assertThat(this.result2.getPrimeImplicants()).hasSize(2);
        assertThat(this.result2.getPrimeImplicants().get(0)).containsExactly(this.A, this.NB);
        assertThat(this.result2.getPrimeImplicants().get(1)).containsExactly(this.C);
        assertThat(this.result3.getPrimeImplicants()).hasSize(1);
        assertThat(this.result3.getPrimeImplicants().get(0)).isEmpty();

        assertThat(this.result1.getPrimeImplicates()).hasSize(1);
        assertThat(this.result1.getPrimeImplicates().get(0)).containsExactly(this.A, this.NB);
        assertThat(this.result2.getPrimeImplicates()).hasSize(0);
        assertThat(this.result3.getPrimeImplicates()).hasSize(1);
        assertThat(this.result3.getPrimeImplicates().get(0)).containsExactly(this.NB);

        assertThat(this.result1.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(this.result2.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(this.result3.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICATES_COMPLETE);
    }

    @Test
    public void testHashCode() {
        assertThat(this.result1.hashCode()).isEqualTo(this.result1.hashCode());
        final List<SortedSet<Literal>> primeImplicants = new ArrayList<>();
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.C)));
        final List<SortedSet<Literal>> primeImplicates = new ArrayList<>();
        primeImplicates.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        final PrimeResult otherResult = new PrimeResult(primeImplicants, primeImplicates, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(otherResult.hashCode()).isEqualTo(this.result1.hashCode());
    }

    @Test
    public void testEquals() {
        assertThat(this.result1.hashCode()).isEqualTo(this.result1.hashCode());
        final List<SortedSet<Literal>> primeImplicants = new ArrayList<>();
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.C)));
        final List<SortedSet<Literal>> primeImplicates = new ArrayList<>();
        primeImplicates.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        final PrimeResult otherResult = new PrimeResult(primeImplicants, primeImplicates, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(this.result1.equals(this.result1)).isTrue();
        assertThat(this.result1.equals(otherResult)).isTrue();
        assertThat(this.result1.equals(this.result2)).isFalse();
        assertThat(this.result2.equals(this.result1)).isFalse();
        assertThat(this.result1.equals(this.result3)).isFalse();
        assertThat(this.result1.equals("String")).isFalse();
        assertThat(this.result1.equals(null)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.result1.toString()).isEqualTo(
                "PrimeResult{" +
                        "primeImplicants=[[a, ~b], [a, c]]" +
                        ", primeImplicates=[[a, ~b]]" +
                        ", coverageInfo=IMPLICANTS_COMPLETE" +
                        '}');
    }
}
