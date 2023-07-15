// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;

/**
 * Unit tests for the term predicate.
 * @version 2.3.0
 * @since 2.2.0
 */
public class TermPredicateTest extends TestWithExampleFormulas {

    private final TermPredicate mintermPredicate = TermPredicate.minterm();
    private final TermPredicate maxtermPredicate = TermPredicate.maxterm();

    @Test
    public void testMintermPredicate() {
        assertThat(this.f.verum().holds(this.mintermPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.mintermPredicate)).isTrue();
        assertThat(this.A.holds(this.mintermPredicate)).isTrue();
        assertThat(this.NA.holds(this.mintermPredicate)).isTrue();
        assertThat(this.AND1.holds(this.mintermPredicate)).isTrue();
        assertThat(this.OR1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.OR3.holds(this.mintermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.AND2, this.A, this.NY).holds(this.mintermPredicate)).isFalse();
        assertThat(this.PBC1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.AND3.holds(this.mintermPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.mintermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.EQ1).holds(this.mintermPredicate)).isFalse();
    }

    @Test
    public void testMaxtermPredicate() {
        assertThat(this.f.verum().holds(this.maxtermPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.maxtermPredicate)).isTrue();
        assertThat(this.A.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.NA.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.AND1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.OR1.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.OR3.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.AND2, this.A, this.NY).holds(this.maxtermPredicate)).isFalse();
        assertThat(this.PBC1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.AND3.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.EQ1).holds(this.maxtermPredicate)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.mintermPredicate.toString()).isEqualTo("TermPredicate");
        assertThat(this.maxtermPredicate.toString()).isEqualTo("TermPredicate");
    }
}
