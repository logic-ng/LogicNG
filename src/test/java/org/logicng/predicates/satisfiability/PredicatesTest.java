// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates.satisfiability;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_SAT;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_TAUTOLOGY;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Variable;
import org.logicng.testutils.PigeonHoleGenerator;

/**
 * Unit tests for the different satisfiability predicates.
 * @version 2.0.0
 * @since 1.0
 */
public class PredicatesTest extends TestWithExampleFormulas {

    private final FormulaPredicate sat = new SATPredicate(this.f);
    private final FormulaPredicate ctr = new ContradictionPredicate(this.f);
    private final FormulaPredicate tau = new TautologyPredicate(this.f);
    private final FormulaPredicate con = new ContingencyPredicate(this.f);

    @Test
    public void testTrue() {
        assertThat(this.TRUE.holds(this.sat)).isTrue();
        assertThat(this.TRUE.holds(this.ctr)).isFalse();
        assertThat(this.TRUE.holds(this.tau)).isTrue();
        assertThat(this.TRUE.holds(this.con)).isFalse();
    }

    @Test
    public void testFalse() {
        assertThat(this.FALSE.holds(this.sat)).isFalse();
        assertThat(this.FALSE.holds(this.ctr)).isTrue();
        assertThat(this.FALSE.holds(this.tau)).isFalse();
        assertThat(this.FALSE.holds(this.con)).isFalse();
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.holds(this.sat)).isTrue();
        assertThat(this.A.holds(this.ctr)).isFalse();
        assertThat(this.A.holds(this.tau)).isFalse();
        assertThat(this.A.holds(this.con)).isTrue();
        assertThat(this.NA.holds(this.sat)).isTrue();
        assertThat(this.NA.holds(this.ctr)).isFalse();
        assertThat(this.NA.holds(this.tau)).isFalse();
        assertThat(this.NA.holds(this.con)).isTrue();
    }

    @Test
    public void testOther() {
        assertThat(this.AND1.holds(this.sat)).isTrue();
        assertThat(this.AND1.holds(this.ctr)).isFalse();
        assertThat(this.AND1.holds(this.tau)).isFalse();
        assertThat(this.AND1.holds(this.con)).isTrue();
        assertThat(this.NOT2.holds(this.sat)).isTrue();
        assertThat(this.NOT2.holds(this.ctr)).isFalse();
        assertThat(this.NOT2.holds(this.tau)).isFalse();
        assertThat(this.NOT2.holds(this.con)).isTrue();
    }

    @Test
    public void testTaut() {
        final Formula taut = this.f.or(this.AND1, this.f.and(this.NA, this.B), this.f.and(this.A, this.NB), this.f.and(this.NA, this.NB));
        assertThat(taut.holds(this.sat)).isTrue();
        assertThat(taut.holds(this.ctr)).isFalse();
        assertThat(taut.holds(this.tau)).isTrue();
        assertThat(taut.holds(this.con)).isFalse();
    }

    @Test
    public void testCont() {
        final Formula cont = this.f.and(this.OR1, this.f.or(this.NX, this.Y), this.f.or(this.X, this.NY), this.f.or(this.NX, this.NY));
        assertThat(cont.holds(this.sat)).isFalse();
        assertThat(cont.holds(this.ctr)).isTrue();
        assertThat(cont.holds(this.tau)).isFalse();
        assertThat(cont.holds(this.con)).isFalse();
    }

    @Test
    public void testSat() {
        assertThat(this.AND1.holds(this.sat)).isTrue();
        assertThat(this.AND2.holds(this.sat)).isTrue();
        assertThat(this.AND3.holds(this.sat)).isTrue();
        assertThat(this.OR1.holds(this.sat)).isTrue();
        assertThat(this.OR2.holds(this.sat)).isTrue();
        assertThat(this.OR3.holds(this.sat)).isTrue();
        assertThat(this.NOT1.holds(this.sat)).isTrue();
        assertThat(this.NOT2.holds(this.sat)).isTrue();
        assertThat(new PigeonHoleGenerator(this.f).generate(1).holds(this.sat)).isFalse();
        assertThat(new PigeonHoleGenerator(this.f).generate(2).holds(this.sat)).isFalse();
        assertThat(new PigeonHoleGenerator(this.f).generate(3).holds(this.sat)).isFalse();
    }

    @Test
    public void testNotCache() {
        final Formula taut = this.f.or(this.AND1, this.f.and(this.NA, this.B), this.f.and(this.A, this.NB), this.f.and(this.NA, this.NB));
        taut.holds(this.tau, false);
        assertThat(taut.predicateCacheEntry(IS_TAUTOLOGY)).isEqualTo(Tristate.UNDEF);

        final Variable a = this.f.variable("A");
        final Variable b = this.f.variable("B");
        final Variable c = this.f.variable("C");
        final Variable d = this.f.variable("D");
        final Formula satDNF = this.f.or(this.f.and(a, b), this.f.and(b, c), this.f.and(d, a));
        assertThat(satDNF.holds(this.sat, false)).isTrue();
        assertThat(satDNF.predicateCacheEntry(IS_SAT)).isEqualTo(Tristate.UNDEF);
    }

    @Test
    public void testToString() {
        assertThat(new SATPredicate(this.f).toString()).isEqualTo("SATPredicate");
        assertThat(new TautologyPredicate(this.f).toString()).isEqualTo("TautologyPredicate");
        assertThat(new ContradictionPredicate(this.f).toString()).isEqualTo("ContradictionPredicate");
        assertThat(new ContingencyPredicate(this.f).toString()).isEqualTo("ContingencyPredicate");
    }
}
