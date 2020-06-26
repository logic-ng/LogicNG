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

package org.logicng.predicates.satisfiability;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Variable;
import org.logicng.testutils.PigeonHoleGenerator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_SAT;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_TAUTOLOGY;

/**
 * Unit tests for the different satisfiability predicates.
 * @version 2.0.0
 * @since 1.0
 */
public class PredicatesTest {

    private final FormulaFactory f = new FormulaFactory();
    private final FormulaPredicate sat = new SATPredicate(this.f);
    private final FormulaPredicate ctr = new ContradictionPredicate(this.f);
    private final FormulaPredicate tau = new TautologyPredicate(this.f);
    private final FormulaPredicate con = new ContingencyPredicate(this.f);

    @Test
    public void testTrue() {
        assertThat(F.TRUE.holds(this.sat)).isTrue();
        assertThat(F.TRUE.holds(this.ctr)).isFalse();
        assertThat(F.TRUE.holds(this.tau)).isTrue();
        assertThat(F.TRUE.holds(this.con)).isFalse();
    }

    @Test
    public void testFalse() {
        assertThat(F.FALSE.holds(this.sat)).isFalse();
        assertThat(F.FALSE.holds(this.ctr)).isTrue();
        assertThat(F.FALSE.holds(this.tau)).isFalse();
        assertThat(F.FALSE.holds(this.con)).isFalse();
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.holds(this.sat)).isTrue();
        assertThat(F.A.holds(this.ctr)).isFalse();
        assertThat(F.A.holds(this.tau)).isFalse();
        assertThat(F.A.holds(this.con)).isTrue();
        assertThat(F.NA.holds(this.sat)).isTrue();
        assertThat(F.NA.holds(this.ctr)).isFalse();
        assertThat(F.NA.holds(this.tau)).isFalse();
        assertThat(F.NA.holds(this.con)).isTrue();
    }

    @Test
    public void testOther() {
        assertThat(F.AND1.holds(this.sat)).isTrue();
        assertThat(F.AND1.holds(this.ctr)).isFalse();
        assertThat(F.AND1.holds(this.tau)).isFalse();
        assertThat(F.AND1.holds(this.con)).isTrue();
        assertThat(F.NOT2.holds(this.sat)).isTrue();
        assertThat(F.NOT2.holds(this.ctr)).isFalse();
        assertThat(F.NOT2.holds(this.tau)).isFalse();
        assertThat(F.NOT2.holds(this.con)).isTrue();
    }

    @Test
    public void testTaut() {
        final FormulaFactory f = F.f;
        final Formula taut = f.or(F.AND1, f.and(F.NA, F.B), f.and(F.A, F.NB), f.and(F.NA, F.NB));
        assertThat(taut.holds(this.sat)).isTrue();
        assertThat(taut.holds(this.ctr)).isFalse();
        assertThat(taut.holds(this.tau)).isTrue();
        assertThat(taut.holds(this.con)).isFalse();
    }

    @Test
    public void testCont() {
        final FormulaFactory f = F.f;
        final Formula cont = f.and(F.OR1, f.or(F.NX, F.Y), f.or(F.X, F.NY), f.or(F.NX, F.NY));
        assertThat(cont.holds(this.sat)).isFalse();
        assertThat(cont.holds(this.ctr)).isTrue();
        assertThat(cont.holds(this.tau)).isFalse();
        assertThat(cont.holds(this.con)).isFalse();
    }

    @Test
    public void testSat() {
        assertThat(F.AND1.holds(this.sat)).isTrue();
        assertThat(F.AND2.holds(this.sat)).isTrue();
        assertThat(F.AND3.holds(this.sat)).isTrue();
        assertThat(F.OR1.holds(this.sat)).isTrue();
        assertThat(F.OR2.holds(this.sat)).isTrue();
        assertThat(F.OR3.holds(this.sat)).isTrue();
        assertThat(F.NOT1.holds(this.sat)).isTrue();
        assertThat(F.NOT2.holds(this.sat)).isTrue();
        assertThat(new PigeonHoleGenerator(F.f).generate(1).holds(this.sat)).isFalse();
        assertThat(new PigeonHoleGenerator(F.f).generate(2).holds(this.sat)).isFalse();
        assertThat(new PigeonHoleGenerator(F.f).generate(3).holds(this.sat)).isFalse();
    }

    @Test
    public void testNotCache() {
        final FormulaFactory f = F.f;
        final Formula taut = f.or(F.AND1, f.and(F.NA, F.B), f.and(F.A, F.NB), f.and(F.NA, F.NB));
        taut.holds(this.tau, false);
        assertThat(taut.predicateCacheEntry(IS_TAUTOLOGY)).isEqualTo(Tristate.UNDEF);

        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Variable d = f.variable("D");
        final Formula satDNF = f.or(f.and(a, b), f.and(b, c), f.and(d, a));
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
