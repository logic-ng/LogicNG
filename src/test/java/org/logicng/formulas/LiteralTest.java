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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Substitution;

/**
 * Unit Tests for the class {@link Literal}.
 * @version 2.0.0
 * @since 1.0
 */
public class LiteralTest {

    @Test
    public void testType() {
        assertThat(F.A.type()).isEqualTo(FType.LITERAL);
        assertThat(F.NA.type()).isEqualTo(FType.LITERAL);
    }

    @Test
    public void testShortcutCreators() {
        assertThat(F.f.literal("a", true) == F.f.variable("a")).isTrue();
        assertThat(F.f.literal("name", true) == F.f.variable("name")).isTrue();
    }

    @Test
    public void testNegation() {
        assertThat(F.A.negate() == F.NA).isTrue();
        assertThat(F.NA.negate() == F.A).isTrue();
    }

    @Test
    public void testGetters() {
        assertThat(F.A.name()).isEqualTo("a");
        assertThat(F.NA.name()).isEqualTo("a");
        assertThat(F.A.phase()).isEqualTo(true);
        assertThat(F.NA.phase()).isEqualTo(false);
    }

    @Test
    public void testVariables() {
        assertThat(F.A.variables)
                .hasSize(1)
                .containsExactly(F.A);
        assertThat(F.NA.variables)
                .hasSize(1)
                .containsExactly(F.A);
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.literals())
                .hasSize(1)
                .containsExactly(F.A);
        assertThat(F.NA.literals())
                .hasSize(1)
                .containsExactly(F.NA);
    }

    @Test
    public void testExpSubstitution() {
        final Substitution substitution = new Substitution();
        substitution.addMapping(F.f.variable("a"), F.f.literal("b", false));
        substitution.addMapping(F.f.variable("c"), F.f.variable("d"));
        substitution.addMapping(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z")));
    }

    @Test
    public void testToString() {
        assertThat(F.A.toString()).isEqualTo("a");
        assertThat(F.NA.toString()).isEqualTo("~a");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.literal("a", true).equals(F.A)).isTrue();
        assertThat(F.f.literal("a", false).equals(F.NA)).isTrue();
        assertThat(F.A.equals(F.A)).isTrue();
        assertThat(F.B.equals(F.A)).isFalse();
        assertThat(F.NA.equals(F.A)).isFalse();
        assertThat(F.f.falsum()).isNotEqualTo(F.A);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.literal("a", true).equals(F.A)).isTrue();
        assertThat(F.g.literal("a", false).equals(F.NA)).isTrue();
        assertThat(F.g.literal("a", false).equals(F.A)).isFalse();
        assertThat(F.g.literal("b", true).equals(F.A)).isFalse();
        assertThat(F.g.falsum()).isNotEqualTo(F.A);
    }

    @Test
    public void testCompareTo() {
        assertThat(F.A.compareTo(F.A) == 0).isTrue();
        assertThat(F.NA.compareTo(F.NA) == 0).isTrue();
        assertThat(F.A.compareTo(F.NA) < 0).isTrue();
        assertThat(F.A.compareTo(F.NB) < 0).isTrue();
        assertThat(F.A.compareTo(F.B) < 0).isTrue();
        assertThat(F.A.compareTo(F.X) < 0).isTrue();
        assertThat(F.NA.compareTo(F.NX) < 0).isTrue();
    }

    @Test
    public void testHash() {
        assertThat(F.f.literal("a", true).hashCode()).isEqualTo(F.A.hashCode());
        assertThat(F.f.literal("a", false).hashCode()).isEqualTo(F.NA.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.A.numberOfAtoms()).isEqualTo(1);
        assertThat(F.NA.numberOfAtoms()).isEqualTo(1);
        assertThat(F.NA.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.A.numberOfNodes()).isEqualTo(1);
        assertThat(F.NA.numberOfNodes()).isEqualTo(1);
        assertThat(F.NA.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(F.A.numberOfInternalNodes()).isEqualTo(1);
        assertThat(F.NA.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.A.numberOfOperands()).isEqualTo(0);
        assertThat(F.NA.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.A.isConstantFormula()).isFalse();
        assertThat(F.NA.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.A.isAtomicFormula()).isTrue();
        assertThat(F.NA.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(F.A.containsVariable(F.f.variable("b"))).isFalse();
        assertThat(F.A.containsVariable(F.f.variable("a"))).isTrue();
        assertThat(F.NA.containsVariable(F.f.variable("b"))).isFalse();
        assertThat(F.NA.containsVariable(F.f.variable("a"))).isTrue();
    }

    @Test
    public void testPosNeg() {
        assertThat(F.A.variable() == F.A).isTrue();
        assertThat(F.NA.variable() == F.A).isTrue();
    }
}
