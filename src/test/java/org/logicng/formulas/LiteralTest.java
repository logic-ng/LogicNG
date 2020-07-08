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
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Substitution;

/**
 * Unit Tests for the class {@link Literal}.
 * @version 2.0.0
 * @since 1.0
 */
public class LiteralTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.A.type()).isEqualTo(FType.LITERAL);
        assertThat(this.NA.type()).isEqualTo(FType.LITERAL);
    }

    @Test
    public void testShortcutCreators() {
        assertThat(this.f.literal("a", true) == this.f.variable("a")).isTrue();
        assertThat(this.f.literal("name", true) == this.f.variable("name")).isTrue();
    }

    @Test
    public void testNegation() {
        assertThat(this.A.negate() == this.NA).isTrue();
        assertThat(this.NA.negate() == this.A).isTrue();
    }

    @Test
    public void testGetters() {
        assertThat(this.A.name()).isEqualTo("a");
        assertThat(this.NA.name()).isEqualTo("a");
        assertThat(this.A.phase()).isEqualTo(true);
        assertThat(this.NA.phase()).isEqualTo(false);
    }

    @Test
    public void testVariables() {
        assertThat(this.A.variables)
                .hasSize(1)
                .containsExactly(this.A);
        assertThat(this.NA.variables)
                .hasSize(1)
                .containsExactly(this.A);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.literals())
                .hasSize(1)
                .containsExactly(this.A);
        assertThat(this.NA.literals())
                .hasSize(1)
                .containsExactly(this.NA);
    }

    @Test
    public void testExpSubstitution() {
        final Substitution substitution = new Substitution();
        substitution.addMapping(this.f.variable("a"), this.f.literal("b", false));
        substitution.addMapping(this.f.variable("c"), this.f.variable("d"));
        substitution.addMapping(this.f.variable("x"), this.f.and(this.f.variable("y"), this.f.variable("z")));
    }

    @Test
    public void testToString() {
        assertThat(this.A.toString()).isEqualTo("a");
        assertThat(this.NA.toString()).isEqualTo("~a");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.literal("a", true).equals(this.A)).isTrue();
        assertThat(this.f.literal("a", false).equals(this.NA)).isTrue();
        assertThat(this.A.equals(this.A)).isTrue();
        assertThat(this.B.equals(this.A)).isFalse();
        assertThat(this.NA.equals(this.A)).isFalse();
        assertThat(this.f.falsum()).isNotEqualTo(this.A);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(this.g.literal("a", true).equals(this.A)).isTrue();
        assertThat(this.g.literal("a", false).equals(this.NA)).isTrue();
        assertThat(this.g.literal("a", false).equals(this.A)).isFalse();
        assertThat(this.g.literal("b", true).equals(this.A)).isFalse();
        assertThat(this.g.falsum()).isNotEqualTo(this.A);
    }

    @Test
    public void testCompareTo() {
        assertThat(this.A.compareTo(this.A) == 0).isTrue();
        assertThat(this.NA.compareTo(this.NA) == 0).isTrue();
        assertThat(this.A.compareTo(this.NA) < 0).isTrue();
        assertThat(this.A.compareTo(this.NB) < 0).isTrue();
        assertThat(this.A.compareTo(this.B) < 0).isTrue();
        assertThat(this.A.compareTo(this.X) < 0).isTrue();
        assertThat(this.NA.compareTo(this.NX) < 0).isTrue();
    }

    @Test
    public void testHash() {
        assertThat(this.f.literal("a", true).hashCode()).isEqualTo(this.A.hashCode());
        assertThat(this.f.literal("a", false).hashCode()).isEqualTo(this.NA.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.A.numberOfAtoms()).isEqualTo(1);
        assertThat(this.NA.numberOfAtoms()).isEqualTo(1);
        assertThat(this.NA.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.A.numberOfNodes()).isEqualTo(1);
        assertThat(this.NA.numberOfNodes()).isEqualTo(1);
        assertThat(this.NA.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(this.A.numberOfInternalNodes()).isEqualTo(1);
        assertThat(this.NA.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.A.numberOfOperands()).isEqualTo(0);
        assertThat(this.NA.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.A.isConstantFormula()).isFalse();
        assertThat(this.NA.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.A.isAtomicFormula()).isTrue();
        assertThat(this.NA.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(this.A.containsVariable(this.f.variable("b"))).isFalse();
        assertThat(this.A.containsVariable(this.f.variable("a"))).isTrue();
        assertThat(this.NA.containsVariable(this.f.variable("b"))).isFalse();
        assertThat(this.NA.containsVariable(this.f.variable("a"))).isTrue();
    }

    @Test
    public void testPosNeg() {
        assertThat(this.A.variable() == this.A).isTrue();
        assertThat(this.NA.variable() == this.A).isTrue();
    }
}
