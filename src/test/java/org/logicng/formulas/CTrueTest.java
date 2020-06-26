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

/**
 * Unit Tests for the class {@link CTrue}.
 * @version 2.0.0
 * @since 1.0
 */
public class CTrueTest {

    @Test
    public void testType() {
        assertThat(F.TRUE.type()).isEqualTo(FType.TRUE);
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.TRUE.numberOfAtoms()).isEqualTo(1);
        assertThat(F.TRUE.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNegation() {
        assertThat(F.TRUE.negate()).isEqualTo(F.FALSE);
    }

    @Test
    public void testVariables() {
        assertThat(F.TRUE.variables().size()).isEqualTo(0);
    }

    @Test
    public void testLiterals() {
        assertThat(F.TRUE.literals().size()).isEqualTo(0);
    }

    @Test
    public void testToString() {
        assertThat(F.TRUE.toString()).isEqualTo("$true");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.verum()).isEqualTo(F.TRUE);
        assertThat(F.f.falsum()).isNotEqualTo(F.TRUE);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.verum()).isEqualTo(F.TRUE);
        assertThat(F.g.falsum()).isNotEqualTo(F.TRUE);
    }

    @Test
    public void testHash() {
        assertThat(F.TRUE.hashCode()).isEqualTo(F.f.verum().hashCode());
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.TRUE.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(F.TRUE.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.TRUE.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.TRUE.isConstantFormula()).isTrue();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.TRUE.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(F.TRUE.containsVariable(F.f.variable("a"))).isFalse();
    }
}
