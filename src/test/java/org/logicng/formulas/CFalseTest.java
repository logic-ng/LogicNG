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
 * Unit Tests for the class {@link CFalse}.
 * @version 2.0.0
 * @since 1.0
 */
public class CFalseTest {

    @Test
    public void testType() {
        assertThat(F.FALSE.type()).isEqualTo(FType.FALSE);
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.FALSE.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNegation() {
        assertThat(F.FALSE.negate()).isEqualTo(F.TRUE);
    }

    @Test
    public void testVariables() {
        assertThat(F.FALSE.variables().size()).isEqualTo(0);
    }

    @Test
    public void testLiterals() {
        assertThat(F.FALSE.literals().size()).isEqualTo(0);
    }

    @Test
    public void testToString() {
        assertThat(F.FALSE.toString()).isEqualTo("$false");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.falsum()).isEqualTo(F.FALSE);
        assertThat(F.f.verum()).isNotEqualTo(F.FALSE);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.falsum()).isEqualTo(F.FALSE);
        assertThat(F.g.verum()).isNotEqualTo(F.FALSE);
    }

    @Test
    public void testHash() {
        assertThat(F.FALSE.hashCode()).isEqualTo(F.f.falsum().hashCode());
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.FALSE.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(F.FALSE.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.FALSE.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.FALSE.isConstantFormula()).isTrue();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.FALSE.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(F.FALSE.containsVariable(F.f.variable("a"))).isFalse();
    }
}
