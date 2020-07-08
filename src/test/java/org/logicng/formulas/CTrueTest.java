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

/**
 * Unit Tests for the class {@link CTrue}.
 * @version 2.0.0
 * @since 1.0
 */
public class CTrueTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.TRUE.type()).isEqualTo(FType.TRUE);
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.TRUE.numberOfAtoms()).isEqualTo(1);
        assertThat(this.TRUE.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNegation() {
        assertThat(this.TRUE.negate()).isEqualTo(this.FALSE);
    }

    @Test
    public void testVariables() {
        assertThat(this.TRUE.variables().size()).isEqualTo(0);
    }

    @Test
    public void testLiterals() {
        assertThat(this.TRUE.literals().size()).isEqualTo(0);
    }

    @Test
    public void testToString() {
        assertThat(this.TRUE.toString()).isEqualTo("$true");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.verum()).isEqualTo(this.TRUE);
        assertThat(this.f.falsum()).isNotEqualTo(this.TRUE);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(this.g.verum()).isEqualTo(this.TRUE);
        assertThat(this.g.falsum()).isNotEqualTo(this.TRUE);
    }

    @Test
    public void testHash() {
        assertThat(this.TRUE.hashCode()).isEqualTo(this.f.verum().hashCode());
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.TRUE.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(this.TRUE.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.TRUE.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.TRUE.isConstantFormula()).isTrue();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.TRUE.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(this.TRUE.containsVariable(this.f.variable("a"))).isFalse();
    }
}
