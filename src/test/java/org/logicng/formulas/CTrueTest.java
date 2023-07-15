// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;

/**
 * Unit Tests for the class {@link CTrue}.
 * @version 2.3.0
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

    @Test
    public void testIsNNF() {
        assertThat(this.FALSE.isNNF()).isTrue();
    }

    @Test
    public void testIsDNF() {
        assertThat(this.FALSE.isDNF()).isTrue();
    }

    @Test
    public void testIsCNF() {
        assertThat(this.FALSE.isCNF()).isTrue();
    }
}
