// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;

/**
 * Unit Tests for the class {@link CFalse}.
 * @version 2.3.0
 * @since 1.0
 */
public class CFalseTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.FALSE.type()).isEqualTo(FType.FALSE);
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.FALSE.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNegation() {
        assertThat(this.FALSE.negate()).isEqualTo(this.TRUE);
    }

    @Test
    public void testVariables() {
        assertThat(this.FALSE.variables().size()).isEqualTo(0);
    }

    @Test
    public void testLiterals() {
        assertThat(this.FALSE.literals().size()).isEqualTo(0);
    }

    @Test
    public void testToString() {
        assertThat(this.FALSE.toString()).isEqualTo("$false");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.falsum()).isEqualTo(this.FALSE);
        assertThat(this.f.verum()).isNotEqualTo(this.FALSE);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(this.g.falsum()).isEqualTo(this.FALSE);
        assertThat(this.g.verum()).isNotEqualTo(this.FALSE);
    }

    @Test
    public void testHash() {
        assertThat(this.FALSE.hashCode()).isEqualTo(this.f.falsum().hashCode());
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.FALSE.numberOfNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(this.FALSE.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.FALSE.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.FALSE.isConstantFormula()).isTrue();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.FALSE.isAtomicFormula()).isTrue();
    }

    @Test
    public void testContains() {
        assertThat(this.FALSE.containsVariable(this.f.variable("a"))).isFalse();
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
