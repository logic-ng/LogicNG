// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for the package data structures.
 * @version 2.0.0
 * @since 1.1
 */
public class DatastructuresTest {

    @Test
    public void testTristate() {
        assertThat(Tristate.valueOf("TRUE")).isEqualTo(Tristate.TRUE);
        assertThat(Tristate.valueOf("FALSE")).isEqualTo(Tristate.FALSE);
        assertThat(Tristate.valueOf("UNDEF")).isEqualTo(Tristate.UNDEF);
    }

    @Test
    public void testEncodingAuxiliaryVariable() {
        final EncodingAuxiliaryVariable eav = new EncodingAuxiliaryVariable("var", false);
        assertThat(eav.toString()).isEqualTo("var");
    }
}
