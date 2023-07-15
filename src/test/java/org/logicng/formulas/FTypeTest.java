// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for formula types.
 * @version 2.0.0
 * @since 2.0.0
 */
public class FTypeTest {

    @Test
    public void testDual() {
        assertThat(FType.dual(FType.AND)).isEqualTo(FType.OR);
        assertThat(FType.dual(FType.OR)).isEqualTo(FType.AND);

        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.FALSE));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.TRUE));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.LITERAL));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.NOT));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.IMPL));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.EQUIV));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.PBC));
    }
}
