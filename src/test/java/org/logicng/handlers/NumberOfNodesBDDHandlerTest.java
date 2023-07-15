// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link NumberOfNodesBDDHandler}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class NumberOfNodesBDDHandlerTest {

    @Test
    public void testInvalidBound() {
        assertThatThrownBy(() -> new NumberOfNodesBDDHandler(-2))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("The bound for added nodes must be equal or greater than 0.");
    }
}
