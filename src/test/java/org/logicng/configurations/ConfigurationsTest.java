// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.configurations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for the package configurations.
 * @version 2.0.0
 * @since 1.1
 */
public class ConfigurationsTest {

    @Test
    public void testValueOf() {
        assertThat(ConfigurationType.valueOf("CNF")).isEqualTo(ConfigurationType.CNF);
        assertThat(ConfigurationType.valueOf("GLUCOSE")).isEqualTo(ConfigurationType.GLUCOSE);
        assertThat(ConfigurationType.valueOf("MAXSAT")).isEqualTo(ConfigurationType.MAXSAT);
        assertThat(ConfigurationType.valueOf("CC_ENCODER")).isEqualTo(ConfigurationType.CC_ENCODER);
    }
}
