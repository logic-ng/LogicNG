// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.explanations.mus;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.transformations.cnf.CNFConfig;

import java.util.Arrays;

/**
 * Unit tests for the class {@link MUSConfig}.
 * @version 2.0.0
 * @since 1.1
 */
public class MUSConfigTest {

    @Test
    public void testMUSConfiguration() {
        final MUSConfig config = MUSConfig.builder().algorithm(MUSConfig.Algorithm.valueOf("DELETION")).build();
        assertThat(config.toString()).isEqualTo(String.format("MUSConfig{%nalgorithm=DELETION%n}%n"));
        assertThat(Arrays.asList(CNFConfig.Algorithm.values())).contains(CNFConfig.Algorithm.valueOf("TSEITIN"));
    }
}
