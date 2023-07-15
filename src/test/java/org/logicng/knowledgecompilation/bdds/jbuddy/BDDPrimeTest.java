// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.jbuddy;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;

import org.junit.jupiter.api.Test;

public class BDDPrimeTest {

    @Test
    public void testNumberOfBits() {
        assertThat(BDDPrime.numberOfBits(0)).isEqualTo(0);
        assertThat(BDDPrime.numberOfBits(1)).isEqualTo(1);
    }

}
