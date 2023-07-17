// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.solvers.maxsat.encodings;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.collections.LNGIntVector;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.sat.MiniSat2Solver;

/**
 * Unit test for the package {@link org.logicng.solvers.maxsat.encodings}.
 * @version 2.0.0
 * @since 1.1
 */
public class EncodingsTest extends TestWithExampleFormulas {

    @Test
    public void testEncoder() {
        final Encoder encoder = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
        assertThat(encoder.toString()).isEqualTo("Encoder");
    }

    @Test
    public void testTotalizer() {
        final Totalizer totalizer = new Totalizer(MaxSATConfig.IncrementalStrategy.ITERATIVE);
        assertThat(totalizer.incremental()).isEqualTo(MaxSATConfig.IncrementalStrategy.ITERATIVE);
        assertThat(totalizer.toString()).isEqualTo("Totalizer");
    }

    @Test
    public void testModularTotalizer() {
        final ModularTotalizer mTotalizer = new ModularTotalizer();
        assertThat(mTotalizer.hasCreatedEncoding()).isEqualTo(false);
        assertThat(mTotalizer.toString()).isEqualTo("ModularTotalizer");
    }

    @Test
    public void testSequentialWeightCounterExceptionalBehavior() {
        assertThatThrownBy(() -> {
            final SequentialWeightCounter swc = new SequentialWeightCounter();
            swc.encode(new MiniSat2Solver(), new LNGIntVector(), new LNGIntVector(), Integer.MAX_VALUE);
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Overflow in the encoding.");
        assertThatThrownBy(() -> {
            final SequentialWeightCounter swc = new SequentialWeightCounter();
            swc.encode(new MiniSat2Solver(), new LNGIntVector(), new LNGIntVector(), Integer.MAX_VALUE,
                    new LNGIntVector(), 1);
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Overflow in the encoding.");
    }

    @Test
    public void testSequentialWeightCounter() {
        final SequentialWeightCounter swc = new SequentialWeightCounter();
        assertThat(swc.hasCreatedEncoding()).isEqualTo(false);
        assertThat(swc.toString()).isEqualTo("SequentialWeightCounter");
    }

    @Test
    public void testLadder() {
        final Ladder ladder = new Ladder();
        assertThat(ladder.toString()).isEqualTo("Ladder");
    }
}
