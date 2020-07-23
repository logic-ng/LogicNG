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
            swc.encode(new MiniSat2Solver(), new LNGIntVector(), new LNGIntVector(), Integer.MAX_VALUE, new LNGIntVector(), 1);
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
