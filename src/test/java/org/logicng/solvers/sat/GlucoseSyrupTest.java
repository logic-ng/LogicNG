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

package org.logicng.solvers.sat;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.collections.LNGIntVector;

/**
 * Unit tests for the class {@link GlucoseSyrup}
 * @version 2.0.0
 * @since 1.1
 */
public class GlucoseSyrupTest {

    private final GlucoseSyrup gs = new GlucoseSyrup();

    @BeforeEach
    public void prepare() {
        this.gs.newVar(true, true);
        this.gs.newVar(true, true);
        this.gs.newVar(true, true);
        this.gs.newVar(true, true);
        this.gs.addClause(clause(1, 2, 3), null);
    }

    @Test
    public void testSaveState() {
        assertThatThrownBy(this.gs::saveState).isInstanceOf(UnsupportedOperationException.class);
    }

    @Test
    public void testLoadState() {
        assertThatThrownBy(() -> this.gs.loadState(new int[5])).isInstanceOf(UnsupportedOperationException.class);
    }

    @Test
    public void testToString() {
        final String expected = String.format("ok            true%n" +
                "qhead         0%n" +
                "#clauses      1%n" +
                "#learnts      0%n" +
                "#watches      8%n" +
                "#vars         4%n" +
                "#orderheap    4%n" +
                "#trail        0%n" +
                "#trailLim     0%n" +
                "model         []%n" +
                "conflict      []%n" +
                "assumptions   []%n" +
                "#seen         4%n" +
                "#stack        0%n" +
                "#toclear      0%n" +
                "claInc        1.0%n" +
                "simpDBAssigns -1%n" +
                "simpDBProps   0%n" +
                "#clause lits  3%n" +
                "#learnts lits 0%n");
        assertThat(this.gs.toString()).isEqualTo(expected);
    }

    private LNGIntVector clause(final int... lits) {
        final LNGIntVector c = new LNGIntVector(lits.length);
        for (final int l : lits) {
            c.push(literal(l));
        }
        return c;
    }

    private int literal(final int l) {
        return l < 0 ? (-l * 2) ^ 1 : l * 2;
    }
}
