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

package org.logicng.solvers.maxsat;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.solvers.maxsat.algorithms.IncWBO;
import org.logicng.solvers.maxsat.algorithms.LinearSU;
import org.logicng.solvers.maxsat.algorithms.LinearUS;
import org.logicng.solvers.maxsat.algorithms.MSU3;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.maxsat.algorithms.WBO;
import org.logicng.solvers.maxsat.algorithms.WMSU3;

import java.util.Arrays;

/**
 * Unit tests for the package {@link org.logicng.solvers.maxsat}.
 * @version 2.0.0
 * @since 1.1
 */
public class MaxSATClassTest {

    @Test
    public void testMaxSATConfig() {
        assertThat(Arrays.asList(MaxSATConfig.SolverType.values()).contains(MaxSATConfig.SolverType.valueOf("GLUCOSE"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.IncrementalStrategy.values()).contains(MaxSATConfig.IncrementalStrategy.valueOf("ITERATIVE"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.AMOEncoding.values()).contains(MaxSATConfig.AMOEncoding.valueOf("LADDER"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.PBEncoding.values()).contains(MaxSATConfig.PBEncoding.valueOf("SWC"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.CardinalityEncoding.values()).contains(MaxSATConfig.CardinalityEncoding.valueOf("TOTALIZER"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.WeightStrategy.values()).contains(MaxSATConfig.WeightStrategy.valueOf("DIVERSIFY"))).isTrue();
        assertThat(Arrays.asList(MaxSATConfig.Verbosity.values()).contains(MaxSATConfig.Verbosity.valueOf("SOME"))).isTrue();
    }

    @Test
    public void testMaxSATenum() {
        assertThat(Arrays.asList(MaxSAT.ProblemType.values()).contains(MaxSAT.ProblemType.valueOf("UNWEIGHTED"))).isTrue();
        assertThat(Arrays.asList(MaxSAT.MaxSATResult.values()).contains(MaxSAT.MaxSATResult.valueOf("OPTIMUM"))).isTrue();
    }

    @Test
    public void testMaxSATtoString() {
        final MaxSAT wmsu3 = new WMSU3();
        assertThat(wmsu3.toString()).isEqualTo("WMSU3");
        final MaxSAT wbo = new WBO();
        assertThat(wbo.toString()).isEqualTo("WBO");
        final MaxSAT incWbo = new IncWBO();
        assertThat(incWbo.toString()).isEqualTo("IncWBO");
        final MaxSAT msu3 = new MSU3();
        assertThat(msu3.toString()).isEqualTo("MSU3");
        final MaxSAT linearSu = new LinearSU();
        assertThat(linearSu.toString()).isEqualTo("LinearSU");
        final MaxSAT linearUs = new LinearUS();
        assertThat(linearUs.toString()).isEqualTo("LinearUS");
    }
}
