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

package org.logicng.cardinalityconstraints;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

/**
 * Unit tests for the exactly-k encoders.
 * @version 2.0.0
 * @since 1.1
 */
public class CCEXKTest implements LogicNGTest {

    private final CCConfig[] configs;

    public CCEXKTest() {
        this.configs = new CCConfig[2];
        this.configs[0] = CCConfig.builder().exkEncoding(CCConfig.EXK_ENCODER.TOTALIZER).build();
        this.configs[1] = CCConfig.builder().exkEncoding(CCConfig.EXK_ENCODER.CARDINALITY_NETWORK).build();
    }

    @Test
    public void testEXK() {
        final FormulaFactory f = new FormulaFactory();
        int counter = 0;
        for (final CCConfig config : this.configs) {
            f.putConfiguration(config);
            testCC(10, 1, 10, f);
            testCC(10, 2, 45, f);
            testCC(10, 3, 120, f);
            testCC(10, 4, 210, f);
            testCC(10, 5, 252, f);
            testCC(10, 6, 210, f);
            testCC(10, 7, 120, f);
            testCC(10, 8, 45, f);
            testCC(10, 9, 10, f);
            testCC(10, 10, 1, f);
            testCC(10, 12, 0, f);
            assertThat(f.newCCVariable().name()).endsWith("_" + counter++);
        }
    }

    private void testCC(final int numLits, final int rhs, final int expected, final FormulaFactory f) {
        final Variable[] problemLits = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            problemLits[i] = f.variable("v" + i);
        }
        final SATSolver solver = MiniSat.miniSat(f);
        solver.add(f.cc(CType.EQ, rhs, problemLits));
        if (expected != 0) {
            assertSolverSat(solver);
        } else {
            assertSolverUnsat(solver);
        }
        assertThat(solver.enumerateAllModels(problemLits))
                .hasSize(expected)
                .allMatch(m -> m.positiveVariables().size() == rhs);
    }

    @Test
    public void testCCEXKTotalizer() {
        final FormulaFactory f = new FormulaFactory();
        final CCEXKTotalizer totalizer = new CCEXKTotalizer();
        totalizer.build(EncodingResult.resultForFormula(f), new Variable[]{f.variable("A"), f.variable("B"), f.variable("C")}, 2);
        assertThat(totalizer.incrementalData()).isNull();
        assertThat(totalizer.toString()).isEqualTo("CCEXKTotalizer");

        final CCEXKCardinalityNetwork cNetwork = new CCEXKCardinalityNetwork();
        cNetwork.build(EncodingResult.resultForFormula(f), new Variable[]{f.variable("A"), f.variable("B"), f.variable("C")}, 2);
        assertThat(cNetwork.incrementalData()).isNull();
        assertThat(cNetwork.toString()).isEqualTo("CCEXKCardinalityNetwork");
    }

    @Test
    public void testToString() {
        assertThat(this.configs[0].exkEncoder.toString()).isEqualTo("TOTALIZER");
        assertThat(this.configs[1].exkEncoder.toString()).isEqualTo("CARDINALITY_NETWORK");

        assertThat(CCConfig.EXK_ENCODER.values()).contains(CCConfig.EXK_ENCODER.valueOf("CARDINALITY_NETWORK"));
    }
}
