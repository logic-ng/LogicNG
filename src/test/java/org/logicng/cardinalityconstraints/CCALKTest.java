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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.formulas.CType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;

import java.util.Arrays;

/**
 * Unit tests for the at-least-k configs.
 * @version 2.0.0
 * @since 1.0
 */
public class CCALKTest implements LogicNGTest {

    private final CCConfig[] configs;

    public CCALKTest() {
        this.configs = new CCConfig[3];
        this.configs[0] = CCConfig.builder().alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build();
        this.configs[1] = CCConfig.builder().alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build();
        this.configs[2] = CCConfig.builder().alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build();
    }

    @Test
    public void testALK() {
        final FormulaFactory f = new FormulaFactory();
        int counter = 0;
        for (final CCConfig config : this.configs) {
            f.putConfiguration(config);
            testCC(10, 0, 1, f);
            testCC(10, 1, 1023, f);
            testCC(10, 2, 1013, f);
            testCC(10, 3, 968, f);
            testCC(10, 4, 848, f);
            testCC(10, 5, 638, f);
            testCC(10, 6, 386, f);
            testCC(10, 7, 176, f);
            testCC(10, 8, 56, f);
            testCC(10, 9, 11, f);
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
        solver.add(f.cc(CType.GE, rhs, problemLits));
        if (expected != 0) {
            assertSolverSat(solver);
        } else {
            assertSolverUnsat(solver);
        }
        assertThat(solver.execute(ModelEnumerationFunction.builder().variables(problemLits).handler(new NumberOfModelsHandler(12000)).build()))
                .hasSize(expected)
                .allMatch(m -> m.positiveVariables().size() >= rhs);
    }

    @Test
    public void testIllegalCC1() {
        final FormulaFactory f = new FormulaFactory();
        final CCEncoder encoder = new CCEncoder(f);
        final int numLits = 100;
        final Variable[] problemLits = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            problemLits[i] = f.variable("v" + i);
        }
        assertThatThrownBy(() -> encoder.encode((CardinalityConstraint) f.cc(CType.GE, -1, problemLits))).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(this.configs[0].alkEncoder.toString()).isEqualTo("TOTALIZER");
        assertThat(this.configs[1].alkEncoder.toString()).isEqualTo("MODULAR_TOTALIZER");
        assertThat(this.configs[2].alkEncoder.toString()).isEqualTo("CARDINALITY_NETWORK");

        assertThat(new CCTotalizer().toString()).isEqualTo("CCTotalizer");
        assertThat(new CCModularTotalizer(f).toString()).isEqualTo("CCModularTotalizer");
        assertThat(new CCCardinalityNetworks().toString()).isEqualTo("CCCardinalityNetworks");

        assertThat(new CCALKTotalizer().toString()).isEqualTo("CCALKTotalizer");
        assertThat(new CCALKModularTotalizer(f).toString()).isEqualTo("CCALKModularTotalizer");
        assertThat(new CCALKCardinalityNetwork().toString()).isEqualTo("CCALKCardinalityNetwork");

        assertThat(Arrays.asList(CCConfig.ALK_ENCODER.values()).contains(CCConfig.ALK_ENCODER.valueOf("MODULAR_TOTALIZER")));
    }
}
