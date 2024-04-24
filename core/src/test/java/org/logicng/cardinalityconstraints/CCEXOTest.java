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
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BEST;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BIMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BINARY;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.COMMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.LADDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.NESTED;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PRODUCT;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PURE;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.FIXED;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.HALF;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.SQRT;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Unit tests for the exactly-one encoders.
 * @version 2.0.0
 * @since 1.0
 */
public class CCEXOTest implements LogicNGTest {

    private final CCConfig[] configs;

    public CCEXOTest() {
        this.configs = new CCConfig[11];
        this.configs[0] = CCConfig.builder().amoEncoding(PURE).build();
        this.configs[1] = CCConfig.builder().amoEncoding(LADDER).build();
        this.configs[2] = CCConfig.builder().amoEncoding(PRODUCT).build();
        this.configs[3] = CCConfig.builder().amoEncoding(BINARY).build();
        this.configs[4] = CCConfig.builder().amoEncoding(NESTED).build();
        this.configs[5] = CCConfig.builder().amoEncoding(COMMANDER).commanderGroupSize(3).build();
        this.configs[6] = CCConfig.builder().amoEncoding(COMMANDER).commanderGroupSize(7).build();
        this.configs[7] = CCConfig.builder().amoEncoding(BIMANDER).bimanderGroupSize(FIXED).build();
        this.configs[8] = CCConfig.builder().amoEncoding(BIMANDER).bimanderGroupSize(HALF).build();
        this.configs[9] = CCConfig.builder().amoEncoding(BIMANDER).bimanderGroupSize(SQRT).build();
        this.configs[10] = CCConfig.builder().amoEncoding(BEST).build();
    }

    @Test
    public void testEXO0() {
        final FormulaFactory f = new FormulaFactory();
        final Formula cc = f.exo();
        assertThat(cc).isEqualTo(f.falsum());
    }

    @Test
    public void testEXO1() {
        final FormulaFactory f = new FormulaFactory();
        final CardinalityConstraint cc = (CardinalityConstraint) f.exo(f.variable("v0"));
        for (final CCConfig config : this.configs) {
            assertThat(new CCEncoder(f, config).encode(cc)).containsExactly(f.variable("v0"));
        }
        assertThat(f.newCCVariable().name()).endsWith("_0");
    }

    @Test
    public void testEXOK() {
        final FormulaFactory f = new FormulaFactory();
        int counter = 0;
        for (final CCConfig config : this.configs) {
            if (config != null) {
                f.putConfiguration(config);
                testEXO(2, f);
                testEXO(10, f);
                testEXO(100, f);
                testEXO(250, f);
                testEXO(500, f);
                assertThat(f.newCCVariable().name()).endsWith("_" + counter++);
            }
        }
    }

    @Test
    public void testEncodingSetting() {
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(CCConfig.builder().amoEncoding(PURE).build());
        final CardinalityConstraint exo = (CardinalityConstraint) f.exo(IntStream.range(0, 100).mapToObj(i -> f.variable("v" + i)).collect(Collectors.toList()));
        assertThat(exo.cnf().variables()).hasSize(100);
        assertThat(exo.cnf().numberOfOperands()).isEqualTo(4951);
    }

    @Test
    public void testToString() {
        assertThat(this.configs[0].amoEncoder.toString()).isEqualTo("PURE");
        assertThat(this.configs[1].amoEncoder.toString()).isEqualTo("LADDER");
        assertThat(this.configs[2].amoEncoder.toString()).isEqualTo("PRODUCT");
        assertThat(this.configs[3].amoEncoder.toString()).isEqualTo("BINARY");
        assertThat(this.configs[4].amoEncoder.toString()).isEqualTo("NESTED");
        assertThat(this.configs[5].amoEncoder.toString()).isEqualTo("COMMANDER");
        assertThat(this.configs[7].amoEncoder.toString()).isEqualTo("BIMANDER");
    }

    private void testEXO(final int numLits, final FormulaFactory f) {
        final Variable[] problemLits = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            problemLits[i] = f.variable("v" + i);
        }
        final SATSolver solver = MiniSat.miniSat(f);
        solver.add(f.exo(problemLits));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(problemLits))
                .hasSize(numLits)
                .allMatch(m -> m.positiveVariables().size() == 1);
    }
}
