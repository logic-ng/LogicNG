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

/**
 * Unit tests for the at-most-one configs.
 * @version 2.0.0
 * @since 1.0
 */
public class CCAMOTest implements LogicNGTest {

    private final CCConfig[] configs;

    public CCAMOTest() {
        this.configs = new CCConfig[14];
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
        this.configs[10] = CCConfig.builder().amoEncoding(BIMANDER).bimanderGroupSize(FIXED).bimanderFixedGroupSize(2).build();
        this.configs[11] = CCConfig.builder().amoEncoding(NESTED).nestingGroupSize(5).build();
        this.configs[12] = CCConfig.builder().amoEncoding(PRODUCT).productRecursiveBound(10).build();
        this.configs[13] = CCConfig.builder().amoEncoding(BEST).build();
    }

    @Test
    public void testAMO0() {
        final FormulaFactory f = new FormulaFactory();
        final Formula cc = f.amo();
        assertThat(cc).isEqualTo(f.verum());
    }

    @Test
    public void testAMO1() {
        final FormulaFactory f = new FormulaFactory();
        final CardinalityConstraint cc = (CardinalityConstraint) f.amo(f.variable("v0"));
        for (final CCConfig config : this.configs) {
            assertThat(new CCEncoder(f, config).encode(cc)).isEmpty();
        }
        assertThat(f.newCCVariable().name()).endsWith("_0");
    }

    @Test
    public void testAMOK() {
        final FormulaFactory f = new FormulaFactory();
        int counter = 0;
        for (final CCConfig config : this.configs) {
            if (config != null) {
                f.putConfiguration(config);
                testAMO(2, f, false);
                testAMO(10, f, false);
                testAMO(100, f, false);
                testAMO(250, f, false);
                testAMO(500, f, false);
                assertThat(f.newCCVariable().name()).endsWith("_" + counter++);
            }
        }
    }

    @Test
    public void testAMOKMiniCard() {
        final FormulaFactory f = new FormulaFactory();
        testAMO(2, f, true);
        testAMO(10, f, true);
        testAMO(100, f, true);
        testAMO(250, f, true);
        testAMO(500, f, true);
        assertThat(f.newCCVariable().name()).endsWith("_0");
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
        assertThat(this.configs[13].amoEncoder.toString()).isEqualTo("BEST");
        assertThat(new CCAMOPure().toString()).isEqualTo("CCAMOPure");
        assertThat(new CCAMOLadder().toString()).isEqualTo("CCAMOLadder");
        assertThat(new CCAMOProduct(2).toString()).isEqualTo("CCAMOProduct");
        assertThat(new CCAMOBinary().toString()).isEqualTo("CCAMOBinary");
        assertThat(new CCAMONested(2).toString()).isEqualTo("CCAMONested");
        assertThat(new CCAMOCommander(2).toString()).isEqualTo("CCAMOCommander");
        assertThat(new CCAMOBimander(2).toString()).isEqualTo("CCAMOBimander");

        assertThat(CCConfig.AMO_ENCODER.values()).contains(CCConfig.AMO_ENCODER.valueOf("LADDER"));
        assertThat(CCConfig.BIMANDER_GROUP_SIZE.values()).contains(CCConfig.BIMANDER_GROUP_SIZE.valueOf("SQRT"));
    }

    private void testAMO(final int numLits, final FormulaFactory f, final boolean miniCard) {
        final Variable[] problemLits = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            problemLits[i] = f.variable("v" + i);
        }
        final SATSolver solver = miniCard ? MiniSat.miniCard(f) : MiniSat.miniSat(f);
        solver.add(f.amo(problemLits));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(problemLits))
                .hasSize(numLits + 1)
                .allMatch(m -> m.positiveVariables().size() <= 1);
    }
}
