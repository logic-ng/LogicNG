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

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.Arrays;

/**
 * Unit tests for the solver configurations.
 * @version 2.0.0
 * @since 1.0
 */
public class ConfigurationsTest extends TestWithExampleFormulas {

    @Test
    public void testMiniSatConfigToString() {
        final MiniSatConfig config = MiniSatConfig.builder()
                .varDecay(1.2)
                .varInc(1.3)
                .clMinimization(MiniSatConfig.ClauseMinimization.BASIC)
                .restartFirst(200)
                .restartInc(0.8)
                .clauseDecay(0.92)
                .removeSatisfied(true)
                .lsFactor(1.4)
                .lsInc(1.5)
                .incremental(false)
                .initialPhase(true)
                .build();
        final String expected = String.format("MiniSatConfig{%n" +
                "varDecay=1.2%n" +
                "varInc=1.3%n" +
                "clauseMin=BASIC%n" +
                "restartFirst=200%n" +
                "restartInc=0.8%n" +
                "clauseDecay=0.92%n" +
                "removeSatisfied=true%n" +
                "learntsizeFactor=1.4%n" +
                "learntsizeInc=1.5%n" +
                "incremental=false%n" +
                "initialPhase=true%n" +
                "proofGeneration=false%n" +
                "cnfMethod=PG_ON_SOLVER%n" +
                "auxiliaryVariablesInModels=false%n" +
                "bbInitialUBCheckForRotatableLiterals=true%n" +
                "bbCheckForComplementModelLiterals=true%n" +
                "bbCheckForRotatableLiterals=true%n" +
                "}");
        assertThat(config.toString()).isEqualTo(expected);
    }

    @Test
    public void testGlucoseConfigToString() {
        final GlucoseConfig config = GlucoseConfig.builder()
                .lbLBDMinimizingClause(3)
                .lbLBDFrozenClause(25)
                .lbSizeMinimizingClause(24)
                .firstReduceDB(1999)
                .specialIncReduceDB(999)
                .incReduceDB(299)
                .factorK(0.7)
                .factorR(1.3)
                .sizeLBDQueue(45)
                .sizeTrailQueue(4999)
                .reduceOnSize(true)
                .reduceOnSizeSize(10)
                .maxVarDecay(0.99)
                .build();
        final String expected = String.format("GlucoseConfig{%n" +
                "lbLBDMinimizingClause=3%n" +
                "lbLBDFrozenClause=25%n" +
                "lbSizeMinimizingClause=24%n" +
                "firstReduceDB=1999%n" +
                "specialIncReduceDB=999%n" +
                "incReduceDB=299%n" +
                "factorK=0.7%n" +
                "factorR=1.3%n" +
                "sizeLBDQueue=45%n" +
                "sizeTrailQueue=4999%n" +
                "reduceOnSize=true%n" +
                "reduceOnSizeSize=10%n" +
                "maxVarDecay=0.99%n" +
                "}");
        assertThat(config.toString()).isEqualTo(expected);
    }

    @Test
    public void testMaxSATConfigToString() {
        final MaxSATConfig config = MaxSATConfig.builder()
                .incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                .cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER)
                .weight(MaxSATConfig.WeightStrategy.DIVERSIFY)
                .solver(MaxSATConfig.SolverType.MINISAT)
                .verbosity(MaxSATConfig.Verbosity.SOME)
                .output(System.out)
                .symmetry(false)
                .limit(1000)
                .bmo(false)
                .build();
        final String expected = String.format("MaxSATConfig{%n" +
                "incrementalStrategy=ITERATIVE%n" +
                "pbEncoding=LADDER%n" +
                "pbEncoding=SWC%n" +
                "cardinalityEncoding=MTOTALIZER%n" +
                "weightStrategy=DIVERSIFY%n" +
                "solverType=MINISAT%n" +
                "verbosity=SOME%n" +
                "symmetry=false%n" +
                "limit=1000%n" +
                "bmo=false%n" +
                "}");
        assertThat(config.toString()).isEqualTo(expected);
    }

    @Test
    public void testFormulaRandomizerConfigToString() {
        final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                .seed(42)
                .numVars(21)
                .variables(Arrays.asList(this.A, this.B, this.C))
                .build();
        final String expected = String.format("FormulaRandomizerConfig{%n" +
                "seed=42%n" +
                "variables=[a, b, c]%n" +
                "numVars=21%n" +
                "weightConstant=0.1%n" +
                "weightPositiveLiteral=1.0%n" +
                "weightNegativeLiteral=1.0%n" +
                "weightOr=30.0%n" +
                "weightAnd=30.0%n" +
                "weightNot=1.0%n" +
                "weightImpl=1.0%n" +
                "weightEquiv=1.0%n" +
                "maximumOperandsAnd=5%n" +
                "maximumOperandsOr=5%n" +
                "weightPbc=0.0%n" +
                "weightPbcCoeffPositive=1.0%n" +
                "weightPbcCoeffNegative=0.2%n" +
                "weightPbcTypeLe=0.2%n" +
                "weightPbcTypeLt=0.2%n" +
                "weightPbcTypeGe=0.2%n" +
                "weightPbcTypeGt=0.2%n" +
                "weightPbcTypeEq=0.2%n" +
                "maximumOperandsPbc=5%n" +
                "maximumCoefficientPbc=10%n" +
                "weightCc=0.0%n" +
                "weightAmo=0.0%n" +
                "weightExo=0.0%n" +
                "maximumOperandsCc=5%n" +
                "}"
        );
        assertThat(config.toString()).isEqualTo(expected);
    }
}
