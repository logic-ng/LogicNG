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

package org.logicng.transformations.cnf;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.LongRunningTag;
import org.logicng.backbones.Backbone;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Unit Tests for the CNF transformations in {@link CNFConfig} and {@link org.logicng.solvers.sat.MiniSatConfig.CNFMethod}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class CnfMethodComparisonTest {

    public static Collection<Object[]> cnfConfigurations() {
        final List<Object[]> configs = new ArrayList<>();
        configs.add(new Object[]{CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(12).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(0).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(0).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(12).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder()
                .algorithm(CNFConfig.Algorithm.ADVANCED).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder()
                .algorithm(CNFConfig.Algorithm.ADVANCED).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.TSEITIN).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF});
        configs.add(new Object[]{CNFConfig.builder().build(),
                MiniSatConfig.CNFMethod.PG_ON_SOLVER});
        configs.add(new Object[]{CNFConfig.builder().build(),
                MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER});
        return configs;
    }

    @ParameterizedTest
    @MethodSource("cnfConfigurations")
    @LongRunningTag
    public void compareFullBackbonesOnLargeFormulas(final CNFConfig cnfConfig, final MiniSatConfig.CNFMethod cnfMethod) throws IOException, ParserException {
        final String baseDir = "src/test/resources/formulas/";
        final List<String> fileNames = Arrays.asList("formula1.txt", "formula2.txt", "formula3.txt", "large_formula.txt", "small_formulas.txt");
        for (final String fileName : fileNames) {
            final String filePath = baseDir + fileName;
            final Backbone backboneReference = computeBackbone(filePath, CNFConfig.builder().build(), MiniSatConfig.builder().build().getCnfMethod());
            final Backbone backbone = computeBackbone(filePath, cnfConfig, cnfMethod);
            assertThat(backboneReference).isEqualTo(backbone);
        }
    }

    @Test
    @LongRunningTag
    public void compareBackbonesForVariablesOnLargeFormulas() throws IOException, ParserException {
        compareBackbonePerVariable("src/test/resources/formulas/formula1.txt");
        compareBackbonePerVariable("src/test/resources/formulas/large_formula.txt");
        compareBackbonePerVariable("src/test/resources/formulas/small_formulas.txt");
    }

    private Backbone computeBackbone(final String fileName, final CNFConfig cnfConfig, final MiniSatConfig.CNFMethod cnfMethod) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(cnfConfig);
        final Formula formula = FormulaReader.readPseudoBooleanFormula(fileName, f);
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(cnfMethod).build());
        solver.add(formula);
        return solver.backbone(formula.variables());
    }

    private void compareBackbonePerVariable(final String fileName) throws IOException, ParserException {
        final Map<Variable, Backbone> backboneFactory = computeBackbonePerVariable(fileName,
                CNFConfig.builder().algorithm(CNFConfig.Algorithm.ADVANCED).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.TSEITIN).build(),
                MiniSatConfig.CNFMethod.FACTORY_CNF);
        final Map<Variable, Backbone> backbonePg = computeBackbonePerVariable(fileName, CNFConfig.builder().build(), MiniSatConfig.CNFMethod.PG_ON_SOLVER);
        final Map<Variable, Backbone> backboneFullPg = computeBackbonePerVariable(fileName, CNFConfig.builder().build(), MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER);
        assertThat(backboneFactory).isEqualTo(backbonePg);
        assertThat(backboneFactory).isEqualTo(backboneFullPg);
    }

    private Map<Variable, Backbone> computeBackbonePerVariable(final String fileName, final CNFConfig cnfConfig, final MiniSatConfig.CNFMethod cnfMethod)
            throws IOException, ParserException {
        final long start = System.currentTimeMillis();
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(cnfConfig);
        final Formula formula = FormulaReader.readPseudoBooleanFormula(fileName, f);
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(cnfMethod).build());
        solver.add(formula);
        final SolverState solverState = solver.saveState();
        final Map<Variable, Backbone> result = new TreeMap<>();
        int counter = 1000;
        for (final Variable variable : formula.variables()) {
            if (counter-- > 0) {
                solver.add(variable);
                if (solver.sat() == Tristate.TRUE) {
                    final Backbone backbone = solver.backbone(formula.variables());
                    result.put(variable, backbone);
                }
                solver.loadState(solverState);
            }
        }
        final long stop = System.currentTimeMillis();
        System.out.println(fileName + " " + cnfConfig.algorithm + " " + cnfConfig.fallbackAlgorithmForAdvancedEncoding + " " + cnfMethod + ": " + (stop - start) + " ms.");
        return result;
    }

}
