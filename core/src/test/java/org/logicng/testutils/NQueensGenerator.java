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

package org.logicng.testutils;

import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A generator for the n-queens problem.
 * @version 1.2
 * @since 1.2
 */
public class NQueensGenerator {

    private final FormulaFactory f;

    public NQueensGenerator(final FormulaFactory f) {
        this.f = f;
        this.f.putConfiguration(CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.PURE).build());
    }

    public Formula generate(final int n) {
        int kk = 1;
        final Variable[][] varNames = new Variable[n][];
        for (int i = 0; i < n; i++) {
            varNames[i] = new Variable[n];
            for (int j = 0; j < n; j++) {
                varNames[i][j] = this.f.variable("v" + kk++);
            }
        }

        final List<Formula> operands = new ArrayList<>();
        final List<Variable> vars = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            vars.addAll(Arrays.asList(varNames[i]).subList(0, n));
            operands.add(this.f.exo(vars).cnf());
            vars.clear();
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                vars.add(varNames[j][i]);
            }
            operands.add(this.f.exo(vars).cnf());
            vars.clear();
        }
        for (int i = 0; i < n - 1; i++) {
            for (int j = 0; j < n - i; j++) {
                vars.add(varNames[j][i + j]);
            }
            operands.add(this.f.amo(vars).cnf());
            vars.clear();
        }
        for (int i = 1; i < n - 1; i++) {
            for (int j = 0; j < n - i; j++) {
                vars.add(varNames[j + i][j]);
            }
            operands.add(this.f.amo(vars).cnf());
            vars.clear();
        }
        for (int i = 0; i < n - 1; i++) {
            for (int j = 0; j < n - i; j++) {
                vars.add(varNames[j][n - 1 - (i + j)]);
            }
            operands.add(this.f.amo(vars).cnf());
            vars.clear();
        }
        for (int i = 1; i < n - 1; i++) {
            for (int j = 0; j < n - i; j++) {
                vars.add(varNames[j + i][n - 1 - j]);
            }
            operands.add(this.f.amo(vars).cnf());
            vars.clear();
        }
        return this.f.and(operands);
    }
}
