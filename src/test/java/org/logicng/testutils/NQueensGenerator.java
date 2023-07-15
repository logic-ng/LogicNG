// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
