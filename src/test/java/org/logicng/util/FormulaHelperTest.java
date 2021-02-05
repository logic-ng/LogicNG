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

package org.logicng.util;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

/**
 * Unit tests for {@link FormulaHelper}.
 * @version 2.0.0
 * @since 1.5.1
 */
public class FormulaHelperTest extends TestWithExampleFormulas {

    @Test
    public void testVariables() {
        assertThat(FormulaHelper.variables(this.TRUE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(this.FALSE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(this.A)).isEqualTo(new TreeSet<>(Collections.singletonList(this.A)));
        assertThat(FormulaHelper.variables(this.NA)).isEqualTo(new TreeSet<>(Collections.singletonList(this.A)));
        assertThat(FormulaHelper.variables(this.IMP1, this.IMP2, this.IMP3)).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y)));
        assertThat(FormulaHelper.variables(this.IMP1, this.Y)).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.Y)));

        assertThat(FormulaHelper.variables(Arrays.asList(this.TRUE, this.FALSE))).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(Arrays.asList(this.IMP1, this.IMP2, this.IMP3))).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y)));
        assertThat(FormulaHelper.variables(Arrays.asList(this.IMP1, this.Y))).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.Y)));
    }

    @Test
    public void testLiterals() {
        assertThat(FormulaHelper.literals(this.TRUE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(this.FALSE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(this.A)).isEqualTo(new TreeSet<>(Collections.singletonList(this.A)));
        assertThat(FormulaHelper.literals(this.NA)).isEqualTo(new TreeSet<>(Collections.singletonList(this.NA)));
        assertThat(FormulaHelper.literals(this.IMP1, this.IMP2, this.IMP3)).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y, this.NA, this.NB)));
        assertThat(FormulaHelper.literals(this.IMP1, this.NY)).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.NY)));

        assertThat(FormulaHelper.literals(Arrays.asList(this.TRUE, this.FALSE))).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(Arrays.asList(this.IMP1, this.IMP2, this.IMP3))).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y, this.NA,
                this.NB)));
        assertThat(FormulaHelper.literals(Arrays.asList(this.IMP1, this.NY))).isEqualTo(new TreeSet<>(Arrays.asList(this.A, this.B, this.NY)));
    }

    @Test
    public void testNegateLiterals() {
        assertThat((ArrayList<Literal>) FormulaHelper.negateLiterals(Collections.emptyList(), ArrayList::new))
                .isEqualTo(new ArrayList<Formula>());
        assertThat((ArrayList<Literal>) FormulaHelper.negateLiterals(Arrays.asList(this.A, this.NB), ArrayList::new))
                .isEqualTo(Arrays.asList(this.NA, this.B));
        assertThat((HashSet<Literal>) FormulaHelper.negateLiterals(Arrays.asList(this.A, this.NB), HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(this.NA, this.B)));
        final List<Variable> variables = Arrays.asList(this.A, this.B);
        assertThat((HashSet<Literal>) FormulaHelper.negateLiterals(variables, HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(this.NA, this.NB)));
    }

    @Test
    public void testNegate() {
        assertThat((ArrayList<Formula>) FormulaHelper.negate(Collections.emptyList(), ArrayList::new))
                .isEqualTo(new ArrayList<Formula>());
        assertThat((ArrayList<Formula>) FormulaHelper.negate(Arrays.asList(this.A, this.TRUE, this.NB, this.AND1), ArrayList::new))
                .isEqualTo(Arrays.asList(this.NA, this.FALSE, this.B, this.f.not(this.AND1)));
        assertThat((HashSet<Formula>) FormulaHelper.negate(Arrays.asList(this.A, this.TRUE, this.NB, this.AND1), HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(this.NA, this.FALSE, this.B, this.f.not(this.AND1))));
        final List<Variable> variables = Arrays.asList(this.A, this.B);
        assertThat((HashSet<Formula>) FormulaHelper.negate(variables, HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(this.NA, this.NB)));
        final List<Literal> literals = Arrays.asList(this.NA, this.B);
        assertThat((HashSet<Formula>) FormulaHelper.negate(literals, HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(this.A, this.NB)));
    }

    @Test
    public void testSplitTopLevelAnd() {
        assertThat(FormulaHelper.splitTopLevelAnd(this.TRUE)).isEqualTo(Collections.singletonList(this.TRUE));
        assertThat(FormulaHelper.splitTopLevelAnd(this.FALSE)).isEqualTo(Collections.singletonList(this.FALSE));
        assertThat(FormulaHelper.splitTopLevelAnd(this.OR1)).isEqualTo(Collections.singletonList(this.OR1));
        assertThat(FormulaHelper.splitTopLevelAnd(this.IMP1)).isEqualTo(Collections.singletonList(this.IMP1));

        assertThat(FormulaHelper.splitTopLevelAnd(this.AND1)).isEqualTo(Arrays.asList(this.A, this.B));
        assertThat(FormulaHelper.splitTopLevelAnd(this.AND3)).isEqualTo(Arrays.asList(this.OR1, this.OR2));
    }
}
