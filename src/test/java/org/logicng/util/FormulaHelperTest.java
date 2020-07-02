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
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link FormulaHelper}.
 * @version 2.0.0
 * @since 1.5.1
 */
public class FormulaHelperTest {

    @Test
    public void testVariables() {
        assertThat(FormulaHelper.variables(F.TRUE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(F.FALSE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(F.A)).isEqualTo(new TreeSet<>(Collections.singletonList(F.A)));
        assertThat(FormulaHelper.variables(F.NA)).isEqualTo(new TreeSet<>(Collections.singletonList(F.A)));
        assertThat(FormulaHelper.variables(F.IMP1, F.IMP2, F.IMP3)).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y)));
        assertThat(FormulaHelper.variables(F.IMP1, F.Y)).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.Y)));

        assertThat(FormulaHelper.variables(Arrays.asList(F.TRUE, F.FALSE))).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.variables(Arrays.asList(F.IMP1, F.IMP2, F.IMP3))).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y)));
        assertThat(FormulaHelper.variables(Arrays.asList(F.IMP1, F.Y))).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.Y)));
    }

    @Test
    public void testLiterals() {
        assertThat(FormulaHelper.literals(F.TRUE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(F.FALSE)).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(F.A)).isEqualTo(new TreeSet<>(Collections.singletonList(F.A)));
        assertThat(FormulaHelper.literals(F.NA)).isEqualTo(new TreeSet<>(Collections.singletonList(F.NA)));
        assertThat(FormulaHelper.literals(F.IMP1, F.IMP2, F.IMP3)).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y, F.NA, F.NB)));
        assertThat(FormulaHelper.literals(F.IMP1, F.NY)).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.NY)));

        assertThat(FormulaHelper.literals(Arrays.asList(F.TRUE, F.FALSE))).isEqualTo(new TreeSet<>());
        assertThat(FormulaHelper.literals(Arrays.asList(F.IMP1, F.IMP2, F.IMP3))).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y, F.NA, F.NB)));
        assertThat(FormulaHelper.literals(Arrays.asList(F.IMP1, F.NY))).isEqualTo(new TreeSet<>(Arrays.asList(F.A, F.B, F.NY)));
    }

    @Test
    public void testNegateLiterals() {
        assertThat((ArrayList<Literal>) FormulaHelper.negateLiterals(Collections.emptyList(), ArrayList::new))
                .isEqualTo(new ArrayList<Formula>());
        assertThat((ArrayList<Literal>) FormulaHelper.negateLiterals(Arrays.asList(F.A, F.NB), ArrayList::new))
                .isEqualTo(Arrays.asList(F.NA, F.B));
        assertThat((HashSet<Literal>) FormulaHelper.negateLiterals(Arrays.asList(F.A, F.NB), HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(F.NA, F.B)));
    }

    @Test
    public void testNegate() {
        assertThat((ArrayList<Formula>) FormulaHelper.negate(Collections.emptyList(), ArrayList::new))
                .isEqualTo(new ArrayList<Formula>());
        assertThat((ArrayList<Formula>) FormulaHelper.negate(Arrays.asList(F.A, F.TRUE, F.NB, F.AND1), ArrayList::new))
                .isEqualTo(Arrays.asList(F.NA, F.FALSE, F.B, F.f.not(F.AND1)));
        assertThat((HashSet<Formula>) FormulaHelper.negate(Arrays.asList(F.A, F.TRUE, F.NB, F.AND1), HashSet::new))
                .isEqualTo(new HashSet<>(Arrays.asList(F.NA, F.FALSE, F.B, F.f.not(F.AND1))));
    }

    @Test
    public void testSplitTopLevelAnd() {
        assertThat(FormulaHelper.splitTopLevelAnd(F.TRUE)).isEqualTo(Collections.singletonList(F.TRUE));
        assertThat(FormulaHelper.splitTopLevelAnd(F.FALSE)).isEqualTo(Collections.singletonList(F.FALSE));
        assertThat(FormulaHelper.splitTopLevelAnd(F.OR1)).isEqualTo(Collections.singletonList(F.OR1));
        assertThat(FormulaHelper.splitTopLevelAnd(F.IMP1)).isEqualTo(Collections.singletonList(F.IMP1));

        assertThat(FormulaHelper.splitTopLevelAnd(F.AND1)).isEqualTo(Arrays.asList(F.A, F.B));
        assertThat(FormulaHelper.splitTopLevelAnd(F.AND3)).isEqualTo(Arrays.asList(F.OR1, F.OR2));
    }
}
