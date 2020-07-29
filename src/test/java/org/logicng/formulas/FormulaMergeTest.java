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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

public class FormulaMergeTest {

    @Test
    public void testPanic() {
        final FormulaFactory f1 = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.PANIC).build());
        final FormulaFactory f2 = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());

        final Variable a1 = f1.variable("A");
        final Variable b1 = f1.variable("B");
        final Variable c1 = f1.variable("C");
        final Variable a2 = f2.variable("A");
        final Variable b2 = f2.variable("B");
        final Variable c2 = f2.variable("C");
        assertThatThrownBy(() -> f1.not(a2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.and(a2, b1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.and(a2, b2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.and(a1, b1, c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.or(a2, b1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.or(a2, b2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.or(a1, b1, c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.clause(a2, b1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.clause(a2, b2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.clause(a1, b1, c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.cnf(f1.clause(a1, b1), f1.clause(a1, c1), c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.cnf(f1.clause(a1, b1), f2.clause(a2, c2), c1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.pbc(CType.GE, 1, new Literal[]{a1, b2.negate(), c1}, new int[]{1, 2, 3})).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.pbc(CType.GE, 1, new Literal[]{a2, b2, c2.negate()}, new int[]{1, 2, 3})).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.cc(CType.GE, 1, a1, b2, c1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.cc(CType.GE, 1, a2, b2, c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.amo(a1, b2, c1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.amo(a2, b2, c2)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.exo(a1, b2, c1)).isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> f1.exo(a2, b2, c2)).isInstanceOf(UnsupportedOperationException.class);
    }

    @Test
    public void testMerge() {
        final FormulaFactory f1 = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final FormulaFactory f2 = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.PANIC).build());

        final Variable a1 = f1.variable("A");
        final Variable b1 = f1.variable("B");
        final Variable c1 = f1.variable("C");
        final Variable a2 = f2.variable("A");
        final Variable b2 = f2.variable("B");
        final Variable c2 = f2.variable("C");
        assertThat(f1.not(a2)).isNotNull();
        assertThat(f1.and(a2, b1)).isNotNull();
        assertThat(f1.and(a2, b2)).isNotNull();
        assertThat(f1.and(a1, b1, c2)).isNotNull();
        assertThat(f1.or(a2, b1)).isNotNull();
        assertThat(f1.or(a2, b2)).isNotNull();
        assertThat(f1.or(a1, b1, c2)).isNotNull();
        assertThat(f1.clause(a2, b1)).isNotNull();
        assertThat(f1.clause(a2, b2)).isNotNull();
        assertThat(f1.clause(a1, b1, c2)).isNotNull();
        assertThat(f1.cnf(f1.clause(a1, b1), f1.clause(a1, c1), c2)).isNotNull();
        assertThat(f1.cnf(f1.clause(a1, b1), f2.clause(a2, c2), c1)).isNotNull();
        assertThat(f1.pbc(CType.GE, 1, new Literal[]{a1, b2.negate(), c1}, new int[]{1, 2, 3})).isNotNull();
        assertThat(f1.pbc(CType.GE, 1, new Literal[]{a2, b2, c2.negate()}, new int[]{1, 2, 3})).isNotNull();
        assertThat(f1.cc(CType.GE, 1, a1, b2, c1)).isNotNull();
        assertThat(f1.cc(CType.GE, 1, a2, b2, c2)).isNotNull();
        assertThat(f1.amo(a1, b2, c1)).isNotNull();
        assertThat(f1.amo(a2, b2, c2)).isNotNull();
        assertThat(f1.exo(a1, b2, c1)).isNotNull();
        assertThat(f1.exo(a2, b2, c2)).isNotNull();
    }
}
