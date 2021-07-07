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

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

/**
 * Unit Tests for the class {@link AdvancedSimplifier}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class SimplifierTest extends TestWithExampleFormulas {

    private final AdvancedSimplifier simplifier = new AdvancedSimplifier(new DefaultRatingFunction());

    @Test
    public void testConstants() {
        assertThat(this.f.falsum().transform(this.simplifier)).isEqualTo(this.f.falsum());
        assertThat(this.f.verum().transform(this.simplifier)).isEqualTo(this.f.verum());
    }

    @Test
    public void testCornerCases() {
        final FormulaCornerCases cornerCases = new FormulaCornerCases(this.f);
        cornerCases.cornerCases().forEach(this::computeAndVerify);
    }

    @Test
    @RandomTag
    public void testRandomized() {
        for (int i = 0; i < 100; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(8).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(5);
            computeAndVerify(formula);
        }
    }

    private void computeAndVerify(final Formula formula) {
        final Formula simplified = formula.transform(this.simplifier);
        assertThat(formula.factory().equivalence(formula, simplified).holds(new TautologyPredicate(this.f)))
                .as("Minimized formula is equivalent to original Formula")
                .isTrue();
    }
}
