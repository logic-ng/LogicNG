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

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;

/**
 * A generator for pigeon hole formulas.
 * @version 1.0
 * @since 1.0
 */
public class PigeonHoleGenerator {

    private final FormulaFactory f;

    public PigeonHoleGenerator(final FormulaFactory f) {
        this.f = f;
    }

    public Formula generate(final int n) {
        return generate(n, "v");
    }

    public Formula generate(final int n, final String prefix) {
        return this.f.and(placeInSomeHole(n, prefix), onlyOnePigeonInHole(n, prefix));
    }

    private Formula placeInSomeHole(final int n, final String prefix) {
        if (n == 1) {
            return this.f.and(this.f.variable(prefix + "1"), this.f.variable(prefix + "2"));
        }
        final List<Formula> ors = new ArrayList<>();
        for (int i = 1; i <= n + 1; i++) {
            final List<Literal> orOps = new ArrayList<>();
            for (int j = 1; j <= n; j++) {
                orOps.add(this.f.variable(prefix + (n * (i - 1) + j)));
            }
            ors.add(this.f.or(orOps));
        }
        return this.f.and(ors);
    }

    private Formula onlyOnePigeonInHole(final int n, final String prefix) {
        if (n == 1) {
            return this.f.or(this.f.literal(prefix + "1", false), this.f.literal(prefix + "2", false));
        }
        final List<Formula> ors = new ArrayList<>();
        for (int j = 1; j <= n; j++) {
            for (int i = 1; i <= n; i++) {
                for (int k = i + 1; k <= n + 1; k++) {
                    ors.add(this.f.or(this.f.literal(prefix + (n * (i - 1) + j), false), this.f.literal(prefix + (n * (k - 1) + j), false)));
                }
            }
        }
        return this.f.and(ors);
    }
}
