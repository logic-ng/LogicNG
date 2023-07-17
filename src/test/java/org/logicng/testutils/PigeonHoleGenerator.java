// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
                    ors.add(this.f.or(this.f.literal(prefix + (n * (i - 1) + j), false),
                            this.f.literal(prefix + (n * (k - 1) + j), false)));
                }
            }
        }
        return this.f.and(ors);
    }
}
