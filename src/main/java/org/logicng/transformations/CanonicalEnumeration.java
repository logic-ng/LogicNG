// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * Superclass for canonical normal form enumeration (CNF or DNF) via enumeration
 * of the falsifying/satisfying assignments.
 * @version 2.3.0
 * @since 2.3.0
 */
public abstract class CanonicalEnumeration {

    /**
     * Constructs the canonical CNF/DNF of the given formula by enumerating the
     * falsifying/satisfying assignments.
     * @param formula the formula
     * @param cnf     {@code true} if the canonical CNF should be computed,
     *                {@code false} if the canonical DNF should be computed
     * @return the canonical normal form (CNF or DNF) of the formula
     */
    protected static Formula compute(final Formula formula, final boolean cnf) {
        final FormulaFactory f = formula.factory();
        final SATSolver solver = MiniSat.miniSat(f);
        solver.add(cnf ? formula.negate() : formula);
        final List<Assignment> enumeration = solver.execute(ModelEnumerationFunction.builder().build());
        if (enumeration.isEmpty()) {
            return f.constant(cnf);
        }
        final List<Formula> ops = new ArrayList<>();
        for (final Assignment a : enumeration) {
            final SortedSet<Literal> literals = a.literals();
            final Formula term = cnf ? f.or(FormulaHelper.negate(literals, ArrayList::new)) : f.and(a.literals());
            ops.add(term);
        }
        return cnf ? f.and(ops) : f.or(ops);
    }
}
