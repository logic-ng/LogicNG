package org.logicng.solvers.functions;

import static org.logicng.datastructures.Tristate.TRUE;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

/**
 * A solver function for getting the current formula on the solver.
 * @version 2.0.0
 * @since 2.0.0
 */
public class FormulaOnSolverFunction implements SolverFunction<Set<Formula>> {

    @Override
    public Set<Formula> apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        final Set<Formula> formulas = new LinkedHashSet<>();
        for (final MSClause clause : solver.underlyingSolver().clauses()) {
            final List<Literal> lits = new ArrayList<>();
            for (int i = 0; i < clause.size(); i++) {
                final int litInt = clause.get(i);
                lits.add(solver.factory().literal(solver.underlyingSolver().nameForIdx(litInt >> 1), (litInt & 1) != 1));
            }
            if (!clause.isAtMost()) {
                formulas.add(solver.factory().clause(lits));
            } else {
                final int rhs = clause.size() + 1 - clause.atMostWatchers();
                final List<Variable> vars = new ArrayList<>();
                for (final Literal lit : lits) {
                    vars.add(lit.variable());
                }
                formulas.add(solver.factory().cc(CType.LE, rhs, vars));
            }
        }
        final LNGVector<MSVariable> variables = solver.underlyingSolver().variables();
        for (int i = 0; i < variables.size(); i++) {
            final MSVariable var = variables.get(i);
            if (var.level() == 0) {
                formulas.add(solver.factory().literal(solver.underlyingSolver().nameForIdx(i), var.assignment() == TRUE));
            }
        }
        return formulas;
    }
}
