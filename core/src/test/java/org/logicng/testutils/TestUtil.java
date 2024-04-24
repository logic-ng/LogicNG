package org.logicng.testutils;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Help methods for unit testing.
 * @version 2.3.0
 * @since 2.3.0
 */
public final class TestUtil {

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private TestUtil() {
        // Intentionally left empty
    }

    /**
     * Tests if the two given formulas have the same models when projected to the given set of variables.
     * @param f1   first formula
     * @param f2   second formula
     * @param vars the set of variables to which the models should be projected
     * @return {@code true} if the two formulas have the same models when projected to the given set of variables, otherwise {@code false}
     */
    public static boolean equivalentModels(final Formula f1, final Formula f2, final SortedSet<Variable> vars) {
        final SATSolver s = MiniSat.miniSat(f1.factory());
        s.add(f1);
        final List<Assignment> models1 = s.enumerateAllModels(vars);
        s.reset();
        s.add(f2);
        final List<Assignment> models2 = s.enumerateAllModels(vars);
        if (models1.size() != models2.size()) {
            return false;
        }
        for (final Assignment model : models1) {
            if (!models2.contains(model)) {
                return false;
            }
        }
        return true;
    }
}
