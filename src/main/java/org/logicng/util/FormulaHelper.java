package org.logicng.util;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

public class FormulaHelper {

    private FormulaHelper() {
        // Intentionally left empty
    }

    public static SortedSet<Variable> variables(Formula... formulas) {
        SortedSet<Variable> variables = new TreeSet<>();
        for (Formula f : formulas) {
            variables.addAll(f.variables());
        }
        return variables;
    }

    public static SortedSet<Variable> variables(Collection<? extends Formula> formulas) {
        SortedSet<Variable> variables = new TreeSet<>();
        for (Formula f : formulas) {
            variables.addAll(f.variables());
        }
        return variables;
    }

    public static SortedSet<Literal> literals(Formula... formulas) {
        SortedSet<Literal> literals = new TreeSet<>();
        for (Formula f : formulas) {
            literals.addAll(f.literals());
        }
        return literals;
    }

    public static SortedSet<Literal> literals(Collection<? extends Formula> formulas) {
        SortedSet<Literal> literals = new TreeSet<>();
        for (Formula f : formulas) {
            literals.addAll(f.literals());
        }
        return literals;
    }
}
