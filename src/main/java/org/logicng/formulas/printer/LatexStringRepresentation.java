// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas.printer;

import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The LaTeX string representation for formulas.
 * @version 1.1
 * @since 1.0
 */
public final class LatexStringRepresentation extends FormulaStringRepresentation {

    private static final Pattern pattern = Pattern.compile("(.*?)(\\d*)");

    /**
     * Returns the latex string for a variable name
     * @param name the name
     * @return the matching UTF8 symbol
     */
    private static String latexName(final String name) {
        final Matcher matcher = pattern.matcher(name);
        if (!matcher.matches()) {
            return name;
        }
        if (matcher.group(2).isEmpty()) {
            return matcher.group(1);
        }
        return matcher.group(1) + "_{" + matcher.group(2) + "}";
    }

    @Override
    public String toInnerString(final Formula formula) {
        if (formula.type() == FType.LITERAL) {
            final Literal lit = (Literal) formula;
            return lit.phase() ? latexName(lit.name()) : this.negation() + " " + latexName(lit.name());
        }
        return super.toInnerString(formula);
    }

    @Override
    protected String falsum() {
        return "\\bottom";
    }

    @Override
    protected String verum() {
        return "\\top";
    }

    @Override
    protected String negation() {
        return "\\lnot";
    }

    @Override
    protected String implication() {
        return " \\rightarrow ";
    }

    @Override
    protected String equivalence() {
        return " \\leftrightarrow ";
    }

    @Override
    protected String and() {
        return " \\land ";
    }

    @Override
    protected String or() {
        return " \\lor ";
    }

    @Override
    protected String pbComparator(final CType comparator) {
        switch (comparator) {
            case EQ:
                return " = ";
            case LE:
                return " \\leq ";
            case LT:
                return " < ";
            case GE:
                return " \\geq ";
            case GT:
                return " > ";
            default:
                throw new IllegalArgumentException("Unknown pseudo-Boolean comparison: " + comparator);
        }
    }

    @Override
    protected String pbMul() {
        return "\\cdot ";
    }

    @Override
    protected String pbAdd() {
        return " + ";
    }

    @Override
    protected String lbr() {
        return "\\left(";
    }

    @Override
    protected String rbr() {
        return "\\right)";
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
