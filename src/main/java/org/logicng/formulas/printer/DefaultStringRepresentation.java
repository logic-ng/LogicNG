// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas.printer;

import org.logicng.formulas.CType;

/**
 * The default string representation for formulas.
 * @version 1.0
 * @since 1.0
 */
public class DefaultStringRepresentation extends FormulaStringRepresentation {

    @Override
    protected String falsum() {
        return "$false";
    }

    @Override
    protected String verum() {
        return "$true";
    }

    @Override
    protected String negation() {
        return "~";
    }

    @Override
    protected String implication() {
        return " => ";
    }

    @Override
    protected String equivalence() {
        return " <=> ";
    }

    @Override
    protected String and() {
        return " & ";
    }

    @Override
    protected String or() {
        return " | ";
    }

    @Override
    protected String pbComparator(final CType comparator) {
        switch (comparator) {
            case EQ:
                return " = ";
            case LE:
                return " <= ";
            case LT:
                return " < ";
            case GE:
                return " >= ";
            case GT:
                return " > ";
            default:
                throw new IllegalArgumentException("Unknown pseudo-Boolean comparison: " + comparator);
        }
    }

    @Override
    protected String pbMul() {
        return "*";
    }

    @Override
    protected String pbAdd() {
        return " + ";
    }

    @Override
    protected String lbr() {
        return "(";
    }

    @Override
    protected String rbr() {
        return ")";
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
