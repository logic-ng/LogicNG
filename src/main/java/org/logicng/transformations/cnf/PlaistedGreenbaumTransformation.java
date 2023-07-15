// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.cnf;

import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_POS;
import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_VARIABLE;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;

/**
 * Transformation of a formula into CNF due to Plaisted &amp; Greenbaum.  Results in this implementation will always be
 * cached.
 * <p>
 * ATTENTION: if you mix formulas from different formula factories this can lead to clashes in the naming of newly
 * introduced variables.
 * @version 2.0.0
 * @since 1.0
 */
public final class PlaistedGreenbaumTransformation implements FormulaTransformation {

    private final int boundaryForFactorization;
    private final CNFFactorization factorization = new CNFFactorization();

    /**
     * Constructor for a Plaisted &amp; Greenbaum transformation.
     * @param boundaryForFactorization the boundary of number of atoms up to which classical factorization is used
     */
    public PlaistedGreenbaumTransformation(final int boundaryForFactorization) {
        this.boundaryForFactorization = boundaryForFactorization;
    }

    /**
     * Constructor for a Plaisted &amp; Greenbaum transformation with conversion to nnf and a factorization
     * bound of 12.
     */
    public PlaistedGreenbaumTransformation() {
        this(12);
    }

    /**
     * Returns the auxiliary variable for a given formula.  Either the formula is already a variable, has already an
     * auxiliary variable or a new one is generated.
     * @param formula the formula
     * @return the old or new auxiliary variable
     */
    private static Literal pgVariable(final Formula formula) {
        if (formula.type() == FType.LITERAL) {
            return (Literal) formula;
        }
        Literal var = (Literal) formula.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE);
        if (var == null) {
            var = formula.factory().newCNFVariable();
            formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE, var);
        }
        return var;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Formula nnf = formula.nnf();
        if (nnf.isCNF()) {
            return nnf;
        }
        Formula pg;
        if (nnf.numberOfAtoms() < this.boundaryForFactorization) {
            pg = nnf.transform(this.factorization);
        } else {
            pg = this.computeTransformation(nnf);
            final Assignment topLevel = new Assignment((Literal) nnf.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
            pg = pg.restrict(topLevel);
        }
        if (cache) {
            formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE,
                    nnf.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
        }
        return pg;
    }

    private Formula computeTransformation(final Formula formula) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case LITERAL:
                return f.verum();
            case OR:
            case AND:
                final List<Formula> nops = new ArrayList<>();
                nops.add(this.computePosPolarity(formula));
                for (final Formula op : formula) {
                    nops.add(this.computeTransformation(op));
                }
                return f.and(nops);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private Formula computePosPolarity(final Formula formula) {
        Formula result = formula.transformationCacheEntry(PLAISTED_GREENBAUM_POS);
        if (result != null) {
            return result;
        }
        final FormulaFactory f = formula.factory();
        final Literal pgVar = pgVariable(formula);
        switch (formula.type()) {
            case AND: {
                final List<Formula> nops = new ArrayList<>();
                for (final Formula op : formula) {
                    nops.add(f.clause(pgVar.negate(), pgVariable(op)));
                }
                result = f.and(nops);
                formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_POS, result);
                return result;
            }
            case OR: {
                final List<Literal> nops = new ArrayList<>();
                nops.add(pgVar.negate());
                for (final Formula op : formula) {
                    nops.add(pgVariable(op));
                }
                result = f.clause(nops);
                formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_POS, result);
                return result;
            }
            default:
                throw new IllegalArgumentException("Unknown or unexpected formula type. Expected AND or OR formula type only.");
        }
    }

    @Override
    public String toString() {
        return String.format("PlaistedGreenbaumTransformation{boundary=%d}", this.boundaryForFactorization);
    }
}
