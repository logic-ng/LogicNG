// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.cnf;

import static org.logicng.formulas.cache.TransformationCacheEntry.TSEITIN;
import static org.logicng.formulas.cache.TransformationCacheEntry.TSEITIN_VARIABLE;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.And;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;

/**
 * Transformation of a formula into CNF due to Tseitin.  Results in this implementation will always be cached.
 * <p>
 * ATTENTION: if you mix formulas from different formula factories this can lead to clashes in the naming of newly
 * introduced variables.
 * @version 2.0.0
 * @since 1.0
 */
public final class TseitinTransformation implements FormulaTransformation {

    private final int boundaryForFactorization;
    private final CNFFactorization factorization = new CNFFactorization();

    /**
     * Constructor for a Tseitin transformation.
     * @param boundaryForFactorization the boundary of number of atoms up to which classical factorization is used
     */
    public TseitinTransformation(final int boundaryForFactorization) {
        this.boundaryForFactorization = boundaryForFactorization;
    }

    /**
     * Constructor for a Tseitin transformation with a factorization bound of 12.
     */
    public TseitinTransformation() {
        this.boundaryForFactorization = 12;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Formula f = formula.nnf();
        if (f.isCNF()) {
            return f;
        }
        Formula tseitin = f.transformationCacheEntry(TSEITIN);
        if (tseitin != null) {
            final Assignment topLevel = new Assignment((Literal) f.transformationCacheEntry(TSEITIN_VARIABLE));
            return f.transformationCacheEntry(TSEITIN).restrict(topLevel);
        }
        if (f.numberOfAtoms() < this.boundaryForFactorization) {
            tseitin = f.transform(this.factorization);
        } else {
            for (final Formula formula1 : f.apply(f.factory().subformulaFunction())) {
                computeTseitin(formula1);
            }
            final Assignment topLevel = new Assignment((Literal) f.transformationCacheEntry(TSEITIN_VARIABLE));
            tseitin = f.transformationCacheEntry(TSEITIN).restrict(topLevel);
        }
        if (cache) {
            formula.setTransformationCacheEntry(TSEITIN_VARIABLE, f.transformationCacheEntry(TSEITIN_VARIABLE));
        }
        return tseitin;
    }

    /**
     * Computes the Tseitin transformation for a given formula and stores it in the formula cache.
     * @param formula the formula
     */
    private void computeTseitin(final Formula formula) {
        if (formula.transformationCacheEntry(TSEITIN) != null) {
            return;
        }
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case LITERAL:
                formula.setTransformationCacheEntry(TSEITIN, formula);
                formula.setTransformationCacheEntry(TSEITIN_VARIABLE, formula);
                break;
            case AND:
            case OR:
                final boolean isConjunction = formula instanceof And;
                final Literal tsLiteral = f.newCNFVariable();
                final List<Formula> nops = new ArrayList<>();
                final List<Formula> operands = new ArrayList<>(formula.numberOfOperands());
                final List<Formula> negOperands = new ArrayList<>(formula.numberOfOperands());
                if (isConjunction) {
                    negOperands.add(tsLiteral);
                    handleNary(formula, nops, operands, negOperands);
                    for (final Formula operand : operands) {
                        nops.add(f.or(tsLiteral.negate(), operand));
                    }
                    nops.add(f.or(negOperands));
                } else {
                    operands.add(tsLiteral.negate());
                    handleNary(formula, nops, operands, negOperands);
                    for (final Formula operand : negOperands) {
                        nops.add(f.or(tsLiteral, operand));
                    }
                    nops.add(f.or(operands));
                }
                formula.setTransformationCacheEntry(TSEITIN_VARIABLE, tsLiteral);
                formula.setTransformationCacheEntry(TSEITIN, f.and(nops));
                break;
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private void handleNary(final Formula formula, final List<Formula> nops, final List<Formula> operands, final List<Formula> negOperands) {
        for (final Formula op : formula) {
            if (op.type() != FType.LITERAL) {
                computeTseitin(op);
                nops.add(op.transformationCacheEntry(TSEITIN));
            }
            operands.add(op.transformationCacheEntry(TSEITIN_VARIABLE));
            negOperands.add(op.transformationCacheEntry(TSEITIN_VARIABLE).negate());
        }
    }

    @Override
    public String toString() {
        return String.format("TseitinTransformation{boundary=%d}", this.boundaryForFactorization);
    }
}
