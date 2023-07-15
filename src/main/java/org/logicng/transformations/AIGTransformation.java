// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import static org.logicng.formulas.cache.TransformationCacheEntry.AIG;

import org.logicng.formulas.And;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Not;
import org.logicng.formulas.Or;
import org.logicng.formulas.cache.PredicateCacheEntry;

import java.util.LinkedHashSet;

/**
 * And-inverter-graph (AIG) transformation.  Returns the AIG of the given formula.
 * @version 2.3.0
 * @since 1.0
 */
public final class AIGTransformation implements FormulaTransformation {

    private FormulaFactory f;
    private boolean cache;
    private static final AIGTransformation INSTANCE = new AIGTransformation();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public AIGTransformation() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static AIGTransformation get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        this.f = formula.factory();
        this.cache = cache;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return formula;
            case NOT:
                return transformNot((Not) formula);
            case IMPL:
                return transformImplication((Implication) formula);
            case EQUIV:
                return transformEquivalence((Equivalence) formula);
            case AND:
                return transformAnd((And) formula);
            case OR:
                return transformOr((Or) formula);
            case PBC:
                return apply(formula.cnf(), cache);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private Formula transformNot(final Not not) {
        Formula aig = not.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = this.f.not(apply(not.operand(), this.cache));
            if (this.cache) {
                not.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformImplication(final Implication impl) {
        Formula aig = impl.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = this.f.not(this.f.and(apply(impl.left(), this.cache), this.f.not(apply(impl.right(), this.cache))));
            if (this.cache) {
                impl.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformEquivalence(final Equivalence equiv) {
        Formula aig = equiv.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = this.f.and(this.f.not(this.f.and(apply(equiv.left(), this.cache), this.f.not(apply(equiv.right(), this.cache)))),
                    this.f.not(this.f.and(this.f.not(equiv.left()), equiv.right())));
            if (this.cache) {
                equiv.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformAnd(final And and) {
        Formula aig = and.transformationCacheEntry(AIG);
        if (aig == null) {
            final LinkedHashSet<Formula> nops = new LinkedHashSet<>(and.numberOfOperands());
            for (final Formula op : and) {
                nops.add(apply(op, this.cache));
            }
            aig = this.f.and(nops);
            if (this.cache) {
                and.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformOr(final Or or) {
        Formula aig = or.transformationCacheEntry(AIG);
        if (aig == null) {
            final LinkedHashSet<Formula> nops = new LinkedHashSet<>(or.numberOfOperands());
            for (final Formula op : or) {
                nops.add(this.f.not(apply(op, this.cache)));
            }
            aig = this.f.not(this.f.and(nops));
            if (this.cache) {
                or.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
