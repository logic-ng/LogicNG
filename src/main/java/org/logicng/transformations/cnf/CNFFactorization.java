///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.transformations.cnf;

import static org.logicng.formulas.FType.AND;
import static org.logicng.formulas.FType.LITERAL;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.handlers.FactorizationHandler;

import java.util.Iterator;
import java.util.LinkedHashSet;

/**
 * Transformation of a formula in CNF by factorization.
 * @version 1.1
 * @since 1.0
 */
public final class CNFFactorization implements FormulaTransformation {

    private final FactorizationHandler handler;
    private boolean proceed;

    /**
     * Constructor for a CNF Factorization without a factorization handler.
     */
    public CNFFactorization() {
        this.proceed = true;
        this.handler = null;
    }

    /**
     * Constructor for a CNF Factorization with a given factorization handler.
     * @param handler the handler
     */
    public CNFFactorization(final FactorizationHandler handler) {
        this.proceed = true;
        this.handler = handler;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (this.handler != null) {
            this.handler.started();
        }
        this.proceed = true;
        return applyRec(formula, cache);
    }

    private Formula applyRec(final Formula formula, final boolean cache) {
        if (!this.proceed) {
            return null;
        }
        if (formula.type().precedence() >= LITERAL.precedence()) {
            return formula;
        }
        Formula cached = formula.transformationCacheEntry(FACTORIZED_CNF);
        if (cached != null) {
            return cached;
        }
        switch (formula.type()) {
            case NOT:
            case IMPL:
            case EQUIV:
                cached = this.applyRec(formula.nnf(), cache);
                break;
            case OR:
                LinkedHashSet<Formula> nops = new LinkedHashSet<>();
                for (final Formula op : formula) {
                    if (!this.proceed) {
                        return null;
                    }
                    nops.add(this.applyRec(op, cache));
                }
                final Iterator<Formula> it = nops.iterator();
                cached = it.next();
                while (it.hasNext()) {
                    if (!this.proceed) {
                        return null;
                    }
                    cached = this.distribute(cached, it.next());
                }
                break;
            case AND:
                nops = new LinkedHashSet<>();
                for (final Formula op : formula) {
                    final Formula apply = this.applyRec(op, cache);
                    if (!this.proceed) {
                        return null;
                    }
                    nops.add(apply);
                }
                cached = formula.factory().and(nops);
                break;
            case PBC:
                cached = formula.nnf();
                break;
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
        if (this.proceed) {
            if (cache) {
                formula.setTransformationCacheEntry(FACTORIZED_CNF, cached);
            }
            return cached;
        }
        return null;
    }

    /**
     * Computes the distribution (factorization) of two formulas.
     * @param f1 the first formula
     * @param f2 the second formula
     * @return the distribution of the two formulas
     */
    private Formula distribute(final Formula f1, final Formula f2) {
        if (this.handler != null) {
            this.proceed = this.handler.performedDistribution();
        }
        if (this.proceed) {
            final FormulaFactory f = f1.factory();
            if (f1.type() == AND || f2.type() == AND) {
                final LinkedHashSet<Formula> nops = new LinkedHashSet<>();
                for (final Formula op : f1.type() == AND ? f1 : f2) {
                    final Formula distribute = this.distribute(op, f1.type() == AND ? f2 : f1);
                    if (!this.proceed) {
                        return null;
                    }
                    nops.add(distribute);
                }
                return f.and(nops);
            }
            final Formula clause = f.or(f1, f2);
            if (this.handler != null) {
                this.proceed = this.handler.createdClause(clause);
            }
            return clause;
        }
        return null;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
