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

package org.logicng.transformations.simplification;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;
import static org.logicng.handlers.OptimizationHandler.satHandler;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneGeneration;
import org.logicng.backbones.BackboneType;
import org.logicng.configurations.ConfigurationType;
import org.logicng.datastructures.Assignment;
import org.logicng.explanations.smus.SmusComputation;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.primecomputation.PrimeCompiler;
import org.logicng.primecomputation.PrimeResult;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * An advanced simplifier for formulas.
 * <p>
 * The aim of the simplification is to minimize the formula with respect to a given rating function,
 * e.g. finding a formula with a minimal number of symbols when represented as string.
 * <p>
 * The simplification performs the following steps:
 * <ul>
 *     <li>Restricting the formula to its backbone</li>
 *     <li>Computation of all prime implicants</li>
 *     <li>Finding a minimal coverage (by finding a smallest MUS)</li>
 *     <li>Building a DNF from the minimal prime implicant coverage</li>
 *     <li>Factoring out: Applying the Distributive Law heuristically for a smaller formula</li>
 *     <li>Minimizing negations: Applying De Morgan's Law heuristically for a smaller formula</li>
 * </ul>
 * The first and the last two steps can be configured using the {@link AdvancedSimplifierConfig}. Also, the handler and the rating
 * function can be configured. If no rating function is specified, the {@link DefaultRatingFunction} is chosen.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class AdvancedSimplifier implements FormulaTransformation {

    private final AdvancedSimplifierConfig initConfig;

    /**
     * Constructs a new simplifier with the given rating functions.
     * @param ratingFunction the rating function
     * @deprecated this constructor is no longer acceptable, use the constructor with an {@link AdvancedSimplifierConfig} or with no parameters (for the
     * default advanced simplifier configuration) instead.
     */
    @Deprecated
    public AdvancedSimplifier(final RatingFunction<?> ratingFunction) {
        this(ratingFunction, null);
    }

    /**
     * Constructs a new simplifier with the given handler and rating functions.
     * <p>
     * The simplifier can be called with an {@link OptimizationHandler}. The given handler instance will be used for every subsequent
     * {@link org.logicng.solvers.functions.OptimizationFunction} call and the handler's SAT handler is used for every subsequent SAT call.
     * @param ratingFunction the rating function
     * @param handler        the handler, can be {@code null}
     * @deprecated this constructor is no longer acceptable, use the constructor with an {@link AdvancedSimplifierConfig} or with no parameters (for the
     * default advanced simplifier configuration) instead.
     */
    @Deprecated
    public AdvancedSimplifier(final RatingFunction<?> ratingFunction, final OptimizationHandler handler) {
        this.initConfig = AdvancedSimplifierConfig.builder().ratingFunction(ratingFunction).handler(handler).build();
    }

    /**
     * Constructs a new simplifier with the advanced simplifier configuration from the formula factory.
     */
    public AdvancedSimplifier() {
        this.initConfig = null;
    }

    /**
     * Constructs a new simplifier with the given configuration.
     * @param config The configuration for the advanced simplifier, including a handler, a rating function and flags for which steps should pe performed
     *               during the computation.
     */
    public AdvancedSimplifier(final AdvancedSimplifierConfig config) {
        this.initConfig = config;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final AdvancedSimplifierConfig config = this.initConfig != null
                ? this.initConfig
                : (AdvancedSimplifierConfig) formula.factory().configurationFor(ConfigurationType.ADVANCED_SIMPLIFIER);
        start(config.handler);
        final FormulaFactory f = formula.factory();
        Formula simplified = formula;
        final SortedSet<Literal> backboneLiterals = new TreeSet<>();
        if (config.restrictBackbone) {
            final Backbone backbone = BackboneGeneration
                    .compute(Collections.singletonList(formula), formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE, satHandler(config.handler));
            if (backbone == null || aborted(config.handler)) {
                return null;
            }
            if (!backbone.isSat()) {
                return f.falsum();
            }
            backboneLiterals.addAll(backbone.getCompleteBackbone());
            simplified = formula.restrict(new Assignment(backboneLiterals));
        }
        final Formula simplifyMinDnf = computeMinDnf(f, simplified, config);
        if (simplifyMinDnf == null) {
            return null;
        }
        simplified = simplifyWithRating(simplified, simplifyMinDnf, config);
        if (config.factorOut) {
            final Formula factoredOut = simplified.transform(new FactorOutSimplifier(config.ratingFunction));
            simplified = simplifyWithRating(simplified, factoredOut, config);
        }
        if (config.restrictBackbone) {
            simplified = f.and(f.and(backboneLiterals), simplified);
        }
        if (config.simplifyNegations) {
            final Formula negationSimplified = simplified.transform(NegationSimplifier.get());
            simplified = simplifyWithRating(simplified, negationSimplified, config);
        }
        return simplified;
    }

    private Formula computeMinDnf(final FormulaFactory f, Formula simplified, final AdvancedSimplifierConfig config) {
        final PrimeResult primeResult =
                PrimeCompiler.getWithMinimization().compute(simplified, PrimeResult.CoverageType.IMPLICANTS_COMPLETE, config.handler);
        if (primeResult == null || aborted(config.handler)) {
            return null;
        }
        final List<SortedSet<Literal>> primeImplicants = primeResult.getPrimeImplicants();
        final List<Formula> minimizedPIs = SmusComputation.computeSmusForFormulas(negateAllLiterals(primeImplicants, f),
                Collections.singletonList(simplified), f, config.handler);
        if (minimizedPIs == null || aborted(config.handler)) {
            return null;
        }
        simplified = f.or(negateAllLiteralsInFormulas(minimizedPIs, f).stream().map(f::and).collect(Collectors.toList()));
        return simplified;
    }

    private List<Formula> negateAllLiterals(final Collection<SortedSet<Literal>> literalSets, final FormulaFactory f) {
        final List<Formula> result = new ArrayList<>();
        for (final SortedSet<Literal> literals : literalSets) {
            result.add(f.or(FormulaHelper.negateLiterals(literals, ArrayList::new)));
        }
        return result;
    }

    private List<Formula> negateAllLiteralsInFormulas(final Collection<Formula> formulas, final FormulaFactory f) {
        final List<Formula> result = new ArrayList<>();
        for (final Formula formula : formulas) {
            result.add(f.and(FormulaHelper.negateLiterals(formula.literals(), ArrayList::new)));
        }
        return result;
    }

    private Formula simplifyWithRating(final Formula formula, final Formula simplifiedOneStep, final AdvancedSimplifierConfig config) {
        final Number ratingSimplified = config.ratingFunction.apply(simplifiedOneStep, true);
        final Number ratingFormula = config.ratingFunction.apply(formula, true);
        return ratingSimplified.intValue() < ratingFormula.intValue() ? simplifiedOneStep : formula;
    }
}
