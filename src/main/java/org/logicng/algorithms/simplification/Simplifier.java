package org.logicng.algorithms.simplification;

import org.logicng.algorithms.SmusComputation;
import org.logicng.algorithms.primecomputation.PrimeCompiler;
import org.logicng.algorithms.primecomputation.PrimeResult;
import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneGeneration;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.stream.Collectors;

/**
 * A simplifier for formulas.
 *
 * The aim of the simplification is to minimize the formula with respect to a given rating function,
 * e.g. finding a formula with a minimal number of symbols when represented as string.
 *
 * The simplification performs the following steps:
 * <ul>
 *     <li>Computation of all prime implicants</li>
 *     <li>Finding a minimal coverage (by finding a smallest MUS)</li>
 *     <li>Building a DNF from the minimal prime implicant coverage</li>
 *     <li>Factoring out: Applying the Distributive Law heuristically for a smaller formula</li>
 *     <li>Minimizing negations: Applying De Morgan's Law heuristically for a smaller formula</li>
 * </ul>
 * @version 2.0.0
 * @since 2.0.0
 */
public class Simplifier implements FormulaTransformation {
    // TODO move to minimization package?

    private final RatingFunction<?> ratingFunction;

    /**
     * Constructs a new simplifier with the given rating functions.
     * @param ratingFunction the rating function
     */
    public Simplifier(final RatingFunction<?> ratingFunction) {
        this.ratingFunction = ratingFunction;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final FormulaFactory f = formula.factory();
        final Backbone backbone = BackboneGeneration.compute(formula, formula.variables());
        if (!backbone.isSat()) {
            return f.falsum();
        }
        final SortedSet<Literal> backboneLiterals = backbone.getCompleteBackbone();
        final Formula restrictedFormula = formula.restrict(new Assignment(backboneLiterals));
        final List<SortedSet<Literal>> primeImplicants = PrimeCompiler.getWithMinimization()
                .compute(restrictedFormula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE).getPrimeImplicants();
        final List<Formula> minimizedPIs = SmusComputation.computeSmusForFormulas(negateAllLiterals(primeImplicants, f), Collections.singletonList(restrictedFormula), f);
        assert minimizedPIs != null : "The conjunction of a satisfiable formula and its negated prime implications is always a contradiction";
        final Formula minDnf = f.or(negateAllLiteralsInFormulas(minimizedPIs, f).stream().map(f::and).collect(Collectors.toList()));
        final Formula fullFactor = minDnf.transform(new FactorOutSimplification(this.ratingFunction));
        return f.and(f.and(backboneLiterals), fullFactor).transform(new NegationMinimizer());
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
}
