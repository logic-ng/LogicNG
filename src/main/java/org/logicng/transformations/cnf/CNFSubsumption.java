package org.logicng.transformations.cnf;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.predicates.CNFPredicate;
import org.logicng.transformations.Subsumption;

import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;

/**
 * This transformation performs subsumption on a given CNF and returns a new CNF.
 * I.e. performs as many subsumptions as possible.  A subsumption in a CNF means,
 * that e.g. a clause {@code A | B | C} is subsumed by another clause {@code A | B}
 * and can therefore be deleted for an equivalent CNF.
 * @version 1.5.0
 * @since 1.5.0
 */
public class CNFSubsumption extends Subsumption implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (!formula.holds(new CNFPredicate())) {
            throw new IllegalArgumentException("CNF subsumption can only be applied to formulas in CNF");
        }
        if (formula.type().precedence() >= FType.LITERAL.precedence() || formula.type() == FType.OR) {
            return formula;
        }
        assert formula.type() == FType.AND;
        final UBTree<Literal> ubTree = generateSubsumedUBTree(formula);
        final List<Formula> clauses = new LinkedList<>();
        for (final SortedSet<Literal> literals : ubTree.allSets()) {
            clauses.add(formula.factory().clause(literals));
        }
        return formula.factory().cnf(clauses);
    }
}
