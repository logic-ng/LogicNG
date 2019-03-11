package org.logicng.transformations.dnf;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.predicates.DNFPredicate;
import org.logicng.transformations.Subsumption;

import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;

/**
 * This transformation performs subsumption on a given DNF and returns a new DNF.
 * I.e. performs as many subsumptions as possible.  A subsumption in a DNF means,
 * that e.g. a minterm {@code A & B & C} is subsumed by another clause {@code A & B}
 * and can therefore be deleted for an equivalent CNF.
 * @version 1.5.0
 * @since 1.5.0
 */
public class DNFSubsumption extends Subsumption implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (!formula.holds(new DNFPredicate())) {
            throw new IllegalArgumentException("DNF subsumption can only be applied to formulas in DNF");
        }
        if (formula.type().precedence() >= FType.LITERAL.precedence() || formula.type() == FType.OR) {
            return formula;
        }
        assert formula.type() == FType.OR;
        final UBTree<Literal> ubTree = generateSubsumedUBTree(formula);
        final List<Formula> minterms = new LinkedList<>();
        for (final SortedSet<Literal> literals : ubTree.allSets()) {
            minterms.add(formula.factory().clause(literals));
        }
        return formula.factory().or(minterms);
    }
}
