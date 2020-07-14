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

package org.logicng.formulas.printer;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A sorted string representation for formulas.
 * <p>
 * A variable ordering is given as a list of variables. The variables in the formula will be sorted in the same order
 * as they appear in the list.
 * <p>
 * Let f1 and f2 be two sub-formulas of a formula to be sorted. It is iteratively checked whether the variables of the
 * given ordering appear in either of the two sub-formulas. We distinguish the following cases for a currently
 * considered variable v:
 * - If v is in f1 and f2, then continue with the next variable.
 * - If v is in f1 but not f2, then f1 is ordered before f2.
 * - If v is in f2 but not f1, then f2 is ordered before f1.
 * - If all variables of the ordering have been in both f1 and f2, the two formulas can be ordered arbitrarily.
 * <p>
 * Example 1:
 * Given the variable ordering [a, b, c, d], the sorted string representation for a simple conjunction b &amp; d &amp; ~a &amp; ~c
 * would be ~a &amp; b &amp; ~c &amp; d.
 * <p>
 * It is important to note that the first variable that appear in only one of the compared sub-formulas decides their
 * ordering. Hence, apart from the deciding variable, the other variables of the sub-formulas might suggest a different
 * order. The user is urged to keep this in mind and an exemplary situation is therefore illustrated in the following
 * example.
 * <p>
 * Example 2:
 * Given the variable ordering [a, b, c, d, e, f], the sorted string representation for the formula
 * b | c | d &lt;=&gt; a | e | f would be a | e | f &lt;=&gt; b | c | d.
 * <p>
 * Furthermore, the fact that implications cannot be ordered should also be kept in mind.
 * <p>
 * Example 3:
 * Given the variable ordering [a, b], the sorted string representation for the formula b =&gt; a stays b =&gt; a.
 * <p>
 * Finally, the user should be aware that any variables of a formula that do not appear in the given ordering will be
 * sorted after the variables that do appear in the ordering.
 * <p>
 * Example 4:
 * Given the variable ordering [b, c, d], the sorted string representation for the formula a &amp; (c | (d =&gt; b)) would be
 * ((d =&gt; b) | c) &amp; a.
 * @version 2.0.0
 * @since 1.5.0
 */
public final class SortedStringRepresentation extends DefaultStringRepresentation {

    /**
     * A list of variables in the order they are supposed to appear in a given formula.
     */
    private final List<Variable> varOrder;
    private final FormulaComparator comparator;

    /**
     * Constructs a new sorted string representation with a given ordering of variables.
     * @param varOrder the given variable ordering
     */
    public SortedStringRepresentation(final List<Variable> varOrder) {
        this.varOrder = varOrder;
        this.comparator = new FormulaComparator(varOrder);
    }

    /**
     * Returns the sorted string representation of the given formula.
     * @param formula the formula
     * @return the sorted string representation of the formula with regard to the variable ordering
     */
    @Override
    public String toInnerString(final Formula formula) {
        switch (formula.type()) {
            case FALSE:
                return falsum();
            case TRUE:
                return verum();
            case LITERAL:
                final Literal lit = (Literal) formula;
                return lit.phase() ? lit.name() : negation() + lit.name();
            case NOT:
                final Not not = (Not) formula;
                return negation() + bracket(not.operand());
            case IMPL:
                return binaryOperator((BinaryOperator) formula, implication());
            case EQUIV:
                return sortedEquivalence((Equivalence) formula);
            case AND:
            case OR:
                final NAryOperator nary = (NAryOperator) formula;
                final String op = formula.type() == FType.AND ? and() : or();
                return naryOperator(nary, String.format("%s", op));
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                return String.format("%s%s%d", pbLhs(pbc.operands(), pbc.coefficients()), pbComparator(pbc.comparator()), pbc.rhs());
            default:
                throw new IllegalArgumentException("Cannot print the unknown formula type " + formula.type());
        }
    }

    /**
     * Returns the sorted string representation of an n-ary operator.
     * @param operator the n-ary operator
     * @param opString the operator string
     * @return the string representation
     */
    @Override
    protected String naryOperator(final NAryOperator operator, final String opString) {
        final List<Formula> operands = new ArrayList<>();
        for (final Formula op : operator) {
            operands.add(op);
        }
        final int size = operator.numberOfOperands();
        operands.sort(this.comparator);
        final StringBuilder sb = new StringBuilder();
        int count = 0;
        Formula last = null;
        for (final Formula op : operands) {
            if (++count == size) {
                last = op;
            } else {
                sb.append(operator.type().precedence() < op.type().precedence() ? toInnerString(op) : bracket(op));
                sb.append(opString);
            }
        }
        if (last != null) {
            sb.append(operator.type().precedence() < last.type().precedence() ? toInnerString(last) : bracket(last));
        }
        return sb.toString();
    }

    /**
     * Returns the sorted string representation of the left-hand side of a pseudo-Boolean constraint.
     * @param operands     the literals of the constraint
     * @param coefficients the coefficients of the constraint
     * @return the sorted string representation
     */
    @Override
    protected String pbLhs(final Literal[] operands, final int[] coefficients) {
        assert operands.length == coefficients.length;
        final List<Literal> sortedOperands = new ArrayList<>();
        final List<Integer> sortedCoefficients = new ArrayList<>();
        final List<Literal> givenOperands = Arrays.asList(operands);
        for (final Variable v : this.varOrder) {
            final int index = givenOperands.indexOf(v);
            if (index != -1) {
                sortedOperands.add(v);
                sortedCoefficients.add(coefficients[index]);
            }
        }
        for (final Literal givenOperand : givenOperands) {
            if (!sortedOperands.contains(givenOperand)) {
                sortedOperands.add(givenOperand);
                sortedCoefficients.add(coefficients[givenOperands.indexOf(givenOperand)]);
            }
        }
        final StringBuilder sb = new StringBuilder();
        final String mul = pbMul();
        final String add = pbAdd();
        for (int i = 0; i < sortedOperands.size() - 1; i++) {
            if (sortedCoefficients.get(i) != 1) {
                sb.append(sortedCoefficients.get(i)).append(mul).append(sortedOperands.get(i)).append(add);
            } else {
                sb.append(sortedOperands.get(i)).append(add);
            }
        }
        if (sortedOperands.size() > 0) {
            if (sortedCoefficients.get(sortedOperands.size() - 1) != 1) {
                sb.append(sortedCoefficients.get(sortedOperands.size() - 1)).append(mul).append(sortedOperands.get(sortedOperands.size() - 1));
            } else {
                sb.append(sortedOperands.get(sortedOperands.size() - 1));
            }
        }
        return sb.toString();
    }

    /**
     * Returns the string representation of an equivalence.
     * @param equivalence the equivalence
     * @return the string representation
     */
    private String sortedEquivalence(final Equivalence equivalence) {
        final Formula right;
        final Formula left;
        if (this.comparator.compare(equivalence.left(), equivalence.right()) <= 0) {
            right = equivalence.right();
            left = equivalence.left();
        } else {
            right = equivalence.left();
            left = equivalence.right();
        }
        final String leftString = FType.EQUIV.precedence() < left.type().precedence() ? toInnerString(left) : bracket(left);
        final String rightString = FType.EQUIV.precedence() < right.type().precedence() ? toInnerString(right) : bracket(right);
        return String.format("%s%s%s", leftString, equivalence(), rightString);
    }

    static class FormulaComparator implements Comparator<Formula> {

        final List<Variable> varOrder;

        FormulaComparator(final List<Variable> varOrder) {
            this.varOrder = varOrder;
        }

        /**
         * Compares two given formulas considering the variable ordering of this class.
         * @param formula1 the first formula
         * @param formula2 the second formula
         * @return -1 iff formula1 &lt; formula2 (when for the first time a variable of the ordering appears in formula1 but not formula2)
         * 0 iff formula1 = formula2 (when all variables of the ordering appear (or not appear) in both formula1 and formula2)
         * 1 iff formula1 &gt; formula2 (when for the first time a variable of the ordering appears in formula2 but not formula1)
         */
        @Override
        public int compare(final Formula formula1, final Formula formula2) {
            final SortedSet<Variable> vars1 = new TreeSet<>(formula1.variables());
            final SortedSet<Variable> vars2 = new TreeSet<>(formula2.variables());
            for (final Variable v : this.varOrder) {
                if (vars1.isEmpty() && vars2.isEmpty()) {
                    break;
                } else if (vars1.isEmpty() || (vars1.contains(v) && !vars2.contains(v))) {
                    return -1;
                } else if (vars2.isEmpty() || (!vars1.contains(v) && vars2.contains(v))) {
                    return 1;
                } else if (vars1.contains(v) && vars2.contains(v)) {
                    vars1.remove(v);
                    vars2.remove(v);
                }
            }
            return (int) formula1.numberOfAtoms() - (int) formula2.numberOfAtoms();
        }
    }
}
