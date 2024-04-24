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

package org.logicng.util;

import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Creation of formula corner cases.
 * <p>
 * Formula corner cases help to cover all cases when writing unit tests with formulas involved.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FormulaCornerCases {

    private final FormulaFactory f;
    private final Variable a;
    private final Variable b;
    private final Variable c;
    private final Literal na;
    private final Literal nb;
    private final Literal nc;

    /**
     * Constructs a new corner cases creator.
     * @param f the formula factory
     */
    public FormulaCornerCases(final FormulaFactory f) {
        this.f = f;
        this.a = f.variable("a");
        this.b = f.variable("b");
        this.c = f.variable("c");
        this.na = this.a.negate();
        this.nb = this.b.negate();
        this.nc = this.c.negate();
    }

    /**
     * Returns the set of variables used for creating the corner cases.
     * @return the set of variables
     */
    public SortedSet<Variable> getVariables() {
        return new TreeSet<>(Arrays.asList(this.a, this.b, this.c));
    }

    /**
     * Generates corner cases for all operators.
     * @return formulas representing corner cases for all operators
     */
    public List<Formula> cornerCases() {
        final List<Formula> formulas = new ArrayList<>();
        formulas.add(this.f.falsum());
        formulas.add(this.f.not(this.f.falsum()));
        formulas.add(this.f.verum());
        formulas.add(this.f.not(this.f.verum()));
        formulas.add(this.a);
        formulas.add(this.a.negate());
        formulas.add(this.f.not(this.a));
        formulas.add(this.f.not(this.f.not(this.a)));
        formulas.add(this.f.not(this.f.not(this.f.not(this.a))));
        formulas.addAll(binaryCornerCases(FType.IMPL, this.a, this.b));
        formulas.addAll(binaryCornerCases(FType.EQUIV, this.a, this.b));
        formulas.addAll(naryCornerCases(FType.OR, this.a, this.b, this.c));
        formulas.addAll(naryCornerCases(FType.AND, this.a, this.b, this.c));
        formulas.addAll(pbcCornerCases(this.a, this.b, this.c));
        return formulas;
    }

    private List<Formula> binaryCornerCases(final FType type, final Variable a, final Variable b) {
        final List<Formula> formulas = new ArrayList<>();
        formulas.add(this.f.binaryOperator(type, this.f.verum(), this.f.verum()));
        formulas.add(this.f.binaryOperator(type, this.f.falsum(), this.f.verum()));
        formulas.add(this.f.binaryOperator(type, this.f.verum(), this.f.falsum()));
        formulas.add(this.f.binaryOperator(type, this.f.falsum(), this.f.falsum()));

        formulas.add(this.f.binaryOperator(type, this.f.verum(), a));
        formulas.add(this.f.binaryOperator(type, a, this.f.verum()));
        formulas.add(this.f.binaryOperator(type, this.f.verum(), this.na));
        formulas.add(this.f.binaryOperator(type, this.na, this.f.verum()));

        formulas.add(this.f.binaryOperator(type, this.f.falsum(), a));
        formulas.add(this.f.binaryOperator(type, a, this.f.falsum()));
        formulas.add(this.f.binaryOperator(type, this.f.falsum(), this.na));
        formulas.add(this.f.binaryOperator(type, this.na, this.f.falsum()));

        formulas.add(this.f.binaryOperator(type, a, a));
        formulas.add(this.f.binaryOperator(type, a, this.na));
        formulas.add(this.f.binaryOperator(type, this.na, a));
        formulas.add(this.f.binaryOperator(type, this.na, this.na));

        formulas.add(this.f.binaryOperator(type, a, b));
        formulas.add(this.f.binaryOperator(type, a, this.nb));
        formulas.add(this.f.binaryOperator(type, this.na, b));
        formulas.add(this.f.binaryOperator(type, this.na, this.nb));
        return formulas;
    }

    private List<Formula> naryCornerCases(final FType type, final Variable a, final Variable b, final Variable c) {
        final List<Formula> formulas = new ArrayList<>();
        formulas.add(this.f.naryOperator(type, new Variable[0]));

        formulas.add(this.f.naryOperator(type, this.f.falsum()));
        formulas.add(this.f.naryOperator(type, this.f.verum()));
        formulas.add(this.f.naryOperator(type, this.f.falsum(), this.f.verum()));

        formulas.add(this.f.naryOperator(type, a));
        formulas.add(this.f.naryOperator(type, this.na));

        formulas.add(this.f.naryOperator(type, this.f.verum(), a));
        formulas.add(this.f.naryOperator(type, this.f.verum(), this.na));
        formulas.add(this.f.naryOperator(type, this.f.falsum(), this.na));
        formulas.add(this.f.naryOperator(type, this.f.falsum(), this.na));

        formulas.add(this.f.naryOperator(type, a, this.na));
        formulas.add(this.f.naryOperator(type, a, b));
        formulas.add(this.f.naryOperator(type, a, b, c));
        formulas.add(this.f.naryOperator(type, this.na, this.nb, this.nc));
        formulas.add(this.f.naryOperator(type, a, b, c, this.na));
        return formulas;
    }

    private List<Formula> pbcCornerCases(final Variable a, final Variable b, final Variable c) {
        final List<Formula> formulas = new ArrayList<>();
        for (final CType type : CType.values()) {
            formulas.addAll(pbcCornerCases(type, a, b, c));
        }
        return formulas;
    }

    private List<Formula> pbcCornerCases(final CType comparator, final Variable a, final Variable b, final Variable c) {
        final List<Formula> formulas = new ArrayList<>();
        formulas.addAll(pbcCornerCases(comparator, new Literal[0], new int[0], this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a}, new int[]{-1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a}, new int[]{0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a}, new int[]{1}, this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{this.na}, new int[]{-1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{this.na}, new int[]{0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{this.na}, new int[]{1}, this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b}, new int[]{-1, -1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b}, new int[]{0, 0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b}, new int[]{1, 1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b}, new int[]{1, -1}, this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.nb}, new int[]{-1, -1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.nb}, new int[]{0, 0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.nb}, new int[]{1, 1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.nb}, new int[]{1, -1}, this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.na}, new int[]{-1, -1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.na}, new int[]{0, 0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.na}, new int[]{1, 1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, this.na}, new int[]{1, -1}, this.f));

        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b, c}, new int[]{-1, -1, -1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b, c}, new int[]{0, 0, 0}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b, c}, new int[]{1, 1, 1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{a, b, c}, new int[]{-1, 1, -1}, this.f));
        formulas.addAll(pbcCornerCases(comparator, new Literal[]{this.na, this.nb, c}, new int[]{-1, 1, -1}, this.f));
        return formulas;
    }

    private List<Formula> pbcCornerCases(final CType comparator, final Literal[] literals, final int[] coefficients, final FormulaFactory f) {
        final List<Formula> formulas = new ArrayList<>();
        for (final Integer rhs : Arrays.asList(-1, 0, 1, -3, -4, 3, 4)) {
            formulas.add(f.pbc(comparator, rhs, literals, coefficients));
        }
        return formulas;
    }
}
