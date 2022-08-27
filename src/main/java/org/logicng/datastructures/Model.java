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

package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * A simple class representing a model of a formula.  In contrast to an {@link Assignment} a model just
 * stores a simple list of literals and cannot be used to evaluate or restrict a formula (because this would
 * be very inefficient).  In this case you want to use the {@link #assignment(boolean)} method to convert
 * the model to an assignment first.
 * <p>
 * The primary use case for models is to use them in the model enumeration function for minimal heap usage of
 * the enumerated models.
 * <p>
 * For efficiency reasons, two models are only equal, if their literal list has the same order.  During a model
 * enumeration this is always true.
 * @version 2.4.0
 * @since 2.4.0
 */
public class Model {
    private final List<Literal> literals;

    /**
     * Constructs a new model with a given list of literals.
     * @param literals the literals
     */
    public Model(final List<Literal> literals) {
        this.literals = literals;
    }

    /**
     * Constructs a new model with a given list of literals.
     * @param literals the literals
     */
    public Model(final Literal... literals) {
        this(Arrays.asList(literals));
    }

    /**
     * Returns the list of literals of this model.
     * @return the list of literals
     */
    public List<Literal> getLiterals() {
        return this.literals;
    }

    /**
     * Returns the size of this model.
     * @return the size of this model
     */
    public long size() {
        return this.literals.size();
    }

    /**
     * Converts this model to an assignment.
     * @param fastEvaluable whether the assignment should be fast evaluable or not
     * @return the assignment
     */
    public Assignment assignment(final boolean fastEvaluable) {
        return new Assignment(this.literals, fastEvaluable);
    }

    /**
     * Returns the formula for this model.  This is a conjunction of all literals.
     * @param f the formula factory
     * @return the conjunction of all literals in this model
     */
    public Formula formula(final FormulaFactory f) {
        return f.and(this.literals);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final Model model = (Model) o;
        return Objects.equals(this.literals, model.literals);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.literals);
    }

    @Override
    public String toString() {
        return "Model{" +
                "literals=" + this.literals +
                '}';
    }
}

