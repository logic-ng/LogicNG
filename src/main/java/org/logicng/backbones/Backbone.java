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

package org.logicng.backbones;

import static java.util.Collections.emptySortedSet;
import static java.util.Collections.unmodifiableSortedSet;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collections;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * This class represents a backbone.  A backbone of a formula is a set of literals (positive and/or negative) which
 * are present in their respective polarity in every model of the given formula.  Therefore the literals must be set
 * accordingly in order for the formula to evaluate to true.
 * <p>
 * A backbone of a formula has up to three sets of variables
 * <ol>
 * <li>Positive backbone variables: Variables that occur positive in each model of the formula
 * <li>Negative backbone variables: Variables that occur negative in each model of the formula
 * <li>Optional variables: Variables that are neither in the positive nor in the negative backbone.
 * Therefore these variables can be assigned to true or false.
 * </ol>
 * All variable sets which were not computed are empty.
 * @version 2.0.0
 * @since 1.5.0
 */
public final class Backbone {
    private final boolean sat;
    private final SortedSet<Variable> positiveBackbone;
    private final SortedSet<Variable> negativeBackbone;
    private final SortedSet<Variable> optionalVariables;

    /**
     * Constructs a new backbone that contains the given backbone variables and the given optional variables.
     * @param sat               is the original formula satisfiable or not
     * @param positiveBackbone  positive backbone variables
     * @param negativeBackbone  negative backbone variables
     * @param optionalVariables optional variables
     */
    private Backbone(final boolean sat, final SortedSet<Variable> positiveBackbone, final SortedSet<Variable> negativeBackbone,
                     final SortedSet<Variable> optionalVariables) {
        this.sat = sat;
        this.positiveBackbone = positiveBackbone == null ? emptySortedSet() : positiveBackbone;
        this.negativeBackbone = negativeBackbone == null ? emptySortedSet() : negativeBackbone;
        this.optionalVariables = optionalVariables == null ? emptySortedSet() : optionalVariables;
    }

    /**
     * Returns a new backbone for a satisfiable formula.
     * @param positiveBackbone  positive backbone variables
     * @param negativeBackbone  negative backbone variables
     * @param optionalVariables optional variables
     * @return the backbone
     */
    public static Backbone satBackbone(final SortedSet<Variable> positiveBackbone, final SortedSet<Variable> negativeBackbone,
                                       final SortedSet<Variable> optionalVariables) {
        return new Backbone(true, positiveBackbone, negativeBackbone, optionalVariables);
    }

    /**
     * Returns a new empty backbone for an unsatisfiable formula.
     * @return the backbone
     */
    public static Backbone unsatBackbone() {
        return new Backbone(false, null, null, null);
    }

    /**
     * Returns whether the original formula of this backbone was satisfiable or not.
     * @return whether the original formula of this backbone was satisfiable or not
     */
    public boolean isSat() {
        return this.sat;
    }

    /**
     * Returns the positive variables of the backbone.
     * @return the set of positive backbone variables
     */
    public SortedSet<Variable> getPositiveBackbone() {
        return unmodifiableSortedSet(this.positiveBackbone);
    }

    /**
     * Returns the negative variables of the backbone.
     * @return the set of negative backbone variables
     */
    public SortedSet<Variable> getNegativeBackbone() {
        return unmodifiableSortedSet(this.negativeBackbone);
    }

    /**
     * Returns the variables of the formula that are optional, i.e. not in the positive or negative backbone.
     * @return the set of non-backbone variables
     */
    public SortedSet<Variable> getOptionalVariables() {
        return unmodifiableSortedSet(this.optionalVariables);
    }

    /**
     * Returns all literals of the backbone.  Positive backbone variables have positive polarity, negative
     * backbone variables have negative polarity.
     * @return the set of both positive and negative backbone variables as literals
     */
    public SortedSet<Literal> getCompleteBackbone() {
        final SortedSet<Literal> completeBackbone = new TreeSet<Literal>(this.positiveBackbone);
        for (final Variable var : this.negativeBackbone) {
            completeBackbone.add(var.negate());
        }
        return Collections.unmodifiableSortedSet(completeBackbone);
    }

    /**
     * Returns the positive and negative backbone as a conjunction of literals.
     * @param f the formula factory needed for construction the backbone formula.
     * @return the backbone formula
     */
    public Formula toFormula(final FormulaFactory f) {
        return this.sat ? f.and(this.getCompleteBackbone()) : f.falsum();
    }

    /**
     * Returns the backbone as map from variables to tri-states. A positive variable is mapped to {@code Tristate.TRUE},
     * a negative variable to {@code Tristate.FALSE} and the optional variables to {@code Tristate.UNDEF}.
     * @return the mapping of the backbone
     */
    public SortedMap<Variable, Tristate> toMap() {
        final SortedMap<Variable, Tristate> map = new TreeMap<>();
        for (final Variable var : this.positiveBackbone) {
            map.put(var, Tristate.TRUE);
        }
        for (final Variable var : this.negativeBackbone) {
            map.put(var, Tristate.FALSE);
        }
        for (final Variable var : this.optionalVariables) {
            map.put(var, Tristate.UNDEF);
        }
        return Collections.unmodifiableSortedMap(map);
    }

    @Override
    public boolean equals(final Object other) {
        if (other == null) {
            return false;
        }
        if (this == other) {
            return true;
        }
        if (getClass() != other.getClass()) {
            return false;
        }
        final Backbone backbone = (Backbone) other;
        return Objects.equals(this.positiveBackbone, backbone.positiveBackbone) &&
                Objects.equals(this.negativeBackbone, backbone.negativeBackbone) &&
                Objects.equals(this.optionalVariables, backbone.optionalVariables);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.positiveBackbone, this.negativeBackbone, this.optionalVariables);
    }

    @Override
    public String toString() {
        return "Backbone{" +
                "positiveBackbone=" + this.positiveBackbone +
                ", negativeBackbone=" + this.negativeBackbone +
                ", optionalVariables=" + this.optionalVariables +
                '}';
    }
}
