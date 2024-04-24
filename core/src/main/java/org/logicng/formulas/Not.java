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

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

/**
 * Boolean negation.
 * @version 2.2.0
 * @since 1.0
 */
public final class Not extends Formula {

    private final Formula operand;
    private volatile int hashCode;

    /**
     * Constructor.
     * @param operand the operand of the negation
     * @param f       the factory which created this instance
     */
    Not(final Formula operand, final FormulaFactory f) {
        super(FType.NOT, f);
        this.operand = operand;
        this.hashCode = 0;
    }

    /**
     * Returns the operand of this negation.
     * @return the operand of this negation
     */
    public Formula operand() {
        return this.operand;
    }

    @Override
    public int numberOfOperands() {
        return 1;
    }

    @Override
    public boolean isConstantFormula() {
        return false;
    }

    @Override
    public boolean isAtomicFormula() {
        return false;
    }

    @Override
    public boolean containsVariable(final Variable variable) {
        return this.operand.containsVariable(variable);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return !this.operand.evaluate(assignment);
    }

    @Override
    public Formula restrict(final Assignment assignment) {
        return this.f.not(this.operand.restrict(assignment));
    }

    @Override
    public boolean containsNode(final Formula formula) {
        return this == formula || this.equals(formula) || this.operand.containsNode(formula);
    }

    @Override
    public Formula substitute(final Substitution substitution) {
        return this.f.not(this.operand.substitute(substitution));
    }

    @Override
    public Formula negate() {
        return this.operand;
    }

    @Override
    public int hashCode() {
        if (this.hashCode == 0) {
            this.hashCode = 29 * this.operand.hashCode();
        }
        return this.hashCode;
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a == object
        }
        if (other instanceof Not) {
            final Not otherNot = (Not) other;
            return this.operand.equals(otherNot.operand);
        }
        return false;
    }

    @Override
    public Iterator<Formula> iterator() {
        return new Iterator<Formula>() {
            private boolean iterated;

            @Override
            public boolean hasNext() {
                return !this.iterated;
            }

            @Override
            public Formula next() {
                if (!this.iterated) {
                    this.iterated = true;
                    return Not.this.operand;
                }
                throw new NoSuchElementException();
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    @Override
    public Stream<Formula> stream() {
        return Stream.of(this.operand);
    }
}
