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

import org.logicng.datastructures.Substitution;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

/**
 * Super class for Boolean binary operators.
 * @version 2.2.0
 * @since 1.0
 */
public abstract class BinaryOperator extends Formula {

    protected final Formula left;
    protected final Formula right;
    protected volatile int hashCode;

    /**
     * Constructor.
     * @param type  the type of the formula
     * @param left  the left-hand side operand
     * @param right the right-hand side operand
     * @param f     the factory which created this instance
     */
    BinaryOperator(final FType type, final Formula left, final Formula right, final FormulaFactory f) {
        super(type, f);
        this.left = left;
        this.right = right;
        this.hashCode = 0;
    }

    /**
     * Returns the left-hand side operator.
     * @return the left-hand side operator
     */
    public Formula left() {
        return this.left;
    }

    /**
     * Returns the right-hand side operator.
     * @return the right-hand side operator
     */
    public Formula right() {
        return this.right;
    }

    @Override
    public int numberOfOperands() {
        return 2;
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
        return this.left.containsVariable(variable) || this.right.containsVariable(variable);
    }

    @Override
    public boolean containsNode(final Formula formula) {
        return this == formula || this.equals(formula) || this.left.containsNode(formula) || this.right.containsNode(formula);
    }

    @Override
    public Formula substitute(final Substitution substitution) {
        return this.f.binaryOperator(this.type, this.left.substitute(substitution), this.right.substitute(substitution));
    }

    @Override
    public Formula negate() {
        return this.f.not(this);
    }

    @Override
    public Iterator<Formula> iterator() {
        return new Iterator<Formula>() {
            private int count;

            @Override
            public boolean hasNext() {
                return this.count < 2;
            }

            @Override
            public Formula next() {
                if (this.count == 0) {
                    this.count++;
                    return BinaryOperator.this.left;
                } else if (this.count == 1) {
                    this.count++;
                    return BinaryOperator.this.right;
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
        return Stream.of(this.left, this.right);
    }
}
