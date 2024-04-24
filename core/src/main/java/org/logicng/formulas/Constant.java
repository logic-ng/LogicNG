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
 * Super class for Boolean constants.
 * @version 2.2.0
 * @since 1.0
 */
public abstract class Constant extends Formula {

    private static final Iterator<Formula> ITERATOR = new Iterator<Formula>() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public Formula next() {
            throw new NoSuchElementException();
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    };

    /**
     * Constructor.
     * @param type    the constant type
     * @param factory the factory which created this instance
     */
    Constant(final FType type, final FormulaFactory factory) {
        super(type, factory);
    }

    @Override
    public int numberOfOperands() {
        return 0;
    }

    @Override
    public boolean isConstantFormula() {
        return true;
    }

    @Override
    public boolean isAtomicFormula() {
        return true;
    }

    @Override
    public boolean containsVariable(final Variable variable) {
        return false;
    }

    @Override
    public Formula restrict(final Assignment assignment) {
        return this;
    }

    @Override
    public boolean containsNode(final Formula formula) {
        return this == formula;
    }

    @Override
    public Formula substitute(final Substitution substitution) {
        return this;
    }

    @Override
    public Iterator<Formula> iterator() {
        return ITERATOR;
    }

    @Override
    public Stream<Formula> stream() {
        return Stream.empty();
    }
}
