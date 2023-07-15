// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
