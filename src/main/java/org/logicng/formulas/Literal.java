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
 * Boolean literals.
 * <p>
 * Literals are besides the constants true and false and pseudo Boolean constraints the
 * atomic formulas in LogicNG.  Each variable is a positive literal.
 * <p>
 * A literal consists of its name and its phase (also sign or polarity in the literature).
 * A new positive literal can be constructed with {@code f.literal("a", true)} or
 * - as a shortcut - {@code f.variable("a")}.  A new negative literal can be constructed
 * with {@code f.literal("a", false)} or if preferred with {@code f.not(f.variable("a"))}
 * or {@code f.variable("a").negate()}.
 * <p>
 * @version 2.2.0
 * @since 1.0
 */
public class Literal extends Formula implements Comparable<Literal> {

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

    private final String name;
    private final boolean phase;
    private final Variable var;
    private volatile Literal negated;
    private volatile int hashCode;

    /**
     * Constructor.  A literal always has a name and a phase.  A positive literal can also
     * be constructed directly as a {@link Variable}.
     * @param name  the literal name
     * @param phase the phase of the literal (also found as sign or polarity in the literature)
     * @param f     the factory which created this literal
     */
    Literal(final String name, final boolean phase, final FormulaFactory f) {
        super(FType.LITERAL, f);
        this.name = name;
        this.phase = phase;
        this.var = phase ? (Variable) this : (Variable) this.negate();
    }

    @Override
    public FormulaFactory factory() {
        return this.f;
    }

    @Override
    public int numberOfOperands() {
        return 0;
    }

    @Override
    public boolean isConstantFormula() {
        return false;
    }

    @Override
    public boolean isAtomicFormula() {
        return true;
    }

    @Override
    public boolean containsVariable(final Variable variable) {
        return variable.name().equals(this.name);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return assignment.evaluateLit(this);
    }

    @Override
    public Formula restrict(final Assignment assignment) {
        return assignment.restrictLit(this);
    }

    @Override
    public boolean containsNode(final Formula formula) {
        return this.equals(formula);
    }

    @Override
    public Formula substitute(final Substitution substitution) {
        final Formula subst = substitution.getSubstitution(this.variable());
        return subst == null ? this : (this.phase ? subst : subst.negate());
    }

    @Override
    public Literal negate() {
        if (this.negated != null) {
            return this.negated;
        }
        this.negated = this.f.literal(this.name, !this.phase);
        return this.negated;
    }

    /**
     * Returns the name of the literal.
     * @return the name of the literal
     */
    public String name() {
        return this.name;
    }

    /**
     * Returns the phase of the literal.
     * @return the phase of the literal.
     */
    public boolean phase() {
        return this.phase;
    }

    /**
     * Returns a positive version of this literal (aka a variable).
     * @return a positive version of this literal
     */
    public Variable variable() {
        return this.var;
    }

    @Override
    public int hashCode() {
        final int result = this.hashCode;
        if (result == 0) {
            this.hashCode = this.name.hashCode() ^ (this.phase ? 1 : 0);
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
        if (other instanceof Literal) {
            final Literal otherLit = (Literal) other;
            return this.phase == otherLit.phase && this.name.equals(otherLit.name);
        }
        return false;
    }

    @Override
    public int compareTo(final Literal lit) {
        if (this == lit) {
            return 0;
        }
        final int res = this.name.compareTo(lit.name);
        if (res == 0 && this.phase != lit.phase) {
            return this.phase ? -1 : 1;
        }
        return res;
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
