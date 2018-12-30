package org.logicng.explanations.backbones;

import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A Backbone (the set of literals of a formula that need to be assigned true in order for the formula to be
 * satisfiable).
 * @version 1.5
 * @since 1.5
 */
public class Backbone {
    private SortedSet<Variable> positiveBackbone;
    private SortedSet<Variable> negativeBackbone;

    /**
     * Constructs a new Backbone.
     */
    public Backbone() {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
    }

    /**
     * Returns the positive literals of the Backbone.
     * @return the set of positive Backbone literals
     */
    public SortedSet<Variable> getPositiveBackbone() {
        return positiveBackbone;
    }

    /**
     * Returns the negative literals of the Backbone.
     * @return the set of negative Backbone literals
     */
    public SortedSet<Variable> getNegativeBackbone() {
        return negativeBackbone;
    }

    /**
     * Returns the all literals of the Backbone.
     * @return the set of both positive and negative Backbone literals
     */
    public SortedSet<Literal> getCompleteBackbone() {
        SortedSet<Literal> completeBackbone = new TreeSet<>();
        for (Variable var : this.positiveBackbone) {
            completeBackbone.add(var);
        }
        for (Variable var : this.negativeBackbone) {
            completeBackbone.add(var.negate());
        }
        return completeBackbone;
    }

    /**
     * Adds a given set of literals to each either the positive or negative set of Backbone literals.
     * @param literals  the set of literals to be added
     */
    public void add(Collection<Literal> literals) {
        for (Literal literal : literals) {
            if (literal.phase()) {
                this.positiveBackbone.add(literal.variable());
            } else {
                this.negativeBackbone.add(literal.variable());
            }
        }
    }

    /**
     * Adds a given literal to either the positive or negative set of Backbone literals.
     * @param literal  the literal to be added
     */
    public void add(Literal l) {
        if (l.phase()) {
            this.positiveBackbone.add(l.variable());
        } else {
            this.negativeBackbone.add(l.variable());
        }
    }

    /**
     * Checks whether this backbone contains no literals.s
     * @return  {@code true} if this Backbone is empty, {@code false} otherwise
     */
    public Boolean isEmpty() {
        return this.positiveBackbone.isEmpty() && this.negativeBackbone.isEmpty();
    }
}
