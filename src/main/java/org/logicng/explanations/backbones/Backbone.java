package org.logicng.explanations.backbones;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.*;

/**
 * A Backbone (the set of literals of a formula that need to be assigned true in order for the formula to be
 * satisfiable).
 * @version 1.5
 * @since 1.5
 */
public class Backbone {
    private SortedSet<Variable> positiveBackbone;
    private SortedSet<Variable> negativeBackbone;
    private SortedSet<Variable> satisfiableVariables;

    /**
     * Constructs a new empty Backbone.
     */
    public Backbone() {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.satisfiableVariables = new TreeSet<>();
    }

    /**
     * Constructs a new Backbone that contains the given literals.
     */
    public Backbone(Collection<Literal> literals) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.satisfiableVariables = new TreeSet<>();
        this.addBackboneLiterals(literals);
    }

    /**
     * Constructs a new Backbone that contains the given literals and the given satisfiable non-backbone literals.
     */
    public Backbone(Collection<Literal> backboneLiterals, Collection<Variable> satisfiableVariables) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.satisfiableVariables = (SortedSet<Variable>) satisfiableVariables;
        this.addBackboneLiterals(backboneLiterals);
    }

    /**
     * Returns the positive variables of the Backbone.
     * @return the set of positive Backbone variables
     */
    public SortedSet<Variable> getPositiveBackbone() {
        return this.positiveBackbone;
    }

    /**
     * Returns the negative variables of the Backbone.
     * @return the set of negative Backbone variables
     */
    public SortedSet<Variable> getNegativeBackbone() {
        return this.negativeBackbone;
    }

    /**
     * Returns the variables of the formula that are satisfiable but not in the Backbone.
     * @return the set of satisfiable non-backbone variables
     */
    public SortedSet<Variable> getSatisfiableVariables() {
        return this.satisfiableVariables;
    }

    /**
     * Sets the variables of the formula that are satisfiable but not in the Backbone.
     * @param variables the set of satisfiable non-backbone variables
     */
    public void setSatisfiableVariables(Collection<Variable> variables) {
        this.satisfiableVariables = (SortedSet<Variable>) variables;
    }

    /**
     * Returns all literals of the Backbone.
     * @return the set of both positive and negative Backbone literals
     */
    public SortedSet<Literal> getCompleteBackbone() {
        SortedSet<Literal> completeBackbone = new TreeSet<>();
        completeBackbone.addAll(this.positiveBackbone);
        for (Variable var : this.negativeBackbone) {
            completeBackbone.add(var.negate());
        }
        return completeBackbone;
    }

    public Formula toFormula() {
        FormulaFactory f = getFormulaFactory();
        return f.and(this.getCompleteBackbone());
    }

    public Map<Variable, Tristate> toMap() {
        Map<Variable, Tristate> map = new HashMap<>();
        for (Variable var : this.positiveBackbone) {
           map.put(var, Tristate.TRUE);
        }
        for (Variable var : this.negativeBackbone) {
            map.put(var, Tristate.FALSE);
        }
        for (Variable var : this.satisfiableVariables) {
            map.put(var, Tristate.UNDEF);
        }
        return map;
    }

    private FormulaFactory getFormulaFactory() {
        FormulaFactory f;
        if (!this.positiveBackbone.isEmpty()) {
            f = this.positiveBackbone.first().factory();
        } else if (!this.negativeBackbone.isEmpty()) {
            f = this.negativeBackbone.first().factory();
        } else if (!this.satisfiableVariables.isEmpty()) {
            f = this.satisfiableVariables.first().factory();
        } else {
            f = new FormulaFactory();
        }
        return f;
    }

    /**
     * Adds a given set of literals to each either the positive or negative set of Backbone literals.
     * @param literals  the set of literals to be added
     */
    public void addBackboneLiterals(Collection<Literal> literals) {
        for (Literal literal : literals) {
            if (literal.phase()) {
                this.positiveBackbone.add(literal.variable());
            } else {
                this.negativeBackbone.add(literal.variable());
            }
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
