package org.logicng.explanations.backbones;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A backbone (the set of literals of a formula that need to be assigned true in order for the formula to be
 * satisfied).
 * @version 1.5
 * @since 1.5
 */
public class Backbone {
    private final SortedSet<Variable> positiveBackbone;
    private final SortedSet<Variable> negativeBackbone;
    private SortedSet<Variable> optionalVariables;

    /**
     * Constructs a new empty Backbone.
     */
    public Backbone() {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = new TreeSet<>();
    }

    /**
     * Constructs a new Backbone that contains the variables of the given literals.
     */
    public Backbone(final Collection<Literal> literals) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = new TreeSet<>();
        this.addBackboneLiterals(literals);
    }

    /**
     * Constructs a new Backbone that contains the variables from the given literals and the given optional literals.
     */
    public Backbone(final Collection<Literal> backboneLiterals, final Collection<Variable> optionalVariables) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = (SortedSet<Variable>) optionalVariables;
        this.addBackboneLiterals(backboneLiterals);
    }

    /**
     * Constructs a new Backbone that contains the given variables and the given optional variables.
     */
    public Backbone(final Collection<Variable> backboneLiteralsPos, final Collection<Variable> backboneLiteralsNeg, final Collection<Variable> optionalVariables) {
        this.positiveBackbone = (SortedSet<Variable>) backboneLiteralsPos;
        this.negativeBackbone = (SortedSet<Variable>) backboneLiteralsNeg;
        this.optionalVariables = (SortedSet<Variable>) optionalVariables;
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
     * Returns the variables of the formula that are optionally but not necessarily satisfiable (e.g. not in the backbone).
     * @return the set of satisfiable non-backbone variables
     */
    public SortedSet<Variable> getOptionalVariables() {
        return this.optionalVariables;
    }

    /**
     * Sets the variables of the formula that are optionally but not necessarily satisfiable (e.g. not in the backbone).
     * @param variables the set of satisfiable non-backbone variables
     */
    public void setOptionalVariables(final Collection<Variable> variables) {
        this.optionalVariables = (SortedSet<Variable>) variables;
    }

    /**
     * Returns all literals of the Backbone.
     * @return the set of both positive and negative Backbone literals
     */
    public SortedSet<Literal> getCompleteBackbone() {
        final SortedSet<Literal> completeBackbone = new TreeSet<>();
        completeBackbone.addAll(this.positiveBackbone);
        for (final Variable var : this.negativeBackbone) {
            completeBackbone.add(var.negate());
        }
        return completeBackbone;
    }

    public Formula toFormula() {
        final FormulaFactory f = getFormulaFactory();
        return f.and(this.getCompleteBackbone());
    }

    public Map<Variable, Tristate> toMap() {
        final Map<Variable, Tristate> map = new HashMap<>();
        for (final Variable var : this.positiveBackbone) {
           map.put(var, Tristate.TRUE);
        }
        for (final Variable var : this.negativeBackbone) {
            map.put(var, Tristate.FALSE);
        }
        for (final Variable var : this.optionalVariables) {
            map.put(var, Tristate.UNDEF);
        }
        return map;
    }

    private FormulaFactory getFormulaFactory() {
        final FormulaFactory f;
        if (!this.positiveBackbone.isEmpty()) {
            f = this.positiveBackbone.first().factory();
        } else if (!this.negativeBackbone.isEmpty()) {
            f = this.negativeBackbone.first().factory();
        } else if (!this.optionalVariables.isEmpty()) {
            f = this.optionalVariables.first().factory();
        } else {
            f = new FormulaFactory();
        }
        return f;
    }

    /**
     * Adds a given set of literals to each either the positive or negative set of backbone literals.
     * @param literals  the set of literals to be added
     */
    public void addBackboneLiterals(final Collection<Literal> literals) {
        for (final Literal literal : literals) {
            if (literal.phase()) {
                this.positiveBackbone.add(literal.variable());
            } else {
                this.negativeBackbone.add(literal.variable());
            }
        }
    }

    /**
     * Checks whether this backbone contains no literals.
     * @return  {@code true} if this Backbone is empty, {@code false} otherwise
     */
    public Boolean isEmpty() {
        return this.positiveBackbone.isEmpty() && this.negativeBackbone.isEmpty();
    }
}
