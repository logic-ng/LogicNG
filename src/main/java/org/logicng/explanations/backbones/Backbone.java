package org.logicng.explanations.backbones;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.*;

/**
 * A backbone (the set of literals of a formula that need to be assigned true in order for the formula to be
 * satisfied).
 *
 * @version 1.5
 * @since 1.5
 */
public class Backbone {
    private final SortedSet<Variable> positiveBackbone;
    private final SortedSet<Variable> negativeBackbone;
    private final SortedSet<Variable> optionalVariables;

    /**
     * Constructs a new empty Backbone.
     */
    public Backbone() {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = new TreeSet<>();
    }

    /**
     * Constructs a new Backbone that contains the variables of the given literals and no optional variables.
     *
     * @param literals the given backbone literals which are split into positive and negative variables.
     */
    public Backbone(final Collection<Literal> literals) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = new TreeSet<>();
        this.addBackboneLiterals(literals);
    }

    /**
     * Constructs a new Backbone that contains the variables from the given literals and the given optional literals.
     *
     * @param backboneLiterals  the given backbone literals which are split in positive and negative variables
     * @param optionalVariables the optional variables
     */
    public Backbone(final Collection<Literal> backboneLiterals, final SortedSet<Variable> optionalVariables) {
        this.positiveBackbone = new TreeSet<>();
        this.negativeBackbone = new TreeSet<>();
        this.optionalVariables = optionalVariables;
        this.addBackboneLiterals(backboneLiterals);
    }

    /**
     * Constructs a new Backbone that contains the given variables and the given optional variables.
     *
     * @param backboneLiteralsPos the given positive variables
     * @param backboneLiteralsNeg the given negative variables
     * @param optionalVariables   the given optional variables
     */
    public Backbone(final SortedSet<Variable> backboneLiteralsPos, final SortedSet<Variable> backboneLiteralsNeg, final SortedSet<Variable> optionalVariables) {
        this.positiveBackbone = backboneLiteralsPos;
        this.negativeBackbone = backboneLiteralsNeg;
        this.optionalVariables = optionalVariables;
    }

    /**
     * Returns the positive variables of the Backbone.
     *
     * @return the set of positive Backbone variables
     */
    public SortedSet<Variable> getPositiveBackbone() {
        return this.positiveBackbone;
    }

    /**
     * Returns the negative variables of the Backbone.
     *
     * @return the set of negative Backbone variables
     */
    public SortedSet<Variable> getNegativeBackbone() {
        return this.negativeBackbone;
    }

    /**
     * Returns the variables of the formula that are optionally but not necessarily satisfiable (e.g. not in the backbone).
     *
     * @return the set of satisfiable non-backbone variables
     */
    public SortedSet<Variable> getOptionalVariables() {
        return this.optionalVariables;
    }

    /**
     * Returns all literals of the Backbone.
     *
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

    /**
     * Returns the backbone as a conjunction of literals.
     *
     * @param f the formula factory needed for construction the backbone formula.
     * @return the backbone formula
     */
    public Formula toFormula(final FormulaFactory f) {
        return f.and(this.getCompleteBackbone());
    }

    /**
     * Returns the backbone as map from variables to tri-states. A positive variable is mapped to {@code Tristate.TRUE},
     * a negative variable to {@code Tristate.FALSE} and the optional variables to {@code Tristate.UNDEF}.
     *
     * @return the mapping of the backbone
     */
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

    /**
     * Adds a given set of literals to each either the positive or negative set of backbone literals.
     *
     * @param literals the set of literals to be added
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
     *
     * @return {@code true} if this Backbone is empty, {@code false} otherwise
     */
    public Boolean isEmpty() {
        return this.positiveBackbone.isEmpty() && this.negativeBackbone.isEmpty();
    }


    @Override
    public boolean equals(final Object o) {
        if (o instanceof Backbone) {
            final Backbone other = (Backbone) o;
            return this.getCompleteBackbone().equals(other.getCompleteBackbone())
                    && this.getOptionalVariables().equals(other.getOptionalVariables());
        }
        return false;
    }
}
