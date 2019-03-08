package org.logicng.explanations.backbones;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
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
    private final SortedSet<Variable> optionalVariables;

    /**
     * Constructs a new backbone that contains the given backbone variables and the given optional variables.
     * @param positiveBackbone  positive backbone variables
     * @param negativeBackbone  negative backbone variables
     * @param optionalVariables optional variables
     */
    public Backbone(final SortedSet<Variable> positiveBackbone, final SortedSet<Variable> negativeBackbone, final SortedSet<Variable> optionalVariables) {
        this.positiveBackbone = positiveBackbone;
        this.negativeBackbone = negativeBackbone;
        this.optionalVariables = optionalVariables;
    }

    /**
     * Tests whether the backbone has a positive backbone result.
     * @return {@code true} if the backbone has a positive backbone result, {@code false} otherwise
     */
    public boolean hasPositiveBackboneResult() {
        return this.positiveBackbone != null;
    }

    /**
     * Tests whether the backbone has a negative backbone result.
     * @return {@code true} if the backbone has a negative backbone result, {@code false} otherwise
     */
    public boolean hasNegativeBackboneResult() {
        return this.negativeBackbone != null;
    }

    /**
     * Tests whether the backbone has an optional variables result.
     * @return {@code true} if the backbone has an optional variables result, {@code false} otherwise
     */
    public boolean hasOptionalVariablesResult() {
        return this.optionalVariables != null;
    }

    /**
     * Returns the positive variables of the backbone.
     * @return the set of positive backbone variables
     */
    public SortedSet<Variable> getPositiveBackbone() {
        return hasPositiveBackboneResult() ? Collections.unmodifiableSortedSet(this.positiveBackbone) : null;
    }

    /**
     * Returns the negative variables of the backbone.
     * @return the set of negative backbone variables
     */
    public SortedSet<Variable> getNegativeBackbone() {
        return hasNegativeBackboneResult() ? Collections.unmodifiableSortedSet(this.negativeBackbone) : null;
    }

    /**
     * Returns the variables of the formula that are optional, i.e. not in the backbone.
     * @return the set of non-backbone variables
     */
    public SortedSet<Variable> getOptionalVariables() {
        return hasOptionalVariablesResult() ? Collections.unmodifiableSortedSet(this.optionalVariables) : null;
    }

    /**
     * Returns all literals of the backbone.
     * @return the set of both positive and negative backbone literals
     */
    public SortedSet<Literal> getCompleteBackbone() {
        final SortedSet<Literal> completeBackbone = new TreeSet<>();
        if (hasPositiveBackboneResult()) {
            completeBackbone.addAll(this.positiveBackbone);
        }
        if (hasNegativeBackboneResult()) {
            for (final Variable var : this.negativeBackbone) {
                completeBackbone.add(var.negate());
            }
        }
        return Collections.unmodifiableSortedSet(completeBackbone);
    }

    /**
     * Returns the backbone as a conjunction of literals.
     * @param f the formula factory needed for construction the backbone formula.
     * @return the backbone formula
     */
    public Formula toFormula(final FormulaFactory f) {
        return f.and(this.getCompleteBackbone());
    }

    /**
     * Returns the backbone as map from variables to tri-states. A positive variable is mapped to {@code Tristate.TRUE},
     * a negative variable to {@code Tristate.FALSE} and the optional variables to {@code Tristate.UNDEF}.
     * @return the mapping of the backbone
     */
    public Map<Variable, Tristate> toMap() {
        final Map<Variable, Tristate> map = new HashMap<>();
        if (hasPositiveBackboneResult()) {
            for (final Variable var : this.positiveBackbone) {
                map.put(var, Tristate.TRUE);
            }
        }
        if (hasNegativeBackboneResult()) {
            for (final Variable var : this.negativeBackbone) {
                map.put(var, Tristate.FALSE);
            }
        }
        if (hasOptionalVariablesResult()) {
            for (final Variable var : this.optionalVariables) {
                map.put(var, Tristate.UNDEF);
            }
        }
        return Collections.unmodifiableMap(map);
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
}
