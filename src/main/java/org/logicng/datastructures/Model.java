package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.List;
import java.util.Objects;

public class Model {
    private final List<Literal> literals;

    public Model(final List<Literal> literals) {
        this.literals = literals;
    }

    public List<Literal> getLiterals() {
        return this.literals;
    }

    public long size() {
        return this.literals.size();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final Model model = (Model) o;
        return Objects.equals(this.literals, model.literals);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.literals);
    }

    @Override
    public String toString() {
        return "Model{" +
                "literals=" + this.literals +
                '}';
    }

    public Formula formula(final FormulaFactory factory) {
        return factory.and(this.literals);
    }
}

