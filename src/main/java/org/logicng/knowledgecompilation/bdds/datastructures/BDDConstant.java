// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.datastructures;

import org.logicng.formulas.Constant;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * A terminal node in a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDConstant implements BDDNode {

    private final Constant value;

    /**
     * Private constructor.
     * @param value the constant value
     */
    private BDDConstant(final Constant value) {
        this.value = value;
    }

    /**
     * Returns the terminal 0 node.
     * @param f the formula factory
     * @return the terminal 0 node
     */
    public static BDDConstant getFalsumNode(final FormulaFactory f) {
        return new BDDConstant(f.falsum());
    }

    /**
     * Returns the terminal 1 node.
     * @param f the formula factory
     * @return the terminal 1 node
     */
    public static BDDConstant getVerumNode(final FormulaFactory f) {
        return new BDDConstant(f.verum());
    }

    @Override
    public Formula label() {
        return this.value;
    }

    @Override
    public boolean isInnerNode() {
        return false;
    }

    @Override
    public BDDNode low() {
        return null;
    }

    @Override
    public BDDNode high() {
        return null;
    }

    @Override
    public Set<BDDNode> nodes() {
        return new HashSet<>(Collections.singletonList(this));
    }

    @Override
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    public boolean equals(final Object other) {
        return this == other || other instanceof BDDConstant
                && Objects.equals(this.value, ((BDDConstant) other).value);
    }

    @Override
    public String toString() {
        return "<" + this.value + ">";
    }
}
