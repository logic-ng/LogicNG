// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;

/**
 * Unit tests for {@link BDDNode}, {@link BDDConstant}, and {@link BDDInnerNode}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDNodeTest {

    @Test
    public void testSimpleMethods() {
        final FormulaFactory f = new FormulaFactory();
        final BDDNode verumNode = BDDConstant.getVerumNode(f);
        final BDDNode falsumNode = BDDConstant.getFalsumNode(f);
        final BDDNode innerNode = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        assertThat(verumNode.isInnerNode()).isFalse();
        assertThat(falsumNode.isInnerNode()).isFalse();
        assertThat(innerNode.isInnerNode()).isTrue();
        assertThat(verumNode.label()).isEqualTo(f.verum());
        assertThat(falsumNode.label()).isEqualTo(f.falsum());
        assertThat(innerNode.label()).isEqualTo(f.variable("A"));
        assertThat(verumNode.low()).isNull();
        assertThat(verumNode.high()).isNull();
        assertThat(falsumNode.low()).isNull();
        assertThat(falsumNode.high()).isNull();
        assertThat(innerNode.low()).isEqualTo(verumNode);
        assertThat(innerNode.high()).isEqualTo(falsumNode);
    }

    @Test
    public void testNodes() {
        final FormulaFactory f = new FormulaFactory();
        final BDDNode verumNode = BDDConstant.getVerumNode(f);
        final BDDNode falsumNode = BDDConstant.getFalsumNode(f);
        final BDDNode innerNode = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        assertThat(verumNode.nodes()).containsExactly(verumNode);
        assertThat(falsumNode.nodes()).containsExactly(falsumNode);
        assertThat(innerNode.nodes()).containsExactlyInAnyOrder(verumNode, falsumNode, innerNode);
    }

    @Test
    public void testHashCode() {
        final FormulaFactory f = new FormulaFactory();
        final BDDNode verumNode = BDDConstant.getVerumNode(f);
        final BDDNode falsumNode = BDDConstant.getFalsumNode(f);
        final BDDNode innerNode1 = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        final BDDNode innerNode2 = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        assertThat(verumNode.hashCode()).isEqualTo(verumNode.hashCode());
        assertThat(falsumNode.hashCode()).isEqualTo(falsumNode.hashCode());
        assertThat(innerNode1.hashCode()).isEqualTo(innerNode2.hashCode());
    }

    @Test
    public void testEquals() {
        final FormulaFactory f = new FormulaFactory();
        final BDDNode verumNode = BDDConstant.getVerumNode(f);
        final BDDNode falsumNode = BDDConstant.getFalsumNode(f);
        final BDDNode innerNode1 = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        final BDDNode innerNode1a = new BDDInnerNode(f.variable("A"), verumNode, falsumNode);
        final BDDNode innerNode2 = new BDDInnerNode(f.variable("A"), falsumNode, verumNode);
        final BDDNode innerNode3 = new BDDInnerNode(f.variable("B"), verumNode, falsumNode);
        assertThat(verumNode).isEqualTo(verumNode);
        assertThat(falsumNode).isEqualTo(falsumNode);
        assertThat(innerNode1).isEqualTo(innerNode1);
        assertThat(innerNode1.equals(innerNode1)).isTrue();
        assertThat(innerNode1).isEqualTo(innerNode1a);
        assertThat(innerNode1).isNotEqualTo(innerNode2);
        assertThat(innerNode1).isNotEqualTo(innerNode3);
        assertThat(innerNode1).isNotEqualTo(null);
        assertThat(innerNode1).isNotEqualTo("String");
        assertThat(verumNode).isNotEqualTo(falsumNode);
        assertThat(verumNode).isNotEqualTo(innerNode3);
        assertThat(verumNode).isNotEqualTo(null);
        assertThat(verumNode).isNotEqualTo("String");
    }
}
