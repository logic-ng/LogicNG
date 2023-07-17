// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDConstant;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDInnerNode;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDNode;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Creates a LogicNG internal BDD data structure of a given BDD.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class LngBDDFunction implements BDDFunction<BDDNode> {

    private static final LngBDDFunction INSTANCE = new LngBDDFunction();

    /**
     * @deprecated In the next version, the standard constructor will be
     *             replaced by a private constructor. In order to instantiate an
     *             object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public LngBDDFunction() {
        // intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static LngBDDFunction get() {
        return INSTANCE;
    }

    @Override
    public BDDNode apply(final BDD bdd) {
        final BDDKernel kernel = bdd.underlyingKernel();
        final int index = bdd.index();
        final Map<Integer, int[]> kernelNodeMap = new BDDOperations(kernel).allNodes(index).stream()
                .collect(Collectors.toMap(node -> node[0], node -> node));
        return buildBDDNode(index, kernel, kernelNodeMap, new HashMap<>());
    }

    private BDDNode buildBDDNode(final int index, final BDDKernel kernel, final Map<Integer, int[]> kernelNodeMap,
                                 final Map<Integer, BDDNode> nodeMap) {
        BDDNode node = nodeMap.get(index);
        if (node != null) {
            return node;
        }
        if (index == BDDKernel.BDD_FALSE) {
            node = BDDConstant.getFalsumNode(kernel.factory());
        } else if (index == BDDKernel.BDD_TRUE) {
            node = BDDConstant.getVerumNode(kernel.factory());
        } else {
            final int[] kernelNode = kernelNodeMap.get(index);
            final Variable variable = kernel.getVariableForIndex(kernelNode[1]);
            final BDDNode lowNode = buildBDDNode(kernelNode[2], kernel, kernelNodeMap, nodeMap);
            final BDDNode highNode = buildBDDNode(kernelNode[3], kernel, kernelNodeMap, nodeMap);
            node = new BDDInnerNode(variable, lowNode, highNode);
        }
        nodeMap.put(index, node);
        return node;
    }
}
