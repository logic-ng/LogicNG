///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

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
 * @version 2.0.0
 * @since 2.0.0
 */
public final class LngBDDFunction implements BDDFunction<BDDNode> {

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
