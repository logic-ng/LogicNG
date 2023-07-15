// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.collections.LNGIntVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

/**
 * A DTree generator using the min-fill heuristic.
 * @version 2.0.0
 * @since 2.0.0
 */
public class MinFillDTreeGenerator extends EliminatingOrderDTreeGenerator {

    @Override
    public DTree generate(final Formula cnf) {
        final Graph graph = new Graph(cnf);
        final List<Variable> ordering = graph.getMinFillOrdering();
        return generateWithEliminatingOrder(cnf, ordering);
    }

    /**
     * Undirected Graph
     */
    public static class Graph {
        protected final int numberOfVertices;

        /**
         * The adjacency matrix (which is symmetric since the graph is undirected)
         */
        protected final boolean[][] adjMatrix;

        /**
         * The list of vertices
         */
        protected final List<Variable> vertices;

        /**
         * The edges of the graph as a list of edges per node ({{2,3},{1},{1}} means that there are the edges 1-2 and 1-3)
         */
        protected final List<LNGIntVector> edgeList;

        /**
         * Computes the DTree from the given CNF.
         * @param cnf the CNF
         */
        public Graph(final Formula cnf) {
            /* build vertices */
            this.numberOfVertices = cnf.variables().size();
            this.vertices = new ArrayList<>(this.numberOfVertices);
            final Map<Literal, Integer> varToIndex = new HashMap<>();
            int index = 0;
            for (final Variable variable : cnf.variables()) {
                this.vertices.add(variable);
                varToIndex.put(variable, index++);
            }

            /* build edge list and adjacency matrix */
            this.adjMatrix = new boolean[this.numberOfVertices][this.numberOfVertices];
            this.edgeList = new ArrayList<>(this.numberOfVertices);
            for (int i = 0; i < this.numberOfVertices; i++) {
                this.edgeList.add(new LNGIntVector());
            }

            for (final Formula clause : cnf) {
                final SortedSet<Variable> variables = clause.variables();
                final int[] varNums = new int[variables.size()];
                index = 0;
                for (final Literal var : variables) {
                    varNums[index++] = varToIndex.get(var);
                }
                for (int i = 0; i < varNums.length; i++) {
                    for (int j = i + 1; j < varNums.length; j++) {
                        this.edgeList.get(varNums[i]).push(varNums[j]);
                        this.edgeList.get(varNums[j]).push(varNums[i]);
                        this.adjMatrix[varNums[i]][varNums[j]] = true;
                        this.adjMatrix[varNums[j]][varNums[i]] = true;
                    }
                }
            }
        }

        protected List<LNGIntVector> getCopyOfEdgeList() {
            final List<LNGIntVector> result = new ArrayList<>();
            for (final LNGIntVector edge : this.edgeList) {
                result.add(new LNGIntVector(edge));
            }
            return result;
        }

        protected boolean[][] getCopyOfAdjMatrix() {
            final boolean[][] result = new boolean[this.numberOfVertices][this.numberOfVertices];
            for (int i = 0; i < this.numberOfVertices; i++) {
                result[i] = Arrays.copyOf(this.adjMatrix[i], this.numberOfVertices);
            }
            return result;
        }

        protected List<Variable> getMinFillOrdering() {
            final boolean[][] fillAdjMatrix = getCopyOfAdjMatrix();
            final List<LNGIntVector> fillEdgeList = getCopyOfEdgeList();

            final Variable[] ordering = new Variable[this.numberOfVertices];
            final boolean[] processed = new boolean[this.numberOfVertices];
            int treewidth = 0;

            for (int iteration = 0; iteration < this.numberOfVertices; iteration++) {
                final LNGIntVector possiblyBestVertices = new LNGIntVector();
                int minEdges = Integer.MAX_VALUE;
                for (int currentVertex = 0; currentVertex < this.numberOfVertices; currentVertex++) {
                    if (processed[currentVertex]) {
                        continue;
                    }
                    int edgesAdded = 0;
                    final LNGIntVector neighborList = fillEdgeList.get(currentVertex);
                    for (int i = 0; i < neighborList.size(); i++) {
                        final int firstNeighbor = neighborList.get(i);
                        if (processed[firstNeighbor]) {
                            continue;
                        }
                        for (int j = i + 1; j < neighborList.size(); j++) {
                            final int secondNeighbor = neighborList.get(j);
                            if (processed[secondNeighbor]) {
                                continue;
                            }
                            if (!fillAdjMatrix[firstNeighbor][secondNeighbor]) {
                                edgesAdded++;
                            }
                        }
                    }
                    if (edgesAdded < minEdges) {
                        minEdges = edgesAdded;
                        possiblyBestVertices.clear();
                        possiblyBestVertices.push(currentVertex);
                    } else if (edgesAdded == minEdges) {
                        possiblyBestVertices.push(currentVertex);
                    }
                }

                final int bestVertex = possiblyBestVertices.get(0); // or choose randomly

                final LNGIntVector neighborList = fillEdgeList.get(bestVertex);
                for (int i = 0; i < neighborList.size(); i++) {
                    final int firstNeighbor = neighborList.get(i);
                    if (processed[firstNeighbor]) {
                        continue;
                    }
                    for (int j = i + 1; j < neighborList.size(); j++) {
                        final int secondNeighbor = neighborList.get(j);
                        if (processed[secondNeighbor]) {
                            continue;
                        }
                        if (!fillAdjMatrix[firstNeighbor][secondNeighbor]) {
                            fillAdjMatrix[firstNeighbor][secondNeighbor] = true;
                            fillAdjMatrix[secondNeighbor][firstNeighbor] = true;
                            fillEdgeList.get(firstNeighbor).push(secondNeighbor);
                            fillEdgeList.get(secondNeighbor).push(firstNeighbor);
                        }
                    }
                }

                int currentNumberOfEdges = 0;
                for (int k = 0; k < this.numberOfVertices; k++) {
                    if (fillAdjMatrix[bestVertex][k] && !processed[k]) {
                        currentNumberOfEdges++;
                    }
                }
                if (treewidth < currentNumberOfEdges) {
                    treewidth = currentNumberOfEdges;
                }

                processed[bestVertex] = true;
                ordering[iteration] = this.vertices.get(bestVertex);
            }
            return Arrays.asList(ordering);
        }
    }
}
