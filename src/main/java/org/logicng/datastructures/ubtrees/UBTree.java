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

package org.logicng.datastructures.ubtrees;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * A data structure for storing sets with efficient sub- and superset queries.
 * C.f. `A New Method to Index and Query Sets`, Hoffmann and Koehler, 1999
 * @param <T> the type of the elements (must be comparable)
 * @version 2.0.0
 * @since 1.5.0
 */
public final class UBTree<T extends Comparable<T>> {
    private final SortedMap<T, UBNode<T>> rootNodes;

    /**
     * Constructs an empty UBTree.
     */
    public UBTree() {
        this.rootNodes = new TreeMap<>();
    }

    /**
     * Adds a set of comparable objects to this UBTree.
     * @param set the set of comparable objects
     */
    public void addSet(final SortedSet<T> set) {
        SortedMap<T, UBNode<T>> nodes = this.rootNodes;
        UBNode<T> node = null;
        for (final T element : set) {
            node = nodes.get(element);
            if (node == null) {
                node = new UBNode<>(element);
                nodes.put(element, node);
            }
            nodes = node.children();
        }
        if (node != null) {
            node.setEndSet(set);
        }
    }

    /**
     * Returns the first subset of a given set in this UBTree.
     * @param set the set to search for
     * @return the first subset which is found for the given set or {@code null} if there is none
     */
    public SortedSet<T> firstSubset(final SortedSet<T> set) {
        if (this.rootNodes.isEmpty() || set == null || set.isEmpty()) {
            return null;
        }
        return firstSubset(set, this.rootNodes);
    }

    /**
     * Returns all subsets of a given set in this UBTree.
     * @param set the set to search for
     * @return all subsets of of the given set
     */
    public Set<SortedSet<T>> allSubsets(final SortedSet<T> set) {
        final Set<SortedSet<T>> subsets = new LinkedHashSet<>();
        allSubsets(set, this.rootNodes, subsets);
        return subsets;
    }

    /**
     * Returns all supersets of a given set in this UBTree.
     * @param set the set to search for
     * @return all supersets of the given set
     */
    public Set<SortedSet<T>> allSupersets(final SortedSet<T> set) {
        final Set<SortedSet<T>> supersets = new LinkedHashSet<>();
        allSupersets(set, this.rootNodes, supersets);
        return supersets;
    }

    /**
     * Returns all sets in this UBTree.
     * @return all sets in this UBTree
     */
    public Set<SortedSet<T>> allSets() {
        final List<UBNode<T>> allEndOfPathNodes = getAllEndOfPathNodes(this.rootNodes);
        final Set<SortedSet<T>> allSets = new LinkedHashSet<>();
        for (final UBNode<T> endOfPathNode : allEndOfPathNodes) {
            allSets.add(endOfPathNode.set());
        }
        return allSets;
    }

    /**
     * Returns all root nodes of this UBTree.
     * @return all root nodes of this UBTree
     */
    SortedMap<T, UBNode<T>> rootNodes() {
        return this.rootNodes;
    }

    private SortedSet<T> firstSubset(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest) {
        final Set<UBNode<T>> nodes = getAllNodesContainingElements(set, forest);
        SortedSet<T> foundSubset = null;
        for (final UBNode<T> node : nodes) {
            if (foundSubset != null) {
                return foundSubset;
            }
            if (node.isEndOfPath()) {
                return node.set();
            }
            final SortedSet<T> remainingSet = new TreeSet<>(set);
            remainingSet.remove(set.first());
            foundSubset = firstSubset(remainingSet, node.children());
        }
        return foundSubset;
    }

    private void allSubsets(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest, final Set<SortedSet<T>> subsets) {
        final Set<UBNode<T>> nodes = getAllNodesContainingElements(set, forest);
        for (final UBNode<T> node : nodes) {
            if (node.isEndOfPath()) {
                subsets.add(node.set());
            }
            final SortedSet<T> remainingSet = new TreeSet<>(set);
            remainingSet.remove(set.first());
            allSubsets(remainingSet, node.children(), subsets);
        }
    }

    private void allSupersets(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest, final Set<SortedSet<T>> supersets) {
        final Set<UBNode<T>> nodes = getAllNodesContainingElementsLessThan(set, forest, set.first());
        for (final UBNode<T> node : nodes) {
            allSupersets(set, node.children(), supersets);
        }
        for (final UBNode<T> node : forest.values()) {
            if (node.element().equals(set.first())) {
                final SortedSet<T> remainingSet = new TreeSet<>(set);
                remainingSet.remove(set.first());
                if (!remainingSet.isEmpty()) {
                    allSupersets(remainingSet, node.children(), supersets);
                } else {
                    final List<UBNode<T>> allEndOfPathNodes = getAllEndOfPathNodes(node.children());
                    if (node.isEndOfPath()) {
                        allEndOfPathNodes.add(node);
                    }
                    for (final UBNode<T> endOfPathNode : allEndOfPathNodes) {
                        supersets.add(endOfPathNode.set());
                    }
                }
            }
        }
    }

    private Set<UBNode<T>> getAllNodesContainingElements(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest) {
        final Set<UBNode<T>> nodes = new LinkedHashSet<>();
        for (final T element : set) {
            final UBNode<T> node = forest.get(element);
            if (node != null) {
                nodes.add(node);
            }
        }
        return nodes;
    }

    private Set<UBNode<T>> getAllNodesContainingElementsLessThan(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest, final T element) {
        final Set<UBNode<T>> nodes = new LinkedHashSet<>();
        for (final UBNode<T> node : forest.values()) {
            if (node != null && node.element().compareTo(element) < 0) {
                nodes.add(node);
            }
        }
        return nodes;
    }

    private List<UBNode<T>> getAllEndOfPathNodes(final SortedMap<T, UBNode<T>> forest) {
        final List<UBNode<T>> endOfPathNodes = new ArrayList<>();
        getAllEndOfPathNodes(forest, endOfPathNodes);
        return endOfPathNodes;
    }

    private void getAllEndOfPathNodes(final SortedMap<T, UBNode<T>> forest, final List<UBNode<T>> endOfPathNodes) {
        for (final UBNode<T> node : forest.values()) {
            if (node.isEndOfPath()) {
                endOfPathNodes.add(node);
            }
            getAllEndOfPathNodes(node.children(), endOfPathNodes);
        }
    }
}
