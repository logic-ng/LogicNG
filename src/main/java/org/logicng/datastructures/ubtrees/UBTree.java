package org.logicng.datastructures.ubtrees;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * A data structure for storing sets with performant sub- and superset queries.
 * C.f. `A New Method to Index and Query Sets`, Hoffmann and Koehler, 1999
 * @param <T> the type of the elements (must be comparable)
 * @version 1.5.0
 * @since 1.5.0
 */
public class UBTree<T extends Comparable<T>> {
    private SortedMap<T, UBNode<T>> rootNodes;

    public UBTree() {
        this.rootNodes = new TreeMap<>();
    }

    public void addSet(final SortedSet<T> set) {
        SortedMap<T, UBNode<T>> nodes = rootNodes;
        UBNode<T> node = null;
        for (T element : set) {
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

    public SortedSet<T> firstSubset(SortedSet<T> set) {
        if (this.rootNodes.isEmpty() || set == null || set.isEmpty()) {
            return null;
        }
        return firstSubset(set, this.rootNodes);
    }

    public Set<SortedSet<T>> allSubsets(final SortedSet<T> set) {
        final Set<SortedSet<T>> subsets = new LinkedHashSet<>();
        allSubsets(set, this.rootNodes, subsets);
        return subsets;
    }

    public Set<SortedSet<T>> allSupersets(final SortedSet<T> set) {
        final Set<SortedSet<T>> supersets = new LinkedHashSet<>();
        allSupersets(set, this.rootNodes, supersets);
        return supersets;
    }

    SortedMap<T, UBNode<T>> rootNodes() {
        return rootNodes;
    }

    private SortedSet<T> firstSubset(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest) {
        Set<UBNode<T>> nodes = getAllNodesContainingElements(set, forest);
        SortedSet<T> foundSubset = null;
        for (UBNode<T> node : nodes) {
            if (foundSubset != null) {
                return foundSubset;
            }
            if (node.isEndOfPath()) {
                return node.set();
            }
            SortedSet<T> remainingSet = new TreeSet<>(set);
            remainingSet.remove(set.first());
            foundSubset = firstSubset(remainingSet, node.children());
        }
        return foundSubset;
    }

    private void allSubsets(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest, Set<SortedSet<T>> subsets) {
        Set<UBNode<T>> nodes = getAllNodesContainingElements(set, forest);
        for (UBNode<T> node : nodes) {
            if (node.isEndOfPath()) {
                subsets.add(node.set());
            }
            SortedSet<T> remainingSet = new TreeSet<>(set);
            remainingSet.remove(set.first());
            allSubsets(remainingSet, node.children(), subsets);
        }
    }

    private void allSupersets(final SortedSet<T> set, final SortedMap<T, UBNode<T>> forest, Set<SortedSet<T>> supersets) {
        Set<UBNode<T>> nodes = getAllNodesContainingElementsLessThan(set, forest, set.first());
        for (UBNode<T> node : nodes) {
            allSupersets(set, node.children(), supersets);
        }
        for (UBNode<T> node : forest.values()) {
            if (node.element().equals(set.first())) {
                SortedSet<T> remainingSet = new TreeSet<>(set);
                remainingSet.remove(set.first());
                if (!remainingSet.isEmpty()) {
                    allSupersets(remainingSet, node.children(), supersets);
                } else {
                    List<UBNode<T>> allEndOfPathNodes = getAllEndOfPathNodes(node.children());
                    if (node.isEndOfPath()) {
                        allEndOfPathNodes.add(node);
                    }
                    for (UBNode<T> endOfPathNode :allEndOfPathNodes) {
                        supersets.add(endOfPathNode.set());
                    }
                }
            }
        }
    }

    private Set<UBNode<T>> getAllNodesContainingElements(SortedSet<T> set, SortedMap<T, UBNode<T>> forest) {
        Set<UBNode<T>> nodes = new LinkedHashSet<>();
        for (T element : set) {
            UBNode<T> node = forest.get(element);
            if (node != null) {
                nodes.add(node);
            }
        }
        return nodes;
    }

    private Set<UBNode<T>> getAllNodesContainingElementsLessThan(SortedSet<T> set, SortedMap<T, UBNode<T>> forest, T element) {
        Set<UBNode<T>> nodes = new LinkedHashSet<>();
        for (UBNode<T> node : forest.values()) {
            if (node != null && node.element().compareTo(element) < 0) {
                nodes.add(node);
            }
        }
        return nodes;
    }

    public Set<SortedSet<T>> allSets() {
        final List<UBNode<T>> allEndOfPathNodes = getAllEndOfPathNodes(this.rootNodes);
        final Set<SortedSet<T>> allSets = new LinkedHashSet<>();
        for (UBNode<T> endOfPathNode : allEndOfPathNodes) {
            allSets.add(endOfPathNode.set());
        }
        return allSets;
    }

    private List<UBNode<T>> getAllEndOfPathNodes(SortedMap<T, UBNode<T>> forest) {
        final List<UBNode<T>> endOfPathNodes = new LinkedList<>();
        getAllEndOfPathNodes(forest, endOfPathNodes);
        return endOfPathNodes;
    }

    private void getAllEndOfPathNodes(SortedMap<T, UBNode<T>> forest, List<UBNode<T>> endOfPathNodes) {
        for (UBNode<T> node : forest.values()) {
            if (node.isEndOfPath()) {
                endOfPathNodes.add(node);
            }
            getAllEndOfPathNodes(node.children(), endOfPathNodes);
        }
    }
}
