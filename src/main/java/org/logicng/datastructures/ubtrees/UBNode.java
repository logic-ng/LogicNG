package org.logicng.datastructures.ubtrees;

import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

public class UBNode<T extends Comparable<T>> {

    private final T element;
    private final SortedMap<T, UBNode<T>> children;
    private SortedSet<T> set;

    public UBNode(T element) {
        this.element = element;
        this.children = new TreeMap<>();
    }

    public T element() {
        return this.element;
    }

    public SortedSet<T> set() {
        return this.set;
    }

    public SortedMap<T, UBNode<T>> children() {
        return this.children;
    }

    public boolean isEndOfPath() {
        return this.set != null;
    }

    public void setEndSet(final SortedSet<T> set) {
        this.set = set;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
        UBNode<?> ubNode = (UBNode<?>) o;
        return Objects.equals(element, ubNode.element) &&
                Objects.equals(children, ubNode.children) &&
                Objects.equals(set, ubNode.set);
    }

    @Override
    public int hashCode() {
        return Objects.hash(element, children, set);
    }

    @Override
    public String toString() {
        return "UBNode{" +
                "element=" + element +
                ", children=" + children +
                ", set=" + set +
                '}';
    }
}
