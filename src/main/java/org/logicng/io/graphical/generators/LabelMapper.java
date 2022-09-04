package org.logicng.io.graphical.generators;

/**
 * A label mapper for generating graphical representations of formulas, BDDs and graphs.
 * This mapper can be used to compute a label for the given node content.
 * @param <T> the type of the node content
 * @version 2.4.0
 * @since 2.4.0
 */
@FunctionalInterface
public interface LabelMapper<T> {

    /**
     * Computes a label for the given node content.
     * @param content the content of the node
     * @return the label for the node with this content
     */
    String computeLabel(T content);
}
