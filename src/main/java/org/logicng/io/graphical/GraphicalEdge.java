// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical;

import java.util.Objects;

/**
 * A single edge in a graphical representation of a formula, BDD, or graph. An
 * edge connects two nodes and holds an optional label and an edge style.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalEdge {
    private final GraphicalNode source;
    private final GraphicalNode destination;
    private final String label;
    private final GraphicalEdgeStyle style;

    /**
     * Constructs a new graphical edge with the given values and without label.
     * @param source      the source node of the edge
     * @param destination the destination node of the edge
     * @param style       the style of the edge
     */
    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final GraphicalEdgeStyle style) {
        this(source, destination, null, style);
    }

    /**
     * Constructs a new graphical edge with the given values.
     * @param source      the source node of the edge
     * @param destination the destination node of the edge
     * @param label       the optional label (can be null)
     * @param style       the style of the edge
     */
    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final String label,
                         final GraphicalEdgeStyle style) {
        this.source = source;
        this.destination = destination;
        this.label = label;
        this.style = style;
    }

    /**
     * Returns the source node of this edge.
     * @return the source node of this edge
     */
    public GraphicalNode getSource() {
        return this.source;
    }

    /**
     * Returns the destination node of this edge.
     * @return the destination node of this edge
     */
    public GraphicalNode getDestination() {
        return this.destination;
    }

    /**
     * Returns the label of this edge.
     * @return the label of this edge
     */
    public String getLabel() {
        return this.label;
    }

    /**
     * Returns the style of this edge
     * @return the style of this edge
     */
    public GraphicalEdgeStyle getStyle() {
        return this.style;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GraphicalEdge that = (GraphicalEdge) o;
        return Objects.equals(this.source, that.source) &&
                Objects.equals(this.destination, that.destination) &&
                Objects.equals(this.label, that.label) &&
                Objects.equals(this.style, that.style);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.source, this.destination, this.label, this.style);
    }

    @Override
    public String toString() {
        return "GraphicalEdge{" +
                "source=" + this.source +
                ", destination=" + this.destination +
                ", label='" + this.label + '\'' +
                ", style=" + this.style +
                '}';
    }
}
