package org.logicng.io.graphical.translators;

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

public abstract class GraphicalTranslator {
    protected static final String ID = "id";

    protected final GraphicalColor backgroundColor;
    protected final boolean alignTerminals;
    protected final GraphicalEdgeStyle edgeStyle;
    protected final GraphicalNodeStyle nodeStyle;

    public GraphicalTranslator(final GraphicalColor backgroundColor, final boolean alignTerminals, final GraphicalEdgeStyle edgeStyle,
                               final GraphicalNodeStyle nodeStyle) {
        this.backgroundColor = backgroundColor;
        this.alignTerminals = alignTerminals;
        this.edgeStyle = edgeStyle;
        this.nodeStyle = nodeStyle;
    }
}
