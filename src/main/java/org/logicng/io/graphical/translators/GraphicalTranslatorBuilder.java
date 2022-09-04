package org.logicng.io.graphical.translators;

import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

import java.util.function.Function;

public class GraphicalTranslatorBuilder<T extends GraphicalTranslator> {

    private GraphicalColor backgroundColor = WHITE;
    private boolean alginTerminal;
    private GraphicalEdgeStyle edgeStyle = new GraphicalEdgeStyle();
    private GraphicalNodeStyle nodeStyle = new GraphicalNodeStyle();
    private final Function<GraphicalTranslatorBuilder<T>, T> constructor;

    public GraphicalTranslatorBuilder(final Function<GraphicalTranslatorBuilder<T>, T> constructor) {
        this.constructor = constructor;
    }

    public GraphicalTranslatorBuilder<T> backgroundColor(final GraphicalColor color) {
        this.backgroundColor = color;
        return this;
    }

    public GraphicalTranslatorBuilder<T> backgroundColor(final String hexColor) {
        this.backgroundColor = GraphicalColor.hex(hexColor);
        return this;
    }

    public GraphicalTranslatorBuilder<T> alignTerminals(final boolean alginTerminal) {
        this.alginTerminal = alginTerminal;
        return this;
    }

    public GraphicalTranslatorBuilder<T> backgroundColor(final int red, final int green, final int blue) {
        this.backgroundColor = GraphicalColor.rgb(red, green, blue);
        return this;
    }

    public GraphicalTranslatorBuilder<T> edgeStyle(final GraphicalEdgeStyle edgeStyle) {
        this.edgeStyle = edgeStyle;
        return this;
    }

    public GraphicalTranslatorBuilder<T> nodeStyle(final GraphicalNodeStyle nodeStyle) {
        this.nodeStyle = nodeStyle;
        return this;
    }

    public T build() {
        return this.constructor.apply(this);
    }

    public GraphicalColor getBackgroundColor() {
        return this.backgroundColor;
    }

    public boolean isAlginTerminal() {
        return this.alginTerminal;
    }

    public GraphicalEdgeStyle getEdgeStyle() {
        return this.edgeStyle;
    }

    public GraphicalNodeStyle getNodeStyle() {
        return this.nodeStyle;
    }
}
