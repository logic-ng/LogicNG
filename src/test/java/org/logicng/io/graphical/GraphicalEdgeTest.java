// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.io.graphical.GraphicalColor.BLACK;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class GraphicalEdgeTest {

    private static final GraphicalDotWriter dotWriter = GraphicalDotWriter.get();
    private static final GraphicalMermaidWriter mermaidWriter = GraphicalMermaidWriter.get();
    private final GraphicalNode n1 = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.noStyle());
    private final GraphicalNode n2 = new GraphicalNode("id2", "ID 2", GraphicalNodeStyle.noStyle());
    private GraphicalRepresentation grUndirected;
    private GraphicalRepresentation grDirected;

    @BeforeEach
    public void init() {
        this.grDirected = new GraphicalRepresentation(false, true);
        this.grUndirected = new GraphicalRepresentation(false, false);
        this.grDirected.addNode(this.n1);
        this.grDirected.addNode(this.n2);
        this.grUndirected.addNode(this.n1);
        this.grUndirected.addNode(this.n2);
    }

    @Test
    public void testNoStyle() {
        final GraphicalEdge edge = new GraphicalEdge(this.n1, this.n2, GraphicalEdgeStyle.noStyle());
        this.grDirected.addEdge(edge);
        this.grUndirected.addEdge(edge);

        assertThat(this.grDirected.writeString(dotWriter)).contains("id1 -> id2");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("id1 --> id2");
        assertThat(this.grDirected.writeString(mermaidWriter)).doesNotContain("linkStyle");

        assertThat(this.grUndirected.writeString(dotWriter)).contains("id1 -- id2");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("id1 --- id2");
        assertThat(this.grUndirected.writeString(mermaidWriter)).doesNotContain("linkStyle");
    }

    @Test
    public void testOnlyEdgeType() {
        final GraphicalEdge edge = new GraphicalEdge(this.n1, this.n2, GraphicalEdgeStyle.dotted(null));
        this.grDirected.addEdge(edge);
        this.grUndirected.addEdge(edge);

        assertThat(this.grDirected.writeString(dotWriter)).contains("id1 -> id2 [style=dotted]");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("id1 --> id2");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke-width:2,stroke-dasharray:3");

        assertThat(this.grUndirected.writeString(dotWriter)).contains("id1 -- id2 [style=dotted]");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("id1 --- id2");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke-width:2,stroke-dasharray:3");
    }

    @Test
    public void testOnlyColor() {
        final GraphicalEdge edge = new GraphicalEdge(this.n1, this.n2, GraphicalEdgeStyle.style(null, BLACK));
        this.grDirected.addEdge(edge);
        this.grUndirected.addEdge(edge);

        assertThat(this.grDirected.writeString(dotWriter)).contains("id1 -> id2 [color=\"#000000\", fontcolor=\"#000000\"]");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("id1 --> id2");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke:#000000");

        assertThat(this.grUndirected.writeString(dotWriter)).contains("id1 -- id2 [color=\"#000000\", fontcolor=\"#000000\"]");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("id1 --- id2");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke:#000000");
    }

    @Test
    public void testAll() {
        final GraphicalEdge edge = new GraphicalEdge(this.n1, this.n2, GraphicalEdgeStyle.bold(BLACK));
        this.grDirected.addEdge(edge);
        this.grUndirected.addEdge(edge);

        assertThat(this.grDirected.writeString(dotWriter)).contains("id1 -> id2 [color=\"#000000\", fontcolor=\"#000000\", style=bold]");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("id1 --> id2");
        assertThat(this.grDirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke:#000000,stroke-width:4");

        assertThat(this.grUndirected.writeString(dotWriter)).contains("id1 -- id2 [color=\"#000000\", fontcolor=\"#000000\", style=bold]");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("id1 --- id2");
        assertThat(this.grUndirected.writeString(mermaidWriter)).contains("linkStyle 0 stroke:#000000,stroke-width:4");
    }
}
