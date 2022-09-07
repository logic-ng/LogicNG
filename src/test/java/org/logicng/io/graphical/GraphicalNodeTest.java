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

package org.logicng.io.graphical;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.io.graphical.GraphicalColor.BLUE;
import static org.logicng.io.graphical.GraphicalColor.WHITE;
import static org.logicng.io.graphical.GraphicalColor.YELLOW;
import static org.logicng.io.graphical.GraphicalNodeStyle.noStyle;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class GraphicalNodeTest {

    private static final GraphicalDotWriter dotWriter = GraphicalDotWriter.get();
    private static final GraphicalMermaidWriter mermaidWriter = GraphicalMermaidWriter.get();

    private GraphicalRepresentation gr;

    @BeforeEach
    public void init() {
        this.gr = new GraphicalRepresentation(false, true);
    }

    @Test
    public void testNoStyleNode() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", noStyle());
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1([\"ID 1\"])");
        assertThat(this.gr.writeString(mermaidWriter)).doesNotContain("style");
    }

    @Test
    public void testOnlyShape() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.rectangle(null, null, null));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", shape=box]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1[\"ID 1\"]");
        assertThat(this.gr.writeString(mermaidWriter)).doesNotContain("style");
    }

    @Test
    public void testOnlyStrokeColor() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.style(null, BLUE, null, null));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", color=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1([\"ID 1\"])");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 stroke:#004f93");
    }

    @Test
    public void testOnlyTextColor() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.style(null, null, BLUE, null));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", fontcolor=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1([\"ID 1\"])");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 color:#004f93");
    }

    @Test
    public void testOnlyBackgroundColor() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.style(null, null, null, BLUE));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", style=filled, fillcolor=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1([\"ID 1\"])");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 fill:#004f93");
    }

    @Test
    public void testMixed1() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.circle(null, null, BLUE));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", shape=circle, style=filled, fillcolor=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1((\"ID 1\"))");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 fill:#004f93");
    }

    @Test
    public void testMixed2() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.circle(null, WHITE, BLUE));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", shape=circle, fontcolor=\"#ffffff\", style=filled, fillcolor=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1((\"ID 1\"))");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 color:#ffffff,fill:#004f93");
    }

    @Test
    public void testAll() {
        final GraphicalNode node = new GraphicalNode("id1", "ID 1", GraphicalNodeStyle.circle(YELLOW, WHITE, BLUE));
        this.gr.addNode(node);
        assertThat(this.gr.writeString(dotWriter)).contains("id1 [label=\"ID 1\", shape=circle, color=\"#ffc612\", fontcolor=\"#ffffff\", style=filled, fillcolor=\"#004f93\"]");
        assertThat(this.gr.writeString(mermaidWriter)).contains("id1((\"ID 1\"))");
        assertThat(this.gr.writeString(mermaidWriter)).contains("style id1 stroke:#ffc612,color:#ffffff,fill:#004f93");
    }

}
