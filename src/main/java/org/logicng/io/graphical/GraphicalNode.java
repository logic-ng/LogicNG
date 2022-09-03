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

import java.util.Objects;

public class GraphicalNode {
    final String id;
    final String label;
    final boolean terminal;
    final GraphicalNodeStyle style;

    public GraphicalNode(final String id, final String label) {
        this(id, label, false, new GraphicalNodeStyle());
    }

    public GraphicalNode(final String id, final String label, final boolean terminal) {
        this(id, label, terminal, new GraphicalNodeStyle());
    }

    public GraphicalNode(final String id, final String label, final GraphicalNodeStyle style) {
        this(id, label, false, style);
    }

    public GraphicalNode(final String id, final String label, final boolean terminal, final GraphicalNodeStyle style) {
        this.id = id;
        this.label = label;
        this.terminal = terminal;
        this.style = style;
    }

    public String getId() {
        return this.id;
    }

    public String getLabel() {
        return this.label;
    }

    public boolean isTerminal() {
        return this.terminal;
    }

    public GraphicalNodeStyle getStyle() {
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
        final GraphicalNode that = (GraphicalNode) o;
        return this.terminal == that.terminal &&
                Objects.equals(this.id, that.id) &&
                Objects.equals(this.label, that.label) &&
                Objects.equals(this.style, that.style);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.id, this.label, this.terminal, this.style);
    }

    @Override
    public String toString() {
        return "GraphicalNode{" +
                "id='" + this.id + '\'' +
                ", label='" + this.label + '\'' +
                ", terminal=" + this.terminal +
                ", style=" + this.style +
                '}';
    }
}
