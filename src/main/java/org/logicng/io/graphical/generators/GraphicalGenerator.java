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

package org.logicng.io.graphical.generators;

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

/**
 * An abstract super class for graphical generators.
 * @param <C> the type of the content from which the nodes are generated
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class GraphicalGenerator<C> {
    protected static final String ID = "id";

    protected final GraphicalColor backgroundColor;
    protected final boolean alignTerminals;
    protected final GraphicalEdgeStyle edgeStyle;
    protected final GraphicalNodeStyle defaultNodeStyle;
    protected final NodeStyleMapper<C> nodeStyleMapper;

    protected GraphicalGenerator(final GraphicalColor backgroundColor, final boolean alignTerminals, final GraphicalEdgeStyle edgeStyle,
                                 final GraphicalNodeStyle defaultNodeStyle, final NodeStyleMapper<C> nodeStyleMapper) {
        this.backgroundColor = backgroundColor;
        this.alignTerminals = alignTerminals;
        this.edgeStyle = edgeStyle;
        this.defaultNodeStyle = defaultNodeStyle;
        this.nodeStyleMapper = nodeStyleMapper;
    }

    protected GraphicalNodeStyle style(final C content) {
        return this.nodeStyleMapper != null ? this.nodeStyleMapper.computeStyle(content) : this.defaultNodeStyle;
    }
}
