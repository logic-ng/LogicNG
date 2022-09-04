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

import org.logicng.io.graphical.GraphicalNodeStyle;

/**
 * A style mapper for generating graphical representations of formulas, BDDs and graphs.
 * This mapper can be used to compute a node style for the given node content.
 * <p>
 * This can be used to style nodes of a graphical representation dynamically depending
 * on the content of the node.
 * @param <T> the type of the node content
 * @version 2.4.0
 * @since 2.4.0
 */
@FunctionalInterface
public interface NodeStyleMapper<T> {
    /**
     * Computes a style for the given node content.
     * @param content the content of the node
     * @return the style for the node with this content
     */
    GraphicalNodeStyle computeStyle(T content);
}
