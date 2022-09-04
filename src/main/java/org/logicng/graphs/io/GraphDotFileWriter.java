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

package org.logicng.graphs.io;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.io.graphical.translators.GraphTranslator;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for a graph.  Writes the internal data structure of the graph to a dot file.
 * @version 2.4.0
 * @since 1.2
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0.  For a more configurable and flexible
 * to use graph writer use {@link GraphTranslator} within the new graphical writer framework.
 */
@Deprecated
public final class GraphDotFileWriter {

    /**
     * Private constructor.
     */
    private GraphDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given graph's internal data structure as a dot file.
     * @param fileName the file name of the dot file to write
     * @param graph    the graph
     * @param <T>      the type of the graph content
     * @throws IOException if there was a problem writing the file
     */
    public static <T> void write(final String fileName, final Graph<T> graph) throws IOException {
        write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), graph);
    }

    /**
     * Writes a given graph's internal data structure as a dot file.
     * @param file  the file of the dot file to write
     * @param graph the graph
     * @param <T>   the type of the graph content
     * @throws IOException if there was a problem writing the file
     */
    public static <T> void write(final File file, final Graph<T> graph) throws IOException {
        GraphTranslator.builder().build().translate(graph).writeDot(file);
    }
}
