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

package org.logicng.knowledgecompilation.bdds.io;

import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

/**
 * A dot file writer for BDDs.  Writes the internal data structure of a BDD to a dot file.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class BDDDotFileWriter {

    private static final String CONST_TRUE = "const_true";
    private static final String CONST_FALSE = "const_false";
    private static final String NODE_PREFIX = "id_";

    /**
     * Private constructor.
     */
    private BDDDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given BDD as a dot file.
     * @param fileName the file name of the dot file to write
     * @param bdd      the BDD
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final BDD bdd) throws IOException {
        write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), bdd);
    }

    /**
     * Writes a given formula's internal data structure as a dot file.
     * @param file the file of the dot file to write
     * @param bdd  the BDD
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final BDD bdd) throws IOException {
        final StringBuilder sb = new StringBuilder(String.format("digraph G {%n"));
        if (!bdd.isContradiction()) {
            sb.append(String.format("  %s [shape=box, label=\"$true\", style = bold, color = darkgreen];%n", CONST_TRUE));
        }
        if (!bdd.isTautology()) {
            sb.append(String.format("  %s [shape=box, label=\"$false\", style = bold, color = red];%n", CONST_FALSE));
        }
        for (final int[] internalNode : new BDDOperations(bdd.underlyingKernel()).allNodes(bdd.index())) {
            sb.append(String.format("  %s%d [shape=ellipse, label=\"%s\"];%n", NODE_PREFIX, internalNode[0], bdd.underlyingKernel().getVariableForIndex(internalNode[1]).name()));
            sb.append(String.format("  %s%d -> %s [style = dotted, color = red];%n", NODE_PREFIX, internalNode[0], getNodeString(internalNode[2])));
            sb.append(String.format("  %s%d -> %s [color = darkgreen];%n", NODE_PREFIX, internalNode[0], getNodeString(internalNode[3])));
        }
        sb.append("}").append(System.lineSeparator());
        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
    }

    private static String getNodeString(final int i) {
        switch (i) {
            case 0:
                return CONST_FALSE;
            case 1:
                return CONST_TRUE;
            default:
                return NODE_PREFIX + i;
        }
    }
}
