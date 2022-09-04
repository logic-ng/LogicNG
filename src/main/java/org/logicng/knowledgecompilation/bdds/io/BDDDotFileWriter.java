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

import org.logicng.io.graphical.translators.BddTranslator;
import org.logicng.knowledgecompilation.bdds.BDD;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for BDDs.  Writes the internal data structure of a BDD to a dot file.
 * @version 2.4.0
 * @since 1.4.0
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0.  For a more configurable and flexible
 * to use graph writer use {@link BddTranslator} within the new graphical writer framework.
 */
@Deprecated
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
        BddTranslator.builder().build().translate(bdd).writeDot(file);
    }
}
