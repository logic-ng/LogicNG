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

package org.logicng.io.writers;

import org.logicng.formulas.Formula;
import org.logicng.io.graphical.translators.FormulaAstTranslator;
import org.logicng.io.graphical.translators.FormulaDagTranslator;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for a formula.  Writes the internal data structure of the formula to a dot file.
 * @version 2.4.0
 * @since 1.0
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0.  For a more configurable and flexible
 * to use graph writer use {@link FormulaDagTranslator} or {@link FormulaAstTranslator} within the new
 * graphical writer framework.
 */
@Deprecated
public final class FormulaDotFileWriter {

    /**
     * Private constructor.
     */
    private FormulaDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default style configuration.
     * @param fileName      the file name of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean alignLiterals) throws IOException {
        write(fileName, formula, alignLiterals);
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default style configuration.
     * @param file          the file of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean alignLiterals) throws IOException {
        FormulaDagTranslator.builder().alignTerminals(alignLiterals).build().translate(formula).writeDot(file);
    }
}
