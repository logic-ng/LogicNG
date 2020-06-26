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

package org.logicng.io;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.printer.UTF8StringRepresentation;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.io.writers.FormulaWriter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

/**
 * Unit tests for {@link org.logicng.io.writers.FormulaWriter} and {@link org.logicng.io.readers.FormulaReader}.
 * @version 2.0.0
 * @since 1.2
 */
public class FormulaWriterReaderTest {

    @Test
    public void testSimpleFormulaOneLine() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula1.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PropositionalParser(f).parse("A & B & ~(C | (D => ~E))");
        FormulaWriter.write(file, p1, false);
        final Formula p2 = FormulaReader.readPropositionalFormula(fileName, f);
        final Formula p3 = FormulaReader.readPropositionalFormula(file, f);
        assertThat(p2).isEqualTo(p1);
        assertThat(p3).isEqualTo(p1);
        try (final BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            int count = 0;
            while (reader.ready()) {
                reader.readLine();
                count++;
            }
            assertThat(count).isEqualTo(1);
        }
        Files.deleteIfExists(file.toPath());
    }

    @Test
    public void testSimpleFormulaMultiLine() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula2.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PropositionalParser(f).parse("A & B & ~(C | (D => ~E))");
        FormulaWriter.write(fileName, p1, true);
        final Formula p2 = FormulaReader.readPropositionalFormula(fileName, f);
        assertThat(p2).isEqualTo(p1);
        try (final BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            int count = 0;
            while (reader.ready()) {
                reader.readLine();
                count++;
            }
            assertThat(count).isEqualTo(3);
        }
        Files.deleteIfExists(file.toPath());
    }

    @Test
    public void testPBFormulaOneLine() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula3.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PseudoBooleanParser(f).parse("A & B & ~(C | (D => ~E)) & (2*y + 3*y >= 4) & (x <= 1)");
        FormulaWriter.write(fileName, p1, false);
        final Formula p2 = FormulaReader.readPseudoBooleanFormula(fileName, f);
        final Formula p3 = FormulaReader.readPseudoBooleanFormula(file, f);
        assertThat(p2).isEqualTo(p1);
        assertThat(p3).isEqualTo(p1);
        try (final BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            int count = 0;
            while (reader.ready()) {
                reader.readLine();
                count++;
            }
            assertThat(count).isEqualTo(1);
        }
        Files.deleteIfExists(file.toPath());
    }

    @Test
    public void testPBFormulaMultiLine() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula4.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PseudoBooleanParser(f).parse("A & B & ~(C | (D => ~E)) & (2*y + 3*y >= 4) & (x <= 1)");
        FormulaWriter.write(fileName, p1, true);
        final Formula p2 = FormulaReader.readPseudoBooleanFormula(fileName, f);
        assertThat(p2).isEqualTo(p1);
        try (final BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            int count = 0;
            while (reader.ready()) {
                reader.readLine();
                count++;
            }
            assertThat(count).isEqualTo(5);
        }
        Files.deleteIfExists(file.toPath());
    }

    @Test
    public void testSimpleFormulaOneLineFormatter() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula5.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PropositionalParser(f).parse("A & B & ~(C | (D => ~E))");
        FormulaWriter.write(fileName, p1, false, new UTF8StringRepresentation());
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), StandardCharsets.UTF_8))) {
            assertThat(reader.readLine()).isEqualTo("A ∧ B ∧ ¬(C ∨ (D ⇒ ¬E))");
        }
        Files.deleteIfExists(file.toPath());
    }

    @Test
    public void testSimpleFormulaMultiLineFormatter() throws ParserException, IOException {
        final String fileName = "src/test/resources/writers/temp/simple_formula6.txt";
        final File file = new File(fileName);
        final FormulaFactory f = new FormulaFactory();
        final Formula p1 = new PropositionalParser(f).parse("A & B & ~(C | (D => ~E))");
        FormulaWriter.write(fileName, p1, true, new UTF8StringRepresentation());
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), StandardCharsets.UTF_8))) {
            assertThat(reader.readLine()).isEqualTo("A");
            assertThat(reader.readLine()).isEqualTo("B");
            assertThat(reader.readLine()).isEqualTo("¬(C ∨ (D ⇒ ¬E))");
        }
        Files.deleteIfExists(file.toPath());
    }

}
