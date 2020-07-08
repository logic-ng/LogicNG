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

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.transformations.cnf.CNFConfig;
import org.logicng.transformations.cnf.CNFEncoder;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * Unit tests for the {@link FormulaDotFileWriter}.
 * @version 2.0.0
 * @since 1.2
 */
public class FormulaDimacsFileWriterTest extends TestWithExampleFormulas {

    private final FormulaFactory f = new FormulaFactory();
    private final CNFEncoder encoder = new CNFEncoder(this.f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build());
    private final PropositionalParser p = new PropositionalParser(this.f);
    private final PseudoBooleanParser pp = new PseudoBooleanParser(this.f);

    @Test
    public void testNonCNF() {
        assertThatThrownBy(() -> FormulaDimacsFileWriter.write("non-cnf", this.IMP1, false)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testConstants() throws IOException {
        testFiles("false", this.f.falsum());
        testFiles("true", this.f.verum());
    }

    @Test
    public void testLiterals() throws IOException {
        testFiles("x", this.f.variable("x"));
        testFiles("not_x", this.f.literal("x", false));
    }

    @Test
    public void testFormulas() throws IOException, ParserException {
        final Formula f1 = this.encoder.encode(this.p.parse("(a & b) <=> (~c => (x | z))"));
        final Formula f2 = this.encoder.encode(this.p.parse("a & b | b & ~c"));
        final Formula f3 = this.encoder.encode(this.p.parse("(a & b) <=> (~c => (a | b))"));
        final Formula f4 = this.encoder.encode(this.p.parse("~(a & b) | b & ~c"));
        final Formula f5 = this.encoder.encode(this.pp.parse("a | ~b | (2*a + 3*~b + 4*c <= 4)"));
        testFiles("f1", f1);
        testFiles("f2", f2);
        testFiles("f3", f3);
        testFiles("f4", f4);
        testFiles("f5", f5);
    }

    @Test
    public void testDuplicateFormulaParts() throws ParserException, IOException {
        final Formula f6 = this.encoder.encode(this.p.parse("(a & b) | (c & ~(a & b))"));
        testFiles("f6", f6);
        final Formula f7 = this.encoder.encode(this.p.parse("(c & d) | (a & b) | ((c & d) <=> (a & b))"));
        testFiles("f7", f7);
    }

    private void testFiles(final String fileName, final Formula formula) throws IOException {
        FormulaDimacsFileWriter.write("src/test/resources/writers/temp/" + fileName + "_t.cnf", formula, true);
        FormulaDimacsFileWriter.write("src/test/resources/writers/temp/" + fileName + "_f", formula, false);
        final File expectedT = new File("src/test/resources/writers/formulas-dimacs/" + fileName + "_t.cnf");
        final File expectedF = new File("src/test/resources/writers/formulas-dimacs/" + fileName + "_f.cnf");
        final File tempT = new File("src/test/resources/writers/temp/" + fileName + "_t.cnf");
        final File tempF = new File("src/test/resources/writers/temp/" + fileName + "_f.cnf");
        final File expectedMap = new File("src/test/resources/writers/formulas-dimacs/" + fileName + "_t.map");
        final File tempMap = new File("src/test/resources/writers/temp/" + fileName + "_t.map");
        assertFilesEqual(expectedT, tempT);
        assertFilesEqual(expectedF, tempF);
        assertFilesEqual(expectedMap, tempMap);
    }

    private void assertFilesEqual(final File expected, final File actual) throws IOException {
        final SoftAssertions softly = new SoftAssertions();
        final BufferedReader expReader = new BufferedReader(new FileReader(expected));
        final BufferedReader actReader = new BufferedReader(new FileReader(actual));
        for (int lineNumber = 1; expReader.ready() && actReader.ready(); lineNumber++) {
            softly.assertThat(actReader.readLine()).as("Line " + lineNumber + " not equal").isEqualTo(expReader.readLine());
        }
        if (expReader.ready()) {
            softly.fail("Missing line(s) found, starting with \"" + expReader.readLine() + "\"");
        }
        if (actReader.ready()) {
            softly.fail("Additional line(s) found, starting with \"" + actReader.readLine() + "\"");
        }
        softly.assertAll();
    }
}
