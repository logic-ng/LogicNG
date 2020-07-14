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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.contentOf;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the {@link BDDDotFileWriter}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDDotFileWriterTest {

    @Test
    public void testWriter() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        testFiles("false", BDDFactory.build(p.parse("$false"), kernel));
        testFiles("true", BDDFactory.build(p.parse("$true"), kernel));
        testFiles("a", BDDFactory.build(p.parse("A"), kernel));
        testFiles("not_a", BDDFactory.build(p.parse("~A"), kernel));
        testFiles("impl", BDDFactory.build(p.parse("A => ~C"), kernel));
        testFiles("equiv", BDDFactory.build(p.parse("A <=> ~C"), kernel));
        testFiles("or", BDDFactory.build(p.parse("A | B | ~C"), kernel));
        testFiles("and", BDDFactory.build(p.parse("A & B & ~C"), kernel));
        testFiles("not", BDDFactory.build(p.parse("~(A & B & ~C)"), kernel));
        testFiles("formula", BDDFactory.build(p.parse("(A => (B|~C)) & (B => C & D) & (D <=> A)"), kernel));
    }

    private void testFiles(final String fileName, final BDD bdd) throws IOException {
        BDDDotFileWriter.write("src/test/resources/writers/temp/" + fileName + "_bdd.dot", bdd);
        final File expectedT = new File("src/test/resources/writers/bdd/" + fileName + "_bdd.dot");
        final File tempT = new File("src/test/resources/writers/temp/" + fileName + "_bdd.dot");
        assertThat(contentOf(tempT)).isEqualTo(contentOf(expectedT));
    }
}

