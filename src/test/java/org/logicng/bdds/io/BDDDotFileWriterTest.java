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
//  Copyright 2015-2018 Christoph Zengler                                //
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

package org.logicng.bdds.io;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.bdds.BDDFactory;
import org.logicng.bdds.datastructures.BDD;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * Unit tests for the {@link BDDDotFileWriter}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class BDDDotFileWriterTest {

  @Test
  public void testWriter() throws IOException, ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final BDDFactory factory = new BDDFactory(1000, 1000, f);
    factory.setVariableOrder(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));
    testFiles("false", factory.build(p.parse("$false")));
    testFiles("true", factory.build(p.parse("$true")));
    testFiles("a", factory.build(p.parse("A")));
    testFiles("not_a", factory.build(p.parse("~A")));
    testFiles("impl", factory.build(p.parse("A => ~C")));
    testFiles("equiv", factory.build(p.parse("A <=> ~C")));
    testFiles("or", factory.build(p.parse("A | B | ~C")));
    testFiles("and", factory.build(p.parse("A & B & ~C")));
    testFiles("not", factory.build(p.parse("~(A & B & ~C)")));
    testFiles("formula", factory.build(p.parse("(A => (B|~C)) & (B => C & D) & (D <=> A)")));
  }

  private void testFiles(final String fileName, final BDD bdd) throws IOException {
    BDDDotFileWriter.write("src/test/resources/writers/temp/" + fileName + "_bdd.dot", bdd);
    final File expectedT = new File("src/test/resources/writers/bdd/" + fileName + "_bdd.dot");
    final File tempT = new File("src/test/resources/writers/temp/" + fileName + "_bdd.dot");
    assertFilesEqual(expectedT, tempT);
  }

  private void assertFilesEqual(final File expected, final File actual) throws IOException {
    final BufferedReader expReader = new BufferedReader(new FileReader(expected));
    final BufferedReader actReader = new BufferedReader(new FileReader(actual));
    for (int lineNumber = 1; expReader.ready() && actReader.ready(); lineNumber++)
      Assert.assertEquals("Line " + lineNumber + " not equal", expReader.readLine(), actReader.readLine());
    if (expReader.ready())
      Assert.fail("Missing line(s) found, starting with \"" + expReader.readLine() + "\"");
    if (actReader.ready())
      Assert.fail("Additional line(s) found, starting with \"" + actReader.readLine() + "\"");
  }
}

