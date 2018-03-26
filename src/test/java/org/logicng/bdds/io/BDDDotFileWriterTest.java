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
import org.logicng.bdds.datastructures.LNGBDD;
import org.logicng.bdds.jbuddy.JBuddyFactory;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * Unit tests for the {@link BDDDotFileWriter}.
 * @version 1.4
 * @since 1.4
 */
public class BDDDotFileWriterTest {

  @Test
  public void test() throws IOException, ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    JBuddyFactory factory = new JBuddyFactory(1000, 1000, f);
    factory.setNumberOfVars(3);
    final LNGBDD bdd = factory.build(p.parse("A => B"));
    BDDDotFileWriter.write("tests/writers/temp/bdd.dot", bdd);
  }


  private void testFiles(final String fileName, final LNGBDD bdd) throws IOException {
    BDDDotFileWriter.write("tests/writers/temp/" + fileName + "_t.dot", bdd);
    final File expected = new File("tests/writers/formulas-dot/" + fileName + "_t.dot");
    final File temp = new File("tests/writers/temp/" + fileName + "_t.dot");
    assertFilesEqual(expected, temp);
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

