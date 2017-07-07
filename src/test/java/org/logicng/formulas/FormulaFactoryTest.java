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
//  Copyright 2015-2017 Christoph Zengler                                //
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

package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * Test some basic formula factory functionality.
 * @version 1.1
 * @since 1.0
 */
public class FormulaFactoryTest {

  @Test
  public void testToString() {
    final FormulaFactory f = new FormulaFactory("MyFormulaFactory");
    f.variable("a");
    f.literal("b", false);
    f.and(f.variable("a"), f.literal("b", false));
    f.or(f.variable("a"), f.literal("b", false), f.variable("x"), f.implication(f.variable("a"), f.variable("x")));
    final String expected = "Name:              MyFormulaFactory\n" +
            "Positive Literals: 3\n" +
            "Negative Literals: 3\n" +
            "Negations:         1\n" +
            "Implications:      1\n" +
            "Equivalences:      0\n" +
            "Conjunctions (2):  1\n" +
            "Conjunctions (3):  0\n" +
            "Conjunctions (4):  0\n" +
            "Conjunctions (>4): 0\n" +
            "Disjunctions (2):  0\n" +
            "Disjunctions (3):  0\n" +
            "Disjunctions (4):  1\n" +
            "Disjunctions (>4): 0\n";
    Assert.assertEquals(expected, f.toString());
  }

  @Test
  public void testDefaultName() {
    final FormulaFactory f = new FormulaFactory();
    Assert.assertEquals("", f.name());
  }

  @Test
  public void testConfigurations() {
    final FormulaFactory f = new FormulaFactory();
    final Configuration configMaxSat = new MaxSATConfig.Builder().build();
    final Configuration configMiniSat = new MiniSatConfig.Builder().build();
    final Configuration configGlucose = new GlucoseConfig.Builder().build();
    f.putConfiguration(configMaxSat);
    f.putConfiguration(configMiniSat);
    f.putConfiguration(configGlucose);
    Assert.assertEquals(configMaxSat, f.configurationFor(ConfigurationType.MAXSAT));
    Assert.assertEquals(configMiniSat, f.configurationFor(ConfigurationType.MINISAT));
    Assert.assertEquals(configGlucose, f.configurationFor(ConfigurationType.GLUCOSE));
    Assert.assertNull(f.configurationFor(ConfigurationType.CLEANELING));
  }

  @Test
  public void testGeneratedVariables() {
    FormulaFactory f = new FormulaFactory();
    Variable ccVar = f.newCCVariable();
    Variable cnfVar = f.newCNFVariable();
    Variable pbVar = f.newPBVariable();
    Variable var = f.variable("x");
    Assert.assertTrue(f.isGeneratedVariable(ccVar));
    Assert.assertTrue(f.isGeneratedVariable(cnfVar));
    Assert.assertTrue(f.isGeneratedVariable(pbVar));
    Assert.assertFalse(f.isGeneratedVariable(var));
    Assert.assertEquals("@RESERVED_CC_0", ccVar.name());
    Assert.assertEquals("@RESERVED_PB_0", pbVar.name());
    Assert.assertEquals("@RESERVED_CNF_0", cnfVar.name());

    f = new FormulaFactory("f");
    ccVar = f.newCCVariable();
    cnfVar = f.newCNFVariable();
    pbVar = f.newPBVariable();
    var = f.variable("x");
    Assert.assertTrue(f.isGeneratedVariable(ccVar));
    Assert.assertTrue(f.isGeneratedVariable(cnfVar));
    Assert.assertTrue(f.isGeneratedVariable(pbVar));
    Assert.assertFalse(f.isGeneratedVariable(var));
    Assert.assertEquals("@RESERVED_CC_f_0", ccVar.name());
    Assert.assertEquals("@RESERVED_PB_f_0", pbVar.name());
    Assert.assertEquals("@RESERVED_CNF_f_0", cnfVar.name());
  }

  @Test
  public void testCNF() {
    FormulaFactory f = new FormulaFactory();
    Variable a = f.variable("A");
    Variable b = f.variable("B");
    Variable c = f.variable("C");
    Variable d = f.variable("D");
    Formula clause1 = f.or(a, b);
    Formula clause2 = f.or(c, d.negate());
    Formula nClause1 = f.implication(a, c);

    List<Formula> clauses = new ArrayList<>();
    clauses.add(clause1);
    clauses.add(clause2);

    List<Formula> nClauses = new ArrayList<>();
    nClauses.add(clause1);
    nClauses.add(clause2);
    nClauses.add(nClause1);

    Formula cnf = f.cnf(clauses);
    Formula nCnf = f.cnf(nClauses);
    Assert.assertEquals(cnf, cnf.cnf());
    Assert.assertNotEquals(nCnf, nCnf.cnf());
  }
}
