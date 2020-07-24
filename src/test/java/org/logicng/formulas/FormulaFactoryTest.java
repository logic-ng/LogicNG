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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test some basic formula factory functionality.
 * @version 2.0.0
 * @since 1.0
 */
public class FormulaFactoryTest {

    @Test
    public void testPutConfigurationWithInvalidArgument() {
        assertThatThrownBy(() -> {
            final FormulaFactory f = new FormulaFactory();
            f.putConfiguration(FormulaFactoryConfig.builder().build());
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Configurations for the formula factory itself can only be passed in the constructor.");
    }

    @Test
    public void testConstant() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(f.constant(true)).isEqualTo(f.verum());
        assertThat(f.constant(false)).isEqualTo(f.falsum());
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().name("MyFormulaFactory").build());
        f.variable("a");
        f.literal("b", false);
        f.and(f.variable("a"), f.literal("b", false));
        f.or(f.variable("a"), f.literal("b", false), f.variable("x"), f.implication(f.variable("a"), f.variable("x")));
        final String expected = String.format("Name:              MyFormulaFactory%n" +
                "Positive Literals: 3%n" +
                "Negative Literals: 3%n" +
                "Negations:         1%n" +
                "Implications:      1%n" +
                "Equivalences:      0%n" +
                "Conjunctions (2):  1%n" +
                "Conjunctions (3):  0%n" +
                "Conjunctions (4):  0%n" +
                "Conjunctions (>4): 0%n" +
                "Disjunctions (2):  0%n" +
                "Disjunctions (3):  0%n" +
                "Disjunctions (4):  1%n" +
                "Disjunctions (>4): 0%n" +
                "Pseudo Booleans:   0%n" +
                "CCs:               0%n");
        assertThat(f.toString()).isEqualTo(expected);
    }

    @Test
    public void testDefaultName() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(f.name()).isEqualTo("");
    }

    @Test
    public void testConfigurations() {
        final FormulaFactory f = new FormulaFactory();
        final Configuration configMaxSat = MaxSATConfig.builder().build();
        final Configuration configMiniSat = MiniSatConfig.builder().build();
        final Configuration configGlucose = GlucoseConfig.builder().build();
        f.putConfiguration(configMaxSat);
        f.putConfiguration(configMiniSat);
        f.putConfiguration(configGlucose);
        assertThat(f.configurationFor(ConfigurationType.MAXSAT)).isEqualTo(configMaxSat);
        assertThat(f.configurationFor(ConfigurationType.MINISAT)).isEqualTo(configMiniSat);
        assertThat(f.configurationFor(ConfigurationType.GLUCOSE)).isEqualTo(configGlucose);
    }

    @Test
    public void testGeneratedVariables() {
        FormulaFactory f = new FormulaFactory();
        Variable ccVar = f.newCCVariable();
        Variable cnfVar = f.newCNFVariable();
        Variable pbVar = f.newPBVariable();
        Variable var = f.variable("x");
        assertThat(f.isGeneratedVariable(ccVar)).isTrue();
        assertThat(f.isGeneratedVariable(cnfVar)).isTrue();
        assertThat(f.isGeneratedVariable(pbVar)).isTrue();
        assertThat(f.isGeneratedVariable(var)).isFalse();
        assertThat(ccVar.name()).isEqualTo("@RESERVED_CC_0");
        assertThat(pbVar.name()).isEqualTo("@RESERVED_PB_0");
        assertThat(cnfVar.name()).isEqualTo("@RESERVED_CNF_0");

        f = new FormulaFactory(FormulaFactoryConfig.builder().name("f").build());
        ccVar = f.newCCVariable();
        cnfVar = f.newCNFVariable();
        pbVar = f.newPBVariable();
        var = f.variable("x");
        assertThat(f.isGeneratedVariable(ccVar)).isTrue();
        assertThat(f.isGeneratedVariable(cnfVar)).isTrue();
        assertThat(f.isGeneratedVariable(pbVar)).isTrue();
        assertThat(f.isGeneratedVariable(var)).isFalse();
        assertThat(ccVar.name()).isEqualTo("@RESERVED_CC_f_0");
        assertThat(pbVar.name()).isEqualTo("@RESERVED_PB_f_0");
        assertThat(cnfVar.name()).isEqualTo("@RESERVED_CNF_f_0");
    }

    @Test
    public void testCNF() {
        final FormulaFactory f = new FormulaFactory();
        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Variable d = f.variable("D");
        final Formula clause1 = f.or(a, b);
        final Formula clause2 = f.or(c, d.negate());
        final Formula nClause1 = f.implication(a, c);

        final List<Formula> clauses = new ArrayList<>();
        clauses.add(clause1);
        clauses.add(clause2);

        final List<Formula> nClauses = new ArrayList<>();
        nClauses.add(clause1);
        nClauses.add(clause2);
        nClauses.add(nClause1);

        final Formula cnf = f.cnf(clauses);
        final Formula nCnf = f.cnf(nClauses);
        assertThat(cnf.cnf()).isEqualTo(cnf);
        assertThat(nCnf.cnf()).isNotEqualTo(nCnf);
        assertThat(f.cnf(Collections.emptyList())).isEqualTo(f.verum());
    }

    @Test
    public void testImportFormula() throws ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory F").build());
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory G").build());
        final PropositionalParser pf = new PropositionalParser(f);
        final String formula = "x1 & x2 & ~x3 => (x4 | (x5 <=> ~x1))";
        final Formula ff = pf.parse(formula);
        final Formula fg = g.importFormula(ff);
        assertThat(fg).isEqualTo(ff);
        assertThat(ff.factory()).isSameAs(f);
        assertThat(fg.factory()).isSameAs(g);
        assertThat(f.statistics()).isEqualToComparingOnlyGivenFields(g.statistics(), "positiveLiterals",
                "negativeLiterals", "negations", "implications", "equivalences", "conjunctions2", "conjunctions3",
                "conjunctions4", "conjunctionsN", "disjunctions2", "disjunctions3", "disjunctions4");
        for (final Literal litF : ff.literals()) {
            assertThat(litF.factory()).isSameAs(f);
        }
        for (final Literal litG : fg.literals()) {
            assertThat(litG.factory()).isSameAs(g);
        }
    }

    @Test
    public void testStatistics() throws ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory F").build());
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().name("Factory F").build());
        final FormulaFactory.FormulaFactoryStatistics statisticsF1 = f.statistics();
        final FormulaFactory.FormulaFactoryStatistics statisticsG = g.statistics();

        assertThat(statisticsF1.name()).isEqualTo("Factory F");
        assertThat(statisticsF1.positiveLiterals()).isEqualTo(0);
        assertThat(statisticsF1.negativeLiterals()).isEqualTo(0);
        assertThat(statisticsF1.negations()).isEqualTo(0);
        assertThat(statisticsF1.implications()).isEqualTo(0);
        assertThat(statisticsF1.equivalences()).isEqualTo(0);
        assertThat(statisticsF1.conjunctions2()).isEqualTo(0);
        assertThat(statisticsF1.conjunctions3()).isEqualTo(0);
        assertThat(statisticsF1.conjunctions4()).isEqualTo(0);
        assertThat(statisticsF1.conjunctionsN()).isEqualTo(0);
        assertThat(statisticsF1.disjunctions2()).isEqualTo(0);
        assertThat(statisticsF1.disjunctions3()).isEqualTo(0);
        assertThat(statisticsF1.disjunctions4()).isEqualTo(0);
        assertThat(statisticsF1.disjunctionsN()).isEqualTo(0);
        assertThat(statisticsF1.pbcs()).isEqualTo(0);
        assertThat(statisticsF1.ccs()).isEqualTo(0);
        assertThat(statisticsF1.ccCounter()).isEqualTo(0);
        assertThat(statisticsF1.pbCounter()).isEqualTo(0);
        assertThat(statisticsF1.cnfCounter()).isEqualTo(0);
        assertThat(statisticsF1.formulas()).isEqualTo(0);

        assertThat(statisticsF1.equals(statisticsF1)).isTrue();
        assertThat(statisticsF1).isEqualTo(statisticsF1);
        assertThat(statisticsF1.equals(42)).isFalse();
        assertThat(statisticsF1).isEqualTo(statisticsG);
        assertThat(statisticsG).isEqualTo(statisticsF1);
        assertThat(statisticsF1.hashCode()).isEqualTo(statisticsG.hashCode());

        assertThat(statisticsF1.toString()).isEqualTo(
                "FormulaFactoryStatistics{"
                        + "name='Factory F'"
                        + ", positiveLiterals=0"
                        + ", negativeLiterals=0"
                        + ", negations=0"
                        + ", implications=0"
                        + ", equivalences=0"
                        + ", conjunctions2=0"
                        + ", conjunctions3=0"
                        + ", conjunctions4=0"
                        + ", conjunctionsN=0"
                        + ", disjunctions2=0"
                        + ", disjunctions3=0"
                        + ", disjunctions4=0"
                        + ", disjunctionsN=0"
                        + ", pbcs=0"
                        + ", ccs=0"
                        + ", ccCounter=0"
                        + ", pbCounter=0"
                        + ", cnfCounter=0"
                        + '}');

        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Variable d = f.variable("D");
        final Variable e = f.variable("E");
        final And and = (And) f.and(a, b);
        final And and3 = (And) f.and(a, b, c);
        final And and4 = (And) f.and(a, b, c, d);
        final And and5 = (And) f.and(a, b, c, d, e);
        final Or or2 = (Or) f.or(a, b);
        final Or or3 = (Or) f.or(a, b, c);
        final Or or4 = (Or) f.or(a, b, c, d);
        final Or or5 = (Or) f.or(a, b, c, d, e);
        assertThat(f.posLiterals).containsValue(b);
        assertThat(f.ands2).containsValue(and);
        assertThat(f.ands3).containsValue(and3);
        assertThat(f.ands4).containsValue(and4);
        assertThat(f.andsN).containsValue(and5);
        assertThat(f.ors2).containsValue(or2);
        assertThat(f.ors3).containsValue(or3);
        assertThat(f.ors4).containsValue(or4);
        assertThat(f.orsN).containsValue(or5);

        final FormulaFactory.FormulaFactoryStatistics statisticsF2 = f.statistics();

        assertThat(statisticsF2.name()).isEqualTo("Factory F");
        assertThat(statisticsF2.positiveLiterals()).isEqualTo(5);
        assertThat(statisticsF2.negativeLiterals()).isEqualTo(5);
        assertThat(statisticsF2.negations()).isEqualTo(0);
        assertThat(statisticsF2.implications()).isEqualTo(0);
        assertThat(statisticsF2.equivalences()).isEqualTo(0);
        assertThat(statisticsF2.conjunctions2()).isEqualTo(1);
        assertThat(statisticsF2.conjunctions3()).isEqualTo(1);
        assertThat(statisticsF2.conjunctions4()).isEqualTo(1);
        assertThat(statisticsF2.conjunctionsN()).isEqualTo(1);
        assertThat(statisticsF2.disjunctions2()).isEqualTo(1);
        assertThat(statisticsF2.disjunctions3()).isEqualTo(1);
        assertThat(statisticsF2.disjunctions4()).isEqualTo(1);
        assertThat(statisticsF2.disjunctionsN()).isEqualTo(1);
        assertThat(statisticsF2.pbcs()).isEqualTo(0);
        assertThat(statisticsF2.ccs()).isEqualTo(0);
        assertThat(statisticsF2.ccCounter()).isEqualTo(0);
        assertThat(statisticsF2.pbCounter()).isEqualTo(0);
        assertThat(statisticsF2.cnfCounter()).isEqualTo(0);
        assertThat(statisticsF2.formulas()).isEqualTo(18);

        assertThat(statisticsF2).isNotEqualTo(statisticsF1);
        assertThat(statisticsF1).isNotEqualTo(statisticsF2);

        assertThat(statisticsF2.toString()).isEqualTo(
                "FormulaFactoryStatistics{"
                        + "name='Factory F'"
                        + ", positiveLiterals=5"
                        + ", negativeLiterals=5"
                        + ", negations=0"
                        + ", implications=0"
                        + ", equivalences=0"
                        + ", conjunctions2=1"
                        + ", conjunctions3=1"
                        + ", conjunctions4=1"
                        + ", conjunctionsN=1"
                        + ", disjunctions2=1"
                        + ", disjunctions3=1"
                        + ", disjunctions4=1"
                        + ", disjunctionsN=1"
                        + ", pbcs=0"
                        + ", ccs=0"
                        + ", ccCounter=0"
                        + ", pbCounter=0"
                        + ", cnfCounter=0"
                        + '}');
    }
}
