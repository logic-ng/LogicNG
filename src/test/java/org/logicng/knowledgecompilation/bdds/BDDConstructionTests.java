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

package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.TestWithExampleFormulas.parse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.util.List;

/**
 * Unit tests for the BDD construction methods
 * @version 2.4.0
 * @since 2.4.0
 */
public class BDDConstructionTests {

    FormulaFactory f;
    List<Variable> variables;
    BDDKernel kernel;
    Formula initFormula;
    Formula secondFormula;
    BDD initBdd;
    BDD secondBdd;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.variables = this.f.variables("a", "b", "c", "d", "e", "f", "g");
        this.kernel = new BDDKernel(this.f, this.variables, 1000, 10000);
        this.initFormula = parse(this.f, "(a & b) => (c | d & ~e)");
        this.secondFormula = parse(this.f, "(g & f) <=> (c | ~a | ~d)");
        this.initBdd = BDDFactory.build(this.initFormula, this.kernel);
        this.secondBdd = BDDFactory.build(this.secondFormula, this.kernel);
    }

    @Test
    public void testNegation() {
        final BDD negation = this.initBdd.negate();
        final BDD expected = BDDFactory.build(this.initFormula.negate(), this.kernel);
        assertThat(negation).isEqualTo(expected);
    }

    @Test
    public void testImplies() {
        final BDD implication = this.initBdd.implies(this.secondBdd);
        final BDD expected = BDDFactory.build(this.f.implication(this.initFormula, this.secondFormula), this.kernel);
        assertThat(implication).isEqualTo(expected);
    }

    @Test
    public void testIsImplied() {
        final BDD implication = this.initBdd.impliedBy(this.secondBdd);
        final BDD expected = BDDFactory.build(this.f.implication(this.secondFormula, this.initFormula), this.kernel);
        assertThat(implication).isEqualTo(expected);
    }

    @Test
    public void testEquivalence() {
        final BDD equivalence = this.initBdd.equivalence(this.secondBdd);
        final BDD expected = BDDFactory.build(this.f.equivalence(this.secondFormula, this.initFormula), this.kernel);
        assertThat(equivalence).isEqualTo(expected);
    }

    @Test
    public void testAnd() {
        final BDD and = this.initBdd.and(this.secondBdd);
        final BDD expected = BDDFactory.build(this.f.and(this.secondFormula, this.initFormula), this.kernel);
        assertThat(and).isEqualTo(expected);
    }

    @Test
    public void testOr() {
        final BDD or = this.initBdd.or(this.secondBdd);
        final BDD expected = BDDFactory.build(this.f.or(this.secondFormula, this.initFormula), this.kernel);
        assertThat(or).isEqualTo(expected);
    }
}
