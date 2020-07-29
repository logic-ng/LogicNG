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

package org.logicng.knowledgecompilation.dnnf;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.io.parsers.FormulaParser;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.io.readers.DimacsReader;
import org.logicng.io.readers.FormulaReader;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.orderings.ForceOrdering;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfModelCountFunction;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.transformations.cnf.CNFFactorization;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Unit Tests for the class {@link DnnfCompiler}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DnnfCompilerTest {

    private final FormulaFactory f = new FormulaFactory();
    private final FormulaParser parser = new PseudoBooleanParser(this.f);

    @Test
    public void testTrivialFormulas() throws ParserException {
        testFormula(this.parser.parse("$true"), true);
        testFormula(this.parser.parse("$false"), true);
        testFormula(this.parser.parse("a"), true);
        testFormula(this.parser.parse("~a"), true);
        testFormula(this.parser.parse("a & b"), true);
        testFormula(this.parser.parse("a | b"), true);
        testFormula(this.parser.parse("a => b"), true);
        testFormula(this.parser.parse("a <=> b"), true);
        testFormula(this.parser.parse("a | b | c"), true);
        testFormula(this.parser.parse("a & b & c"), true);
        testFormula(this.parser.parse("f & ((~b | c) <=> ~a & ~c)"), true);
        testFormula(this.parser.parse("a | ((b & ~c) | (c & (~d | ~a & b)) & e)"), true);
        testFormula(this.parser.parse("a + b + c + d <= 1"), true);
        testFormula(this.parser.parse("a + b + c + d <= 3"), true);
        testFormula(this.parser.parse("2*a + 3*b + -2*c + d < 5"), true);
        testFormula(this.parser.parse("2*a + 3*b + -2*c + d >= 5"), true);
        testFormula(this.parser.parse("~a & (~a | b | c | d)"), true);
    }

    @Test
    public void testLargeFormulas() throws IOException {
        final FormulaFactory f = new FormulaFactory();
        List<Formula> dimacs = DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_1.cnf", f);
        testFormula(f.cnf(dimacs), true);
        dimacs = DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_2.cnf", f);
        testFormula(f.cnf(dimacs), true);
        dimacs = DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_3.cnf", f);
        testFormula(f.cnf(dimacs), true);
        dimacs = DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_4.cnf", f);
        testFormula(f.cnf(dimacs), true);
        dimacs = DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_5.cnf", f);
        testFormula(f.cnf(dimacs), true);
    }

    @Test
    public void testDnnfProperties() throws ParserException {
        final Dnnf dnnf = new DnnfFactory().compile(this.parser.parse("a | ((b & ~c) | (c & (~d | ~a & b)) & e)"));
        assertThat(dnnf.getOriginalVariables()).extracting(Variable::name).containsExactlyInAnyOrder("a", "b", "c", "d", "e");
    }

    @Test
    @LongRunningTag
    public void testAllSmallFormulas() throws IOException, ParserException {
        final Formula formulas = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", this.f);
        formulas.stream().forEach(f -> testFormula(f, false));
    }

    @Test
    @LongRunningTag
    public void testLargeFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.PURE).build());
        final Formula parsed = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/formula1.txt", f);
        final DnnfFactory dnnfFactory = new DnnfFactory();
        Dnnf dnnf = dnnfFactory.compile(parsed);
        final BigInteger dnnfCount = dnnf.execute(DnnfModelCountFunction.get());
        final List<Formula> formulas = new ArrayList<>();
        final List<Formula> originalFormulas = new ArrayList<>();
        for (final Formula formula : parsed) {
            originalFormulas.add(formula);
            if (formula instanceof PBConstraint) {
                formulas.add(formula);
            } else {
                formulas.add(formula.transform(new CNFFactorization()));
            }
        }
        final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromCnf(formulas);
        final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
        final List<List<Formula>> split = ConnectedComponentsComputation.splitFormulasByComponent(originalFormulas, ccs);
        BigInteger multipliedCount = BigInteger.ONE;
        for (final List<Formula> component : split) {
            dnnf = dnnfFactory.compile(f.and(component));
            multipliedCount = multipliedCount.multiply(dnnf.execute(DnnfModelCountFunction.get()));
        }
        assertThat(dnnfCount).isEqualTo(multipliedCount);
    }

    private void testFormula(final Formula formula, final boolean withEquivalence) {
        final DnnfFactory dnnfFactory = new DnnfFactory();
        final Dnnf dnnf = dnnfFactory.compile(formula);
        final BigInteger dnnfCount = dnnf.execute(DnnfModelCountFunction.get());
        if (withEquivalence) {
            final Formula equivalence = formula.factory().equivalence(formula, dnnf.formula());
            assertThat(equivalence.holds(new TautologyPredicate(formula.factory()))).isTrue();
        }
        final BigInteger bddCount = countWithBdd(formula);
        assertThat(dnnfCount).isEqualTo(bddCount);
    }

    private BigInteger countWithBdd(final Formula formula) {
        if (formula.type() == FType.TRUE) {
            return BigInteger.ONE;
        } else if (formula.type() == FType.FALSE) {
            return BigInteger.ZERO;
        }
        final BDDKernel kernel = new BDDKernel(formula.factory(), new ForceOrdering().getOrder(formula), 100000, 1000000);
        final BDD bdd = BDDFactory.build(formula, kernel);
        return bdd.modelCount();
    }
}
