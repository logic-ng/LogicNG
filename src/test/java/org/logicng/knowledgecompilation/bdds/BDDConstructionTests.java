package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.util.List;

public class BDDConstructionTests {

    FormulaFactory f;
    List<Variable> variables;
    BDDKernel kernel;
    Formula initFormula;
    Formula secondFormula;
    BDD initBdd;
    BDD secondBdd;

    @BeforeEach
    public void init() throws ParserException {
        this.f = new FormulaFactory();
        this.variables = this.f.variables("a", "b", "c", "d", "e", "f", "g");
        this.kernel = new BDDKernel(this.f, this.variables, 1000, 10000);
        this.initFormula = this.f.parse("(a & b) => (c | d & ~e)");
        this.secondFormula = this.f.parse("(g & f) <=> (c | ~a | ~d)");
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
