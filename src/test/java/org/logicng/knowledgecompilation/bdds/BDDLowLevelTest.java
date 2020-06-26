package org.logicng.knowledgecompilation.bdds;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.knowledgecompilation.bdds.datastructures.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

/**
 * Unit tests for some low level BDD kernel methods.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDLowLevelTest {

    private BDD bdd;

    @BeforeEach
    public void init() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final BDDKernel kernel = new BDDKernel(f, 3, 1000, 1000);
        BDDFactory.build(f.verum(), kernel);
        BDDFactory.build(f.falsum(), kernel);
        BDDFactory.build(f.literal("A", true), kernel);
        BDDFactory.build(f.literal("A", false), kernel);
        BDDFactory.build(parser.parse("A => ~B"), kernel);
        BDDFactory.build(parser.parse("A <=> ~B"), kernel);
        BDDFactory.build(parser.parse("A | B | ~C"), kernel);
        this.bdd = BDDFactory.build(parser.parse("A & B & ~C"), kernel);
    }

    @Test
    public void testStatistics() {
        final BDDKernel.BDDStatistics statistics = this.bdd.underlyingKernel().statistics();
        assertThat(statistics.cachesize()).isEqualTo(1000);
        assertThat(statistics.freenum()).isEqualTo(993);
        assertThat(statistics.gbcollectnum()).isEqualTo(0);
        assertThat(statistics.nodesize()).isEqualTo(1009);
        assertThat(statistics.produced()).isEqualTo(14);
        assertThat(statistics.varnum()).isEqualTo(3);
        assertThat(statistics.toString()).isEqualTo("BDDStatistics{produced nodes=14, allocated nodes=1009, free nodes=993, variables=3, cache size=1000, garbage collections=0}");
    }

    @Test
    public void kernelTests() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThat(kernel.ithVar(0)).isEqualTo(2);
        assertThat(kernel.nithVar(0)).isEqualTo(3);
        assertThat(kernel.bddVar(2)).isEqualTo(0);
        assertThat(kernel.bddLow(2)).isEqualTo(0);
        assertThat(kernel.bddHigh(2)).isEqualTo(1);

    }

    @Test
    public void illegalKernel1() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThatThrownBy(() -> kernel.ithVar(-1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void illegalKernel2() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThatThrownBy(() -> kernel.nithVar(-1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void illegalKernel3() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThatThrownBy(() -> kernel.bddVar(1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void illegalKernel4() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThatThrownBy(() -> kernel.bddLow(1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void illegalKernel5() {
        final BDDKernel kernel = this.bdd.underlyingKernel();
        assertThatThrownBy(() -> kernel.bddHigh(1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testSetNegativeVarNum() {
        assertThatThrownBy(() -> new BDDKernel(new FormulaFactory(), -4, 100, 100)).isInstanceOf(IllegalArgumentException.class);
    }
}
