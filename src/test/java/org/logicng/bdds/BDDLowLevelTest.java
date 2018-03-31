package org.logicng.bdds;

import org.junit.Before;
import org.junit.Test;
import org.logicng.bdds.jbuddy.BDDKernel;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for some low level BDD kernel methods.
 * @version 1.4.0
 * @since 1.4.0
 */
public class BDDLowLevelTest {

  private BDDFactory factory;

  @Before
  public void init() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser parser = new PropositionalParser(f);
    this.factory = new BDDFactory(1000, 1000, f);
    this.factory.setNumberOfVars(3);
    this.factory.build(f.verum());
    this.factory.build(f.falsum());
    this.factory.build(f.literal("A", true));
    this.factory.build(f.literal("A", false));
    this.factory.build(parser.parse("A => ~B"));
    this.factory.build(parser.parse("A <=> ~B"));
    this.factory.build(parser.parse("A | B | ~C"));
    this.factory.build(parser.parse("A & B & ~C"));
  }

  @Test
  public void testStatistics() {
    final BDDKernel.BDDStatistics statistics = this.factory.underlyingKernel().statistics();
    assertThat(statistics.cachesize()).isEqualTo(1000);
    assertThat(statistics.freenum()).isEqualTo(993);
    assertThat(statistics.gbcollectnum()).isEqualTo(0);
    assertThat(statistics.nodesize()).isEqualTo(1009);
    assertThat(statistics.produced()).isEqualTo(14);
    assertThat(statistics.varnum()).isEqualTo(3);
  }
}
