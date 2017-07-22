package org.logicng.explanations.unsatcores;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.propositions.StandardProposition;

import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for {@link UNSATCore}.
 * @version 1.3
 * @since 1.1
 */
public class UNSATCoreTest {

  private final List<StandardProposition> props1;
  private final List<StandardProposition> props2;
  private final UNSATCore core1;
  private final UNSATCore core2;


  public UNSATCoreTest() throws ParserException {
    props1 = new ArrayList<>();
    props2 = new ArrayList<>();
    FormulaFactory f = new FormulaFactory();
    PropositionalParser parser = new PropositionalParser(f);
    props1.add(new StandardProposition(parser.parse("a | b")));
    props1.add(new StandardProposition(parser.parse("~a | b")));
    props1.add(new StandardProposition(parser.parse("a | ~b")));
    props1.add(new StandardProposition(parser.parse("~a | ~b")));
    props2.add(new StandardProposition(parser.parse("a | b")));
    props2.add(new StandardProposition(parser.parse("~a | b")));
    props2.add(new StandardProposition(parser.parse("a | ~b")));
    props2.add(new StandardProposition(parser.parse("~a | ~b")));
    props2.add(new StandardProposition(parser.parse("~a | ~b | c")));
    this.core1 = new UNSATCore<>(props1, true);
    this.core2 = new UNSATCore<>(props2, false);
  }

  @Test
  public void testGetters() {
    Assert.assertEquals(props1, core1.propositions());
    Assert.assertEquals(props2, core2.propositions());
    Assert.assertTrue(core1.isMUS());
    Assert.assertFalse(core2.isMUS());
  }

  @Test
  public void testHashCode() {
    Assert.assertEquals(core1.hashCode(), core1.hashCode());
    Assert.assertEquals(core2.hashCode(), new UNSATCore<>(props2, false).hashCode());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(core1, core1);
    Assert.assertEquals(core1, new UNSATCore<>(props1, true));
    Assert.assertNotEquals(core1, core2);
    Assert.assertNotEquals(core1, new UNSATCore<>(props1, false));
    Assert.assertNotEquals(core1, new UNSATCore<>(props2, true));
    Assert.assertNotEquals(core1, null);
    Assert.assertNotEquals(core1, "String");
  }

  @Test
  public void testToString() {
    final String exp1 = "UNSATCore{isMUS=true, propositions=[StandardProposition{formulas=AND[a | b], description=}, " +
            "StandardProposition{formulas=AND[~a | b], description=}, StandardProposition{formulas=AND[a | ~b], " +
            "description=}, StandardProposition{formulas=AND[~a | ~b], description=}]}";
    final String exp2 = "UNSATCore{isMUS=false, propositions=[StandardProposition{formulas=AND[a | b], description=}, " +
            "StandardProposition{formulas=AND[~a | b], description=}, StandardProposition{formulas=AND[a | ~b], " +
            "description=}, StandardProposition{formulas=AND[~a | ~b], description=}, " +
            "StandardProposition{formulas=AND[~a | ~b | c], description=}]}";
    Assert.assertEquals(exp1, core1.toString());
    Assert.assertEquals(exp2, core2.toString());
  }

}
