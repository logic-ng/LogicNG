package org.logicng.formulas.printer;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;

/**
 * Unit tests for {@link DefaultStringRepresentation}
 * @version 1.0
 * @since 1.0
 */
public class DefaultStringRepresentationTest {
  private final FormulaStringRepresentation sr = new DefaultStringRepresentation();

  @Test
  public void testDefaultPrinter() {
    Assert.assertEquals("$false", F.f.string(F.FALSE, sr));
    Assert.assertEquals("$true", F.f.string(F.TRUE, sr));
    Assert.assertEquals("x", F.f.string(F.X, sr));
    Assert.assertEquals("~a", F.f.string(F.NA, sr));
    Assert.assertEquals("x1", F.f.string(F.f.variable("x1"), sr));
    Assert.assertEquals("x190", F.f.string(F.f.variable("x190"), sr));
    Assert.assertEquals("x234", F.f.string(F.f.variable("x234"), sr));
    Assert.assertEquals("x567", F.f.string(F.f.variable("x567"), sr));
    Assert.assertEquals("abc8", F.f.string(F.f.variable("abc8"), sr));
    Assert.assertEquals("~a => ~b", F.f.string(F.IMP2, sr));
    Assert.assertEquals("a & b => x | y", F.f.string(F.IMP3, sr));
    Assert.assertEquals("a => b <=> ~a => ~b", F.f.string(F.EQ4, sr));
    Assert.assertEquals("(x | y) & (~x | ~y)", F.f.string(F.AND3, sr));
    Assert.assertEquals("a & b & c & x", F.f.string(F.f.and(F.A, F.B, F.C, F.X), sr));
    Assert.assertEquals("a | b | c | x", F.f.string(F.f.or(F.A, F.B, F.C, F.X), sr));
    Assert.assertEquals("2*a + -4*b + 3*x = 2", F.f.string(F.PBC1, sr));
    Assert.assertEquals("2*a + -4*b + 3*x > 2", F.f.string(F.PBC2, sr));
    Assert.assertEquals("2*a + -4*b + 3*x >= 2", F.f.string(F.PBC3, sr));
    Assert.assertEquals("2*a + -4*b + 3*x < 2", F.f.string(F.PBC4, sr));
    Assert.assertEquals("2*a + -4*b + 3*x <= 2", F.f.string(F.PBC5, sr));
  }

  @Test
  public void testToString(){
    Assert.assertEquals("DefaultStringRepresentation", sr.toString());
  }

}
