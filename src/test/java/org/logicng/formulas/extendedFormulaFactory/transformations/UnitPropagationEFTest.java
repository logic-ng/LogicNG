package org.logicng.formulas.extendedFormulaFactory.transformations;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.extendedFormulaFactory.EF;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.transformations.UnitPropagation;

public class UnitPropagationEFTest {

  private static EF F = new EF();

  private UnitPropagation unitPropagation = new UnitPropagation();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(unitPropagation));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(unitPropagation));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(unitPropagation));
    Assert.assertEquals(F.NA, F.NA.transform(unitPropagation));
  }

  @Test
  public void testNoPropagation() {
    Assert.assertEquals(F.AND1, F.AND1.transform(unitPropagation));
    Assert.assertEquals(F.AND2, F.AND2.transform(unitPropagation));
    Assert.assertEquals(F.OR1, F.OR1.transform(unitPropagation));
    Assert.assertEquals(F.OR2, F.OR2.transform(unitPropagation));
  }

  @Test
  public void testPropagations() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.f.and(F.AND1, F.A).transform(unitPropagation));
    Assert.assertEquals(F.FALSE, F.f.and(F.AND2, F.A).transform(unitPropagation));
    Assert.assertEquals(F.X, F.f.and(F.OR1, F.X).transform(unitPropagation));
    Assert.assertEquals(F.f.and(F.X, F.NY), F.f.and(F.OR2, F.X).transform(unitPropagation));
    Assert.assertEquals(F.A, F.f.or(F.AND1, F.A).transform(unitPropagation));
    Assert.assertEquals(F.f.or(F.A, F.NB), F.f.or(F.AND2, F.A).transform(unitPropagation));
    Assert.assertEquals(F.OR1, F.f.or(F.OR1, F.X).transform(unitPropagation));
    Assert.assertEquals(p.parse("(e | g) & (e | ~g | h) & f & c & d & ~a & b"),
            p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(unitPropagation));
  }
}