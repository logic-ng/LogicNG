package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Unit tests for the copy method of the class {@link FormulaFactory}.
 * @version 1.2
 * @since 1.2
 */
public class CopyFormulaFactoryTest {

  @Test
  public void test() {
    FormulaFactory f = new FormulaFactory("Original");
    Set<Formula> formulas = new LinkedHashSet<>();
    formulas.add(f.verum());
    formulas.add(f.falsum());
    Variable A = f.variable("a");
    formulas.add(A);
    Variable B = f.variable("b");
    formulas.add(B);
    Formula C = f.variable("c");
    formulas.add(C);
    Variable X = f.variable("x");
    formulas.add(X);
    Formula Y = f.variable("y");
    formulas.add(Y);
    Formula NA = f.literal("a", false);
    formulas.add(NA);
    Formula NB = f.literal("b", false);
    formulas.add(NB);
    Formula NX = f.literal("x", false);
    formulas.add(NX);
    Formula NY = f.literal("y", false);
    formulas.add(NY);
    Formula OR1 = f.or(X, Y);
    formulas.add(OR1);
    Formula OR2 = f.or(NX, NY);
    formulas.add(OR2);
    Formula OR3 = f.or(f.and(A, B), f.and(NA, NB));
    formulas.add(OR3);
    Formula AND1 = f.and(A, B);
    formulas.add(AND1);
    Formula AND2 = f.and(NA, NB);
    formulas.add(AND2);
    Formula AND3 = f.and(OR1, OR2);
    formulas.add(AND3);
    Formula NOT1 = f.not(AND1);
    formulas.add(NOT1);
    Formula NOT2 = f.not(OR1);
    formulas.add(NOT2);
    Formula IMP1 = f.implication(A, B);
    formulas.add(IMP1);
    Formula IMP2 = f.implication(NA, NB);
    formulas.add(IMP2);
    Formula IMP3 = f.implication(AND1, OR1);
    formulas.add(IMP3);
    Formula IMP4 = f.implication(f.equivalence(A, B), f.equivalence(NX, NY));
    formulas.add(IMP4);
    Formula EQ1 = f.equivalence(A, B);
    formulas.add(EQ1);
    Formula EQ2 = f.equivalence(NA, NB);
    formulas.add(EQ2);
    Formula EQ3 = f.equivalence(AND1, OR1);
    formulas.add(EQ3);
    Formula EQ4 = f.equivalence(IMP1, IMP2);
    formulas.add(EQ4);
    Literal[] literals = new Literal[]{A, B, X};
    int[] coefficients = new int[]{2, -4, 3};
    Formula PBC1 = f.pbc(CType.EQ, 2, literals, coefficients);
    formulas.add(PBC1);
    Formula PBC2 = f.pbc(CType.GT, 2, literals, coefficients);
    formulas.add(PBC2);
    Formula PBC3 = f.pbc(CType.GE, 2, literals, coefficients);
    formulas.add(PBC3);
    Formula PBC4 = f.pbc(CType.LT, 2, literals, coefficients);
    formulas.add(PBC4);
    Formula PBC5 = f.pbc(CType.LE, 2, literals, coefficients);
    formulas.add(PBC5);

    FormulaFactory r = f.copy();
    for (Formula formula : r.getAllFormulas()) {
      checkFormulaFactory(formula);
    }
  }

  private void checkFormulaFactory(Formula formula) {

    Assert.assertTrue(formula.factory().name().contains("copy"));
    if (formula.type() != FType.LITERAL && formula.type() != FType.TRUE && formula.type() != FType.FALSE) {
      for (Formula formula1 : formula) {
        checkFormulaFactory(formula1);
      }
    }
  }
}
