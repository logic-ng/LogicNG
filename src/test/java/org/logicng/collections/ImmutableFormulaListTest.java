package org.logicng.collections;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

/**
 * Unit tests for {@link ImmutableFormulaList}.
 */
public class ImmutableFormulaListTest {
  private FormulaFactory formulaFactory = new FormulaFactory();
  private Variable a = formulaFactory.variable("A");
  private Variable b = formulaFactory.variable("B");

  @Test
  public void testFormula(){
    ImmutableFormulaList ifl = new ImmutableFormulaList(FType.AND,a,b);
    Assert.assertEquals(ifl.formula(formulaFactory),ifl.formula(formulaFactory)); //On purpose to check if both ways in method lead to the same result
    Assert.assertEquals(formulaFactory.and(a,b),ifl.formula(formulaFactory));
  }
}
