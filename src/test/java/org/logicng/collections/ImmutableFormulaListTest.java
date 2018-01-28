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
//  Copyright 2015-2018 Christoph Zengler                                //
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

package org.logicng.collections;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.FType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

/**
 * Unit tests for {@link ImmutableFormulaList}.
 * @version 1.1
 * @since 1.1
 */
public class ImmutableFormulaListTest {
  private FormulaFactory formulaFactory = new FormulaFactory();
  private Variable a = formulaFactory.variable("A");
  private Variable b = formulaFactory.variable("B");

  @Test
  public void testFormula() {
    ImmutableFormulaList ifl = new ImmutableFormulaList(FType.AND, a, b);
    Assert.assertEquals(ifl.formula(formulaFactory), ifl.formula(formulaFactory)); //On purpose to check if both ways in method lead to the same result
    Assert.assertEquals(formulaFactory.and(a, b), ifl.formula(formulaFactory));
  }
}
