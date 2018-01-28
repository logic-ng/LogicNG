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

package org.logicng.formulas;

/**
 * Formulas for testing.
 * @version 1.0
 * @since 1.0
 */
public class F {

  public static final FormulaFactory f = new FormulaFactory();
  public static final FormulaFactory g = new FormulaFactory();

  // Constants
  public static final Constant TRUE = f.verum();
  public static final Constant FALSE = f.falsum();

  // Literals
  public static final Variable A = f.variable("a");
  public static final Variable B = f.variable("b");
  public static final Variable C = f.variable("c");
  public static final Variable X = f.variable("x");
  public static final Variable Y = f.variable("y");
  public static final Literal NA = f.literal("a", false);
  public static final Literal NB = f.literal("b", false);
  public static final Literal NX = f.literal("x", false);
  public static final Literal NY = f.literal("y", false);

  // Disjunctions
  public static final Formula OR1 = f.or(X, Y);
  public static final Formula OR2 = f.or(NX, NY);
  public static final Formula OR3 = f.or(f.and(A, B), f.and(NA, NB));

  // Conjunctions
  public static final Formula AND1 = f.and(A, B);
  public static final Formula AND2 = f.and(NA, NB);
  public static final Formula AND3 = f.and(OR1, OR2);

  // Negations
  public static final Formula NOT1 = f.not(AND1);
  public static final Formula NOT2 = f.not(OR1);

  // Implications
  public static final Formula IMP1 = f.implication(A, B);
  public static final Formula IMP2 = f.implication(NA, NB);
  public static final Formula IMP3 = f.implication(AND1, OR1);
  public static final Formula IMP4 = f.implication(f.equivalence(A, B), f.equivalence(NX, NY));

  // Equivalences
  public static final Formula EQ1 = f.equivalence(A, B);
  public static final Formula EQ2 = f.equivalence(NA, NB);
  public static final Formula EQ3 = f.equivalence(AND1, OR1);
  public static final Formula EQ4 = f.equivalence(IMP1, IMP2);

  // PBCs
  private static final Literal[] literals = new Literal[]{A, B, X};
  private static final int[] coefficients = new int[]{2, -4, 3};
  public static final Formula PBC1 = f.pbc(CType.EQ, 2, literals, coefficients);
  public static final Formula PBC2 = f.pbc(CType.GT, 2, literals, coefficients);
  public static final Formula PBC3 = f.pbc(CType.GE, 2, literals, coefficients);
  public static final Formula PBC4 = f.pbc(CType.LT, 2, literals, coefficients);
  public static final Formula PBC5 = f.pbc(CType.LE, 2, literals, coefficients);
}
