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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.formulas.clearCacheFormulaFactory;

import org.logicng.formulas.CType;
import org.logicng.formulas.ClearCacheFormulaFactory;
import org.logicng.formulas.Constant;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * Formulas for testing.
 * @version 1.2
 * @since 1.2
 */
public class CCF {

  public final ClearCacheFormulaFactory f;

  // Constants
  public final Constant TRUE;
  public final Constant FALSE;

  // Literals
  public final Variable A;
  public final Variable B;
  public final Variable C;
  public final Variable X;
  public final Variable Y;
  public final Literal NA;
  public final Literal NB;
  public final Literal NX;
  public final Literal NY;

  // Disjunctions
  public final Formula OR1;
  public final Formula OR2;
  public final Formula OR3;

  // Conjunctions
  public final Formula AND1;
  public final Formula AND2;
  public final Formula AND3;

  // Negations
  public final Formula NOT1;
  public final Formula NOT2;

  // Implications
  public final Formula IMP1;
  public final Formula IMP2;
  public final Formula IMP3;
  public final Formula IMP4;

  // Equivalences
  public final Formula EQ1;
  public final Formula EQ2;
  public final Formula EQ3;
  public final Formula EQ4;

  // PBCs
  private final Literal[] literals;
  private final int[] coefficients;
  public final Formula PBC1;
  public final Formula PBC2;
  public final Formula PBC3;
  public final Formula PBC4;
  public final Formula PBC5;


  public CCF() {

    f = new ClearCacheFormulaFactory();
    f.save();
    TRUE = f.verum();
    FALSE = f.falsum();
    A = f.variable("a");
    B = f.variable("b");
    C = f.variable("c");
    X = f.variable("x");
    Y = f.variable("y");
    NA = f.literal("a", false);
    NB = f.literal("b", false);
    NX = f.literal("x", false);
    NY = f.literal("y", false);
    OR1 = f.or(X, Y);
    OR2 = f.or(NX, NY);
    OR3 = f.or(f.and(A, B), f.and(NA, NB));
    AND1 = f.and(A, B);
    AND2 = f.and(NA, NB);
    AND3 = f.and(OR1, OR2);
    NOT1 = f.not(AND1);
    NOT2 = f.not(OR1);
    IMP1 = f.implication(A, B);
    IMP2 = f.implication(NA, NB);
    IMP3 = f.implication(AND1, OR1);
    IMP4 = f.implication(f.equivalence(A, B), f.equivalence(NX, NY));
    EQ1 = f.equivalence(A, B);
    EQ2 = f.equivalence(NA, NB);
    EQ3 = f.equivalence(AND1, OR1);
    EQ4 = f.equivalence(IMP1, IMP2);
    literals = new Literal[]{A, B, X};
    coefficients = new int[]{2, -4, 3};
    PBC1 = f.pbc(CType.EQ, 2, literals, coefficients);
    PBC2 = f.pbc(CType.GT, 2, literals, coefficients);
    PBC3 = f.pbc(CType.GE, 2, literals, coefficients);
    PBC4 = f.pbc(CType.LT, 2, literals, coefficients);
    PBC5 = f.pbc(CType.LE, 2, literals, coefficients);
  }
}
