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

package org.logicng.pseudobooleans;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

/**
 * The adder encoding for pseudo-Boolean constraints to CNF.
 * @version 1.1
 * @since 1.1
 */
public final class PBAdderEncoding extends PBEncoder {

  /**
   * Constructs a new adder encoding.
   * @param f the formula factory
   */
  public PBAdderEncoding(final FormulaFactory f) {
    super(f);
  }

  @Override
  public ImmutableFormulaList build(Literal[] lits, int[] coeffs, int rhs) {

    //    vector<queue<int32_t> > buckets;
    //    result.clear();
    //    vector<int32_t> rhs;
    //
    //    int numberBits = PBLib::ld64(pbconstraint.getLeq()); // number of bits
    //
    //
    //    for ( int iBit = 0; iBit < nb; ++iBit ) {
    //      buckets.push_back ( queue<int32_t>() );
    //      result.push_back ( 0 );
    //      for ( int iVar = 0; iVar < pbconstraint.getWeightedLiterals().size(); ++iVar ) {
    //        if ( ( ( ((int64_t)1) << iBit ) & pbconstraint.getWeightedLiterals()[iVar].weight ) != 0 )
    //          buckets.back().push ( pbconstraint.getWeightedLiterals()[iVar].lit );
    //      }
    //    }
    //
    //
    //    vector<int32_t> kBits;
    //
    //    adderTree ( buckets, result );
    //
    //    numToBits ( kBits, buckets.size(), pbconstraint.getLeq() );
    //
    //    formula.addConditionals(pbconstraint.getConditionals());
    //
    //    if (pbconstraint.getComparator() == PBLib::BOTH)
    //    {
    //      if (!isInc && pbconstraint.getLeq() == pbconstraint.getGeq())
    //        resultIsEqual(result, kBits);
    //      else
    //      {
    //        int32_t true_lit = auxvars.getVariable();
    //        formula.addClause(true_lit);
    //        lessThanOrEqual ( result, kBits , formula);
    //        numToBits ( kBits, buckets.size(), pbconstraint.getGeq() );
    //        assert(kBits.size() == result.size());
    //        for (int i = 0; i < kBits.size(); ++i) // negate everythink
    //        {
    //          kBits[i] = kBits[i] == 0 ? 1 : 0;
    //          result[i] = result[i] == 0 ? true_lit : -result[i];
    //        }
    //        lessThanOrEqual ( result, kBits , formula);
    //        for (int i = 0; i < kBits.size(); ++i) // negate everythink
    //        {
    //          result[i] = -result[i]; // reset result vector
    //        }
    //      }
    //    }
    //    else
    //    {
    //      lessThanOrEqual ( result, kBits , formula);
    //    }
    //
    //    for (int i = 0; i < pbconstraint.getConditionals().size(); ++i)
    //      formula.getConditionals().pop_back();
    //
    return null;
  }
}
