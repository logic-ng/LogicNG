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

package org.logicng.transformations.qmc;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * An implementation of the Quine-McCluskey algorithm for minimizing canonical DNFs.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskey {

  void computePrimeImplicants(final List<Term> terms) {
    final SortedMap<Integer, List<Term>> termsInClasses = generateInitialTermClasses(terms);
  }

  static SortedMap<Integer, List<Term>> generateInitialTermClasses(final List<Term> terms) {
    final SortedMap<Integer, List<Term>> termsInClasses = new TreeMap<>();
    for (final Term term : terms) {
      List<Term> presentTerms = termsInClasses.get(term.termClass());
      if (presentTerms == null) {
        presentTerms = new ArrayList<>();
        termsInClasses.put(term.termClass(), presentTerms);
      }
      presentTerms.add(term);
    }
    return termsInClasses;
  }

  static Term convertToTerm(final List<Literal> minterm, final FormulaFactory f) {
    final Tristate[] bits = new Tristate[minterm.size()];
    for (int i = 0; i < minterm.size(); i++)
      bits[i] = Tristate.fromBool(minterm.get(i).phase());
    return new Term(bits, Collections.singletonList(f.and(minterm)));
  }

}
