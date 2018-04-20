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

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * An implementation of the Quine-McCluskey algorithm for minimizing canonical DNFs.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskey {

  static LinkedHashSet<Term> computePrimeImplicants(final List<Term> terms) {
    SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = generateInitialTermClasses(terms);
    SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = uniteInTermClasses(termsInClasses);
    final LinkedHashSet<Term> primeImplicants = getUnusedTerms(termsInClasses);
    while (!newTermsInClasses.isEmpty()) {
      termsInClasses = newTermsInClasses;
      newTermsInClasses = uniteInTermClasses(termsInClasses);
      primeImplicants.addAll(getUnusedTerms(termsInClasses));
    }
    return primeImplicants;
  }

  static SortedMap<Integer, LinkedHashSet<Term>> uniteInTermClasses(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = new TreeMap<>();
    for (int i = 0; i < termsInClasses.lastKey(); i++) {
      final LinkedHashSet<Term> thisClass = termsInClasses.get(i);
      final LinkedHashSet<Term> otherClass = termsInClasses.get(i + 1);
      if (thisClass != null && otherClass != null) {
        for (final Term thisTerm : thisClass) {
          for (final Term otherTerm : otherClass) {
            final Term unite = thisTerm.unite(otherTerm);
            if (unite != null) {
              thisTerm.setUsed(true);
              otherTerm.setUsed(true);
              LinkedHashSet<Term> foundTerms = newTermsInClasses.get(unite.termClass());
              if (foundTerms == null) {
                foundTerms = new LinkedHashSet<>();
                newTermsInClasses.put(unite.termClass(), foundTerms);
              }
              foundTerms.add(unite);
            }
          }
        }
      }
    }
    return newTermsInClasses;
  }

  private static LinkedHashSet<Term> getUnusedTerms(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final LinkedHashSet<Term> unusedTerms = new LinkedHashSet<>();
    for (final Map.Entry<Integer, LinkedHashSet<Term>> entry : termsInClasses.entrySet())
      for (final Term term : entry.getValue())
        if (!term.isUsed())
          unusedTerms.add(term);
    return unusedTerms;
  }

  static SortedMap<Integer, LinkedHashSet<Term>> generateInitialTermClasses(final List<Term> terms) {
    final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = new TreeMap<>();
    for (final Term term : terms) {
      LinkedHashSet<Term> presentTerms = termsInClasses.get(term.termClass());
      if (presentTerms == null) {
        presentTerms = new LinkedHashSet<>();
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
