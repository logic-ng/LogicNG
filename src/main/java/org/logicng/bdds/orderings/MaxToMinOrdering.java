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

package org.logicng.bdds.orderings;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.functions.VariableProfileFunction;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import static org.logicng.bdds.orderings.MinToMaxOrdering.sortProfileByOccurrence;

/**
 * A BDD variable ordering sorting the variables from maximal to minimal occurrence
 * in the input formula.  If two variables have the same number of occurrences, their
 * ordering according to their DFS ordering will be considered.
 * @version 1.4.0
 * @since 1.4.0
 */
public class MaxToMinOrdering implements VariableOrderingProvider {

  private final VariableProfileFunction profileFunction = new VariableProfileFunction();
  private final DFSOrdering dfsOrdering = new DFSOrdering();

  @Override
  public List<Variable> getOrder(final Formula formula) {
    final Map<Variable, Integer> profile = formula.apply(this.profileFunction);
    final List<Variable> dfs = this.dfsOrdering.getOrder(formula);

    final Comparator<Map.Entry<Variable, Integer>> comparator = new Comparator<Map.Entry<Variable, Integer>>() {
      @Override
      public int compare(final Map.Entry<Variable, Integer> o1, final Map.Entry<Variable, Integer> o2) {
        final int occComp = o1.getValue().compareTo(o2.getValue());
        if (occComp != 0)
          return occComp;
        final int index1 = dfs.indexOf(o1.getKey());
        final int index2 = dfs.indexOf(o2.getKey());
        return index1 - index2;
      }
    };
    final Map<Variable, Integer> sortedProfile = sortProfileByOccurrence(profile, comparator);
    final List<Variable> order = new ArrayList<>(sortedProfile.size());
    for (final Map.Entry<Variable, Integer> entry : sortedProfile.entrySet())
      order.add(entry.getKey());
    return order;
  }
}
