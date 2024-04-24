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
//  Copyright 2015-20xx Christoph Zengler                                //
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

package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.SUBFORMULAS;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;

import java.util.LinkedHashSet;

/**
 * A function that computes all sub-nodes of a given formula.  The order of the sub-nodes is bottom-up, i.e. a
 * sub-node only appears in the result when all of its sub-nodes are already listed.
 * @version 2.3.0
 * @since 1.0
 */
public final class SubNodeFunction implements FormulaFunction<LinkedHashSet<Formula>> {

    private static final SubNodeFunction INSTANCE = new SubNodeFunction();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public SubNodeFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static SubNodeFunction get() {
        return INSTANCE;
    }

    @Override
    @SuppressWarnings("unchecked")
    public LinkedHashSet<Formula> apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(SUBFORMULAS);
        if (cached != null) {
            return (LinkedHashSet<Formula>) cached;
        }
        final LinkedHashSet<Formula> result = new LinkedHashSet<>();
        for (final Formula op : formula) {
            if (!result.contains(op)) {
                result.addAll(apply(op, cache));
            }
        }
        result.add(formula);
        if (cache) {
            formula.setFunctionCacheEntry(SUBFORMULAS, result);
        }
        return result;
    }
}
