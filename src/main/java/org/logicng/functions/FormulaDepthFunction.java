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

import static org.logicng.formulas.cache.FunctionCacheEntry.DEPTH;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;

/**
 * A function that computes the depth of a formula. The depth of an atomic formula
 * is defined as 0, all other operators increase the depth by 1.
 * @version 2.0
 * @since 2.0
 */
public final class FormulaDepthFunction implements FormulaFunction<Integer> {

    @Override
    public Integer apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(DEPTH);
        if (cached != null) {
            return (Integer) cached;
        }
        final int result;
        if (formula.isAtomicFormula()) {
            result = 0;
        } else {
            int maxDepth = 0;
            for (final Formula op : formula) {
                maxDepth = Math.max(maxDepth, apply(op, cache));
            }
            result = maxDepth + 1;
        }
        if (cache) {
            formula.setFunctionCacheEntry(DEPTH, result);
        }
        return result;
    }
}
