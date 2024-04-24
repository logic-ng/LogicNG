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

package org.logicng.knowledgecompilation.dnnf.functions;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.formulas.cache.FunctionCacheEntry;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A DNNF function which counts models.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class DnnfModelCountFunction implements DnnfFunction<BigInteger> {

    private static final DnnfModelCountFunction INSTANCE = new DnnfModelCountFunction();

    private DnnfModelCountFunction() {
        // intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static DnnfModelCountFunction get() {
        return INSTANCE;
    }

    @Override
    public BigInteger apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        final Object cached = formula.functionCacheEntry(FunctionCacheEntry.DNNF_MODELCOUNT);
        final BigInteger result;
        if (cached != null) {
            result = (BigInteger) cached;
        } else {
            result = count(formula, new HashMap<>());
        }
        formula.setFunctionCacheEntry(FunctionCacheEntry.DNNF_MODELCOUNT, result);
        final SortedSet<Variable> dontCareVariables = new TreeSet<>();
        final SortedSet<Variable> dnnfVariables = formula.variables();
        for (final Variable originalVariable : originalVariables) {
            if (!dnnfVariables.contains(originalVariable)) {
                dontCareVariables.add(originalVariable);
            }
        }
        final BigInteger factor = BigInteger.valueOf(2).pow(dontCareVariables.size());
        return result.multiply(factor);
    }

    private BigInteger count(final Formula dnnf, final Map<Formula, BigInteger> internalCache) {
        BigInteger c = internalCache.get(dnnf);
        if (c == null) {
            switch (dnnf.type()) {
                case LITERAL:
                case TRUE:
                    c = BigInteger.ONE;
                    break;
                case AND:
                    c = BigInteger.ONE;
                    for (final Formula op : dnnf) {
                        c = c.multiply(count(op, internalCache));
                    }
                    break;
                case OR:
                    final int allVariables = dnnf.variables().size();
                    c = BigInteger.ZERO;
                    for (final Formula op : dnnf) {
                        final BigInteger opCount = count(op, internalCache);
                        final BigInteger factor = BigInteger.valueOf(2L).pow(allVariables - op.variables().size());
                        c = c.add(opCount.multiply(factor));
                    }
                    break;
                case FALSE:
                    c = BigInteger.ZERO;
                    break;
            }
            internalCache.put(dnnf, c);
        }
        return c;
    }
}
