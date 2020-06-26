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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_DNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.CacheEntry;

import java.util.Arrays;

/**
 * Test some common formula functionality.
 * @version 2.0.0
 * @since 1.0
 */
public class FormulaTest {

    private enum MyOwnCacheKey implements CacheEntry {
        MYKEY1("My Key 1"),
        MYKEY2("My Key 2");

        private final String description;

        MyOwnCacheKey(final String description) {
            this.description = description;
        }

        @Override
        public String description() {
            return "MyOwnCacheKey{description=" + this.description + "}";
        }
    }

    @Test
    public void testStringContains() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        assertThat(formula.containsVariable("a")).isTrue();
        assertThat(formula.containsVariable("b")).isTrue();
        assertThat(formula.containsVariable("x")).isFalse();
        assertThat(formula.containsVariable("y")).isFalse();
    }

    @Test
    public void testTransformationCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setTransformationCacheEntry(FACTORIZED_CNF, f.or(f.literal("a", false), f.literal("b", false)));
        assertThat(formula.transformationCacheEntry(FACTORIZED_CNF)).isEqualTo(f.or(f.literal("a", false), f.literal("b", false)));
    }

    @Test
    public void testPredicateCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setPredicateCacheEntry(IS_CNF, false);
        formula.setPredicateCacheEntry(IS_DNF, Tristate.UNDEF);
        assertThat(formula.predicateCacheEntry(IS_CNF)).isEqualTo(Tristate.FALSE);
        assertThat(formula.predicateCacheEntry(IS_DNF)).isEqualTo(Tristate.UNDEF);
    }

    @Test
    public void testFunctionCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY1, "key1");
        formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY2, "key2");
        assertThat(MyOwnCacheKey.MYKEY1.description).isEqualTo("My Key 1");
        assertThat(formula.functionCacheEntry(MyOwnCacheKey.MYKEY1)).isEqualTo("key1");
        assertThat(formula.functionCacheEntry(MyOwnCacheKey.MYKEY2)).isEqualTo("key2");
    }

    @Test
    public void testFType() {
        assertThat(FType.valueOf("AND")).isEqualTo(FType.AND);
        assertThat(Arrays.asList(FType.values()).contains(FType.valueOf("PBC"))).isTrue();
        assertThat(FType.values().length).isEqualTo(9);
    }

    @Test
    public void testCType() {
        assertThat(CType.valueOf("EQ")).isEqualTo(CType.EQ);
        assertThat(CType.valueOf("LE")).isEqualTo(CType.LE);
        assertThat(Arrays.asList(CType.values()).contains(CType.valueOf("GT"))).isTrue();
        assertThat(CType.values().length).isEqualTo(5);
    }
}
