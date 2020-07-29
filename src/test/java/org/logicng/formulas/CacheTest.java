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

import org.junit.jupiter.api.Test;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the package formulas.cache.
 * @version 2.0.0
 * @since 1.1
 */
public class CacheTest {

    @Test
    public void testDescription() {
        assertThat(TransformationCacheEntry.AIG.description()).isEqualTo("TransformationCacheEntry{description=and-inverter graph}");
        assertThat(TransformationCacheEntry.NNF.description()).isEqualTo("TransformationCacheEntry{description=negation normal form}");
        assertThat(TransformationCacheEntry.PLAISTED_GREENBAUM_POS.description()).isEqualTo("TransformationCacheEntry{description=Plaisted & Greenbaum conjunctive normal form (positive polarity)}");
        assertThat(TransformationCacheEntry.TSEITIN.description()).isEqualTo("TransformationCacheEntry{description=Tseitin conjunctive normal form}");
        assertThat(TransformationCacheEntry.FACTORIZED_CNF.description()).isEqualTo("TransformationCacheEntry{description=factorized conjunctive normal form}");

        assertThat(PredicateCacheEntry.IS_AIG.description()).isEqualTo("PredicateCacheEntry{description=and-inverter graph}");
        assertThat(PredicateCacheEntry.IS_TAUTOLOGY.description()).isEqualTo("PredicateCacheEntry{description=tautology}");
        assertThat(PredicateCacheEntry.IS_CNF.description()).isEqualTo("PredicateCacheEntry{description=conjunctive normal form}");

        assertThat(FunctionCacheEntry.LITPROFILE.description()).isEqualTo("FunctionCacheEntry{description=literal profile}");
        assertThat(FunctionCacheEntry.VARPROFILE.description()).isEqualTo("FunctionCacheEntry{description=variable profile}");
        assertThat(FunctionCacheEntry.SUBFORMULAS.description()).isEqualTo("FunctionCacheEntry{description=sub-formulas}");
    }

    @Test
    public void testValues() {
        final List<TransformationCacheEntry> valuesTrans = Arrays.asList(TransformationCacheEntry.values());
        assertThat(valuesTrans.size()).isEqualTo(13);
        assertThat(valuesTrans.contains(TransformationCacheEntry.valueOf("FACTORIZED_DNF"))).isTrue();
        assertThat(valuesTrans.contains(TransformationCacheEntry.valueOf("PLAISTED_GREENBAUM_NEG"))).isTrue();

        final List<PredicateCacheEntry> valuesPred = Arrays.asList(PredicateCacheEntry.values());
        assertThat(valuesPred.size()).isEqualTo(6);
        assertThat(valuesPred.contains(PredicateCacheEntry.valueOf("IS_NNF"))).isTrue();
        assertThat(valuesPred.contains(PredicateCacheEntry.valueOf("IS_CNF"))).isTrue();
        assertThat(valuesPred.contains(PredicateCacheEntry.valueOf("IS_DNF"))).isTrue();
        assertThat(valuesPred.contains(PredicateCacheEntry.valueOf("IS_SAT"))).isTrue();

        final List<FunctionCacheEntry> valuesFunc = Arrays.asList(FunctionCacheEntry.values());
        assertThat(valuesFunc.size()).isEqualTo(5);
        assertThat(valuesFunc.contains(FunctionCacheEntry.valueOf("LITPROFILE"))).isTrue();
        assertThat(valuesFunc.contains(FunctionCacheEntry.valueOf("SUBFORMULAS"))).isTrue();
    }
}
