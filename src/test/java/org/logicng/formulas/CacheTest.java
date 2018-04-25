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

package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the package formulas.cache.
 * @version 1.3
 * @since 1.1
 */
public class CacheTest {

  @Test
  public void testDescription() {
    Assert.assertEquals("TransformationCacheEntry{description=and-inverter graph}", TransformationCacheEntry.AIG.description());
    Assert.assertEquals("TransformationCacheEntry{description=negation normal form}", TransformationCacheEntry.NNF.description());
    Assert.assertEquals("TransformationCacheEntry{description=Plaisted & Greenbaum conjunctive normal form (positive polarity)}", TransformationCacheEntry.PLAISTED_GREENBAUM_POS.description());
    Assert.assertEquals("TransformationCacheEntry{description=Tseitin conjunctive normal form}", TransformationCacheEntry.TSEITIN.description());
    Assert.assertEquals("TransformationCacheEntry{description=factorized conjunctive normal form}", TransformationCacheEntry.FACTORIZED_CNF.description());

    Assert.assertEquals("PredicateCacheEntry{description=and-inverter graph}", PredicateCacheEntry.IS_AIG.description());
    Assert.assertEquals("PredicateCacheEntry{description=tautology}", PredicateCacheEntry.IS_TAUTOLOGY.description());
    Assert.assertEquals("PredicateCacheEntry{description=conjunctive normal form}", PredicateCacheEntry.IS_CNF.description());

    Assert.assertEquals("FunctionCacheEntry{description=literal profile}", FunctionCacheEntry.LITPROFILE.description());
    Assert.assertEquals("FunctionCacheEntry{description=variable profile}", FunctionCacheEntry.VARPROFILE.description());
    Assert.assertEquals("FunctionCacheEntry{description=sub-formulas}", FunctionCacheEntry.SUBFORMULAS.description());
  }

  @Test
  public void testValues() {
    final List<TransformationCacheEntry> valuesTrans = Arrays.asList(TransformationCacheEntry.values());
    Assert.assertEquals(13, valuesTrans.size());
    Assert.assertTrue(valuesTrans.contains(TransformationCacheEntry.valueOf("FACTORIZED_DNF")));
    Assert.assertTrue(valuesTrans.contains(TransformationCacheEntry.valueOf("PLAISTED_GREENBAUM_NEG")));

    final List<PredicateCacheEntry> valuesPred = Arrays.asList(PredicateCacheEntry.values());
    Assert.assertEquals(5, valuesPred.size());
    Assert.assertTrue(valuesPred.contains(PredicateCacheEntry.valueOf("IS_DNF")));
    Assert.assertTrue(valuesPred.contains(PredicateCacheEntry.valueOf("IS_SAT")));

    final List<FunctionCacheEntry> valuesFunc = Arrays.asList(FunctionCacheEntry.values());
    Assert.assertEquals(3, valuesFunc.size());
    Assert.assertTrue(valuesFunc.contains(FunctionCacheEntry.valueOf("LITPROFILE")));
    Assert.assertTrue(valuesFunc.contains(FunctionCacheEntry.valueOf("SUBFORMULAS")));
  }

}
