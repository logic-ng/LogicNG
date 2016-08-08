package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.transformations.AIGTransformation;

/**
 * Unit tests for the package formulas.cache.
 */
public class CacheTest {

  @Test
  public void testDescription(){
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

}
