package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.formulas.cache.PredicateCacheEntry;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.transformations.AIGTransformation;

import java.util.Arrays;
import java.util.List;

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

  @Test
  public void testValues(){
    List<TransformationCacheEntry> valuesTrans = Arrays.asList(TransformationCacheEntry.values());
    Assert.assertTrue(valuesTrans.size() == 9);
    Assert.assertTrue(valuesTrans.contains(TransformationCacheEntry.valueOf("FACTORIZED_DNF")));
    Assert.assertTrue(valuesTrans.contains(TransformationCacheEntry.valueOf("PLAISTED_GREENBAUM_NEG")));

    List<PredicateCacheEntry> valuesPred = Arrays.asList(PredicateCacheEntry.values());
    Assert.assertTrue(valuesPred.size() == 5);
    Assert.assertTrue(valuesPred.contains(PredicateCacheEntry.valueOf("IS_DNF")));
    Assert.assertTrue(valuesPred.contains(PredicateCacheEntry.valueOf("IS_SAT")));

    List<FunctionCacheEntry> valuesFunc = Arrays.asList(FunctionCacheEntry.values());
    Assert.assertTrue(valuesFunc.size() == 3);
    Assert.assertTrue(valuesFunc.contains(FunctionCacheEntry.valueOf("LITPROFILE")));
    Assert.assertTrue(valuesFunc.contains(FunctionCacheEntry.valueOf("SUBFORMULAS")));
  }

}
