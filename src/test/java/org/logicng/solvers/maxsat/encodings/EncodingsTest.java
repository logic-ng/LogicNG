package org.logicng.solvers.maxsat.encodings;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.maxsat.encodings.Encoder;

/**
 * Unit test for the package solvers.maxsat.encodings.
 * @version 1.1
 * @since 1.1
 */
public class EncodingsTest {

  @Test
  public void testEncoder(){
    Encoder encoder = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
    Assert.assertEquals("Encoder", encoder.toString());
  }

  @Test
  public void testTotalizer(){
    Totalizer totalizer = new Totalizer(MaxSATConfig.IncrementalStrategy.ITERATIVE);
    Assert.assertEquals(MaxSATConfig.IncrementalStrategy.ITERATIVE, totalizer.incremental());
    Assert.assertEquals("Totalizer", totalizer.toString());
  }

  @Test
  public void testModularTotalizer(){
    ModularTotalizer mTotalizer = new ModularTotalizer();
    Assert.assertEquals(false, mTotalizer.hasCreatedEncoding());
    Assert.assertEquals("ModularTotalizer", mTotalizer.toString());
  }

  @Test
  public void testSequentialWeightCounter(){
    SequentialWeightCounter swc = new SequentialWeightCounter();
    Assert.assertEquals(false, swc.hasCreatedEncoding());
    Assert.assertEquals("SequentialWeightCounter", swc.toString());
  }

  @Test
  public void testLadder(){
    Ladder ladder = new Ladder();
    Assert.assertEquals("Ladder", ladder.toString());
    System.out.println(((Encoding) ladder).toString());
  }
}
