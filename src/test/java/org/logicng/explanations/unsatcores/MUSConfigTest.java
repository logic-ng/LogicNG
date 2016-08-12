package org.logicng.explanations.unsatcores;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.transformations.cnf.CNFConfig;

import java.util.Arrays;

/**
 * Unit tests for the class {@link MUSConfig}.
 * @version 1.1
 * @since 1.1
 */
public class MUSConfigTest {

  @Test
  public void testMUSConfiguration() {
    final MUSConfig config = new MUSConfig.Builder().algorithm(MUSConfig.Algorithm.DELETION).build();
    Assert.assertEquals("MUSConfig{\nalgorithm=DELETION\n}\n", config.toString());
    Assert.assertTrue(Arrays.asList(CNFConfig.Algorithm.values()).contains(CNFConfig.Algorithm.valueOf("TSEITIN")));
  }

}
