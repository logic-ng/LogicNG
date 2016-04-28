package org.logicng.explanations.unsatcores;

import org.junit.Assert;
import org.junit.Test;

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
  }

}
