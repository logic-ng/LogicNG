package org.logicng.configurations;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests for the package configurations.
 * @version 1.1
 * @since 1.1
 */
public class ConfigurationsTest {

  @Test
  public void testValueOf(){
    Assert.assertEquals(ConfigurationType.CNF, ConfigurationType.valueOf("CNF"));
    Assert.assertEquals(ConfigurationType.GLUCOSE, ConfigurationType.valueOf("GLUCOSE"));
    Assert.assertEquals(ConfigurationType.MAXSAT, ConfigurationType.valueOf("MAXSAT"));
    Assert.assertEquals(ConfigurationType.CC_ENCODER, ConfigurationType.valueOf("CC_ENCODER"));
  }
}
