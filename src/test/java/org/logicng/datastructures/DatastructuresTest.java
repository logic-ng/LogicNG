package org.logicng.datastructures;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests for the package datastructues.
 * @version 1.1
 * @since 1.1
 */
public class DatastructuresTest {

  @Test
  public void testTristate(){
    Assert.assertEquals(Tristate.TRUE, Tristate.valueOf("TRUE"));
    Assert.assertEquals(Tristate.FALSE, Tristate.valueOf("FALSE"));
    Assert.assertEquals(Tristate.UNDEF, Tristate.valueOf("UNDEF"));
  }

  @Test
  public void testEncodingAuxiliaryVariable(){
    EncodingAuxiliaryVariable eav = new EncodingAuxiliaryVariable("var", false);
    Assert.assertEquals("var", eav.toString());
  }
}
