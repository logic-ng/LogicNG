package org.logicng.solvers.sat;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.logicng.collections.LNGIntVector;

/**
 * Unit tests for the class {@link GlucoseSyrup}
 * @version 1.3
 * @since 1.1
 */
public class GlucoseSyrupTest {

  private GlucoseSyrup gs = new GlucoseSyrup();

  @Before
  public void prepare() {
    gs.newVar(true, true);
    gs.newVar(true, true);
    gs.newVar(true, true);
    gs.newVar(true, true);
    gs.addClause(clause(1, 2, 3), null);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testSaveState() {
    gs.saveState();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testLoadState() {
    gs.loadState(new int[5]);
  }

  @Test
  public void testToString() {
    String expected = String.format("ok            true%n" +
            "qhead         0%n" +
            "#clauses      1%n" +
            "#learnts      0%n" +
            "#watches      8%n" +
            "#vars         4%n" +
            "#orderheap    4%n" +
            "#trail        0%n" +
            "#trailLim     0%n" +
            "model         []%n" +
            "conflict      []%n" +
            "assumptions   []%n" +
            "#seen         4%n" +
            "#stack        0%n" +
            "#toclear      0%n" +
            "claInc        1.0%n" +
            "simpDBAssigns -1%n" +
            "simpDBProps   0%n" +
            "#clause lits  3%n" +
            "#learnts lits 0%n");
    Assert.assertEquals(expected, gs.toString());
  }

  private LNGIntVector clause(int... lits) {
    final LNGIntVector c = new LNGIntVector(lits.length);
    for (int l : lits)
      c.push(literal(l));
    return c;
  }

  private int literal(int l) {
    return l < 0 ? (-l * 2) ^ 1 : l * 2;
  }

}
