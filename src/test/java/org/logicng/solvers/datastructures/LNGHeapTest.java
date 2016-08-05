package org.logicng.solvers.datastructures;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.collections.LNGIntVector;
import org.logicng.formulas.FormulaFactory;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * Unit tests for the class {@link LNGHeap}.
 */
public class LNGHeapTest {

  @Test
  public void test(){
    MiniSatStyleSolver solver = new MiniCard();
    LNGHeap heap = new LNGHeap(solver);
    Assert.assertTrue(heap.empty());
    heap.insert(5);
    Assert.assertTrue(heap.toString().contains("5"));
    Assert.assertEquals(1,heap.size());
    heap.clear();
    Assert.assertTrue(heap.empty());
  }
}
