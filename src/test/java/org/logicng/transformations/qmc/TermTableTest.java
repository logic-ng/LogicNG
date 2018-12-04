package org.logicng.transformations.qmc;

import org.junit.Before;
import org.junit.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Vector;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.transformations.qmc.QuineMcCluskeyAlgorithm.computePrimeImplicants;
import static org.logicng.transformations.qmc.QuineMcCluskeyTest.getTerm;
import static org.logicng.transformations.qmc.TermTable.isSubsetOf;

/**
 * Unit tests for {@link TermTable}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class TermTableTest {

  private LinkedHashSet<Term> primeImplicants;

  @Before
  public void init() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final Term m0 = getTerm("~x0 ~x1 ~x2 ~x3", f);
    final Term m1 = getTerm("~x0 ~x1 ~x2 x3", f);
    final Term m4 = getTerm("~x0 x1 ~x2 ~x3", f);
    final Term m8 = getTerm("x0 ~x1 ~x2 ~x3", f);
    final Term m5 = getTerm("~x0 x1 ~x2 x3", f);
    final Term m6 = getTerm("~x0 x1 x2 ~x3", f);
    final Term m9 = getTerm("x0 ~x1 ~x2 x3", f);
    final Term m7 = getTerm("~x0 x1 x2 x3", f);
    final Term m11 = getTerm("x0 ~x1 x2 x3", f);
    final Term m15 = getTerm("x0 x1 x2 x3", f);
    this.primeImplicants = computePrimeImplicants(Arrays.asList(m0, m1, m4, m8, m5, m6, m9, m7, m11, m15));
  }

  @Test
  public void testToString() {
    final TermTable table = new TermTable(this.primeImplicants);
    assertThat(table.toString()).isEqualTo(String.format("             | m0 | m1 | m2 | m3 | m4 | m5 | m6 | m7 | m8 | m9 | %n" +
            "[1, 0, -, 1] | X  | X  |    |    |    |    |    |    |    |    | %n" +
            "[-, 1, 1, 1] |    |    | X  | X  |    |    |    |    |    |    | %n" +
            "[1, -, 1, 1] |    | X  |    | X  |    |    |    |    |    |    | %n" +
            "[0, -, 0, -] |    |    |    |    | X  | X  | X  | X  |    |    | %n" +
            "[-, 0, 0, -] | X  |    |    |    | X  | X  |    |    | X  |    | %n" +
            "[0, 1, -, -] |    |    | X  |    |    |    | X  | X  |    | X  | %n"));
  }

  @Test
  public void testSubset() {
    final Vector<Boolean> vec1 = new Vector<>(Arrays.asList(true, true, false, false, true));
    final Vector<Boolean> vec2 = new Vector<>(Arrays.asList(true, true, false, false, true));
    final Vector<Boolean> vec3 = new Vector<>(Arrays.asList(true, false, false, false, false));
    final Vector<Boolean> vec4 = new Vector<>(Arrays.asList(true, true, true, false, true));
    assertThat(isSubsetOf(vec1, vec2)).isTrue();
    assertThat(isSubsetOf(vec1, vec3)).isFalse();
    assertThat(isSubsetOf(vec1, vec4)).isTrue();
    assertThat(isSubsetOf(vec2, vec3)).isFalse();
    assertThat(isSubsetOf(vec2, vec4)).isTrue();
    assertThat(isSubsetOf(vec3, vec4)).isTrue();
  }

  @Test
  public void testElimination() {
    final TermTable table = new TermTable(this.primeImplicants);
    table.simplifyTableByDominance();
    assertThat(table.lineHeaders()).hasSize(3);
    assertThat(table.columnHeaders()).hasSize(3);
    assertThat(table.lineHeaders()).hasSize(3);
    assertThat(table.columns()).hasSize(3);
    assertThat(table.lines()).hasSize(3);
  }
}
