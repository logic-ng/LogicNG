package org.logicng.transformations.qmc;

import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * Unit tests for {@link Term}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class TermTest {

  @Test
  public void testUnite() {
    final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
    final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
    final Tristate[] bits3 = new Tristate[]{TRUE, TRUE, FALSE, TRUE};
    final Tristate[] bits4 = new Tristate[]{FALSE, FALSE, TRUE, TRUE};
    final Tristate[] bits5 = new Tristate[]{TRUE, FALSE, FALSE, UNDEF};
    final Tristate[] bits6 = new Tristate[]{TRUE, FALSE, TRUE, UNDEF};
    final Tristate[] bits7 = new Tristate[]{TRUE, FALSE, TRUE};

    final FormulaFactory f = new FormulaFactory();
    final List<Formula> formulas1 = Collections.<Formula>singletonList(f.variable("f1"));
    final List<Formula> formulas2 = Collections.<Formula>singletonList(f.variable("f2"));
    final List<Formula> formulas3 = Collections.<Formula>singletonList(f.variable("f3"));
    final List<Formula> formulas4 = Collections.<Formula>singletonList(f.variable("f4"));
    final List<Formula> formulas5 = Collections.<Formula>singletonList(f.variable("f5"));
    final List<Formula> formulas6 = Collections.<Formula>singletonList(f.variable("f6"));
    final List<Formula> formulas7 = Collections.<Formula>singletonList(f.variable("f7"));

    final Term term1 = new Term(bits1, formulas1);
    final Term term2 = new Term(bits2, formulas2);
    final Term term3 = new Term(bits3, formulas3);
    final Term term4 = new Term(bits4, formulas4);
    final Term term5 = new Term(bits5, formulas5);
    final Term term6 = new Term(bits6, formulas6);
    final Term term7 = new Term(bits7, formulas7);

    assertThat(term1.combine(term2)).isEqualTo(new Term(new Tristate[]{TRUE, UNDEF, TRUE, TRUE}, Arrays.<Formula>asList(f.variable("f1"), f.variable("f2"))));
    assertThat(term1.combine(term3)).isEqualTo(new Term(new Tristate[]{TRUE, TRUE, UNDEF, TRUE}, Arrays.<Formula>asList(f.variable("f1"), f.variable("f3"))));
    assertThat(term2.combine(term3)).isNull();
    assertThat(term1.combine(term4)).isNull();
    assertThat(term2.combine(term4)).isEqualTo(new Term(new Tristate[]{UNDEF, FALSE, TRUE, TRUE}, Arrays.<Formula>asList(f.variable("f2"), f.variable("f4"))));
    assertThat(term3.combine(term4)).isNull();
    assertThat(term5.combine(term6)).isEqualTo(new Term(new Tristate[]{TRUE, FALSE, UNDEF, UNDEF}, Arrays.<Formula>asList(f.variable("f5"), f.variable("f6"))));
    assertThat(term1.combine(term7)).isNull();
  }

  @Test
  public void testToFormula() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
    final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
    final Tristate[] bits3 = new Tristate[]{TRUE, TRUE, FALSE, TRUE};
    final Tristate[] bits4 = new Tristate[]{FALSE, FALSE, TRUE, TRUE};
    final Tristate[] bits5 = new Tristate[]{TRUE, FALSE, FALSE, UNDEF};
    final Tristate[] bits6 = new Tristate[]{TRUE, UNDEF, TRUE, UNDEF};
    final List<Formula> formulas1 = Collections.<Formula>singletonList(f.variable("f1"));
    final List<Formula> formulas2 = Collections.<Formula>singletonList(f.variable("f2"));
    final List<Formula> formulas3 = Collections.<Formula>singletonList(f.variable("f3"));
    final List<Formula> formulas4 = Collections.<Formula>singletonList(f.variable("f4"));
    final List<Formula> formulas5 = Collections.<Formula>singletonList(f.variable("f5"));
    final List<Formula> formulas6 = Collections.<Formula>singletonList(f.variable("f6"));

    final Term term1 = new Term(bits1, formulas1);
    final Term term2 = new Term(bits2, formulas2);
    final Term term3 = new Term(bits3, formulas3);
    final Term term4 = new Term(bits4, formulas4);
    final Term term5 = new Term(bits5, formulas5);
    final Term term6 = new Term(bits6, formulas6);
    final List<Variable> varOrder = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));

    assertThat(term1.translateToFormula(varOrder)).isEqualTo(p.parse("A & B & C & D"));
    assertThat(term2.translateToFormula(varOrder)).isEqualTo(p.parse("A & ~B & C & D"));
    assertThat(term3.translateToFormula(varOrder)).isEqualTo(p.parse("A & B & ~C & D"));
    assertThat(term4.translateToFormula(varOrder)).isEqualTo(p.parse("~A & ~B & C & D"));
    assertThat(term5.translateToFormula(varOrder)).isEqualTo(p.parse("A & ~B & ~C"));
    assertThat(term6.translateToFormula(varOrder)).isEqualTo(p.parse("A & C"));
    assertThat(term1.combine(term1)).isNull();
  }

  @Test
  public void testToString() {
    final FormulaFactory f = new FormulaFactory();
    final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
    final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
    final List<Formula> formulas1 = Collections.<Formula>singletonList(f.variable("f1"));
    final List<Formula> formulas2 = Collections.<Formula>singletonList(f.variable("f2"));
    final Term term1 = new Term(bits1, formulas1);
    final Term term2 = new Term(bits2, formulas2);
    assertThat(term1.toString()).isEqualTo("Term{bits=[TRUE, TRUE, TRUE, TRUE], minterms=[f1], termClass=4}");
    assertThat(term2.toString()).isEqualTo("Term{bits=[TRUE, FALSE, TRUE, TRUE], minterms=[f2], termClass=3}");
  }

}
