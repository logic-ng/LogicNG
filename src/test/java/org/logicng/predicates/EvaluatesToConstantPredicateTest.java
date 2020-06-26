package org.logicng.predicates;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.HashMap;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;

/**
 * Unit Tests for the class {@link EvaluatesToConstantPredicate}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class EvaluatesToConstantPredicateTest {

    private static final FormulaFactory f = F.f;

    private static final EvaluatesToConstantPredicate emptyToFalse;
    private static final EvaluatesToConstantPredicate aToFalse;
    private static final EvaluatesToConstantPredicate aNotBToFalse;

    private static final EvaluatesToConstantPredicate emptyToTrue;
    private static final EvaluatesToConstantPredicate aToTrue;
    private static final EvaluatesToConstantPredicate aNotBToTrue;

    static {
        emptyToFalse = new EvaluatesToConstantPredicate(false, new HashMap<>());
        emptyToTrue = new EvaluatesToConstantPredicate(true, new HashMap<>());

        final HashMap<Variable, Boolean> aMap = new HashMap<>();
        aMap.put(F.A, true);
        aToFalse = new EvaluatesToConstantPredicate(false, aMap);
        aToTrue = new EvaluatesToConstantPredicate(true, aMap);

        final HashMap<Variable, Boolean> aNotBMap = new HashMap<>();
        aNotBMap.put(F.A, true);
        aNotBMap.put(F.B, false);
        aNotBToFalse = new EvaluatesToConstantPredicate(false, aNotBMap);
        aNotBToTrue = new EvaluatesToConstantPredicate(true, aNotBMap);
    }

    @Test
    public void getMapping() {
        assertThat(emptyToFalse.getMapping()).containsExactly();
        assertThat(aToFalse.getMapping()).containsExactly(entry(F.A, true));
        assertThat(aNotBToFalse.getMapping()).containsExactly(entry(F.A, true), entry(F.B, false));

        assertThat(emptyToTrue.getMapping()).containsExactly();
        assertThat(aToTrue.getMapping()).containsExactly(entry(F.A, true));
        assertThat(aNotBToTrue.getMapping()).containsExactly(entry(F.A, true), entry(F.B, false));
    }


    @Test
    public void testConstantsToFalse() {
        assertThat(f.falsum().holds(emptyToFalse)).isTrue();
        assertThat(f.falsum().holds(aToFalse)).isTrue();
        assertThat(f.falsum().holds(aNotBToFalse)).isTrue();

        assertThat(f.verum().holds(emptyToFalse)).isFalse();
        assertThat(f.verum().holds(aToFalse)).isFalse();
        assertThat(f.verum().holds(aNotBToFalse)).isFalse();
    }

    @Test
    public void testLiteralsToFalse() {
        assertThat(F.A.holds(emptyToFalse)).isFalse();
        assertThat(F.A.holds(aToFalse)).isFalse();
        assertThat(F.A.holds(aNotBToFalse)).isFalse();

        assertThat(F.NA.holds(emptyToFalse)).isFalse();
        assertThat(F.NA.holds(aToFalse)).isTrue();
        assertThat(F.NA.holds(aNotBToFalse)).isTrue();

        assertThat(F.B.holds(emptyToFalse)).isFalse();
        assertThat(F.B.holds(aToFalse)).isFalse();
        assertThat(F.B.holds(aNotBToFalse)).isTrue();

        assertThat(F.NB.holds(emptyToFalse)).isFalse();
        assertThat(F.NB.holds(aToFalse)).isFalse();
        assertThat(F.NB.holds(aNotBToFalse)).isFalse();
    }

    @Test
    public void testNotToFalse() throws ParserException {
        assertThat(f.parse("~~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~~a").holds(aToFalse)).isFalse();
        assertThat(f.parse("~~a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~~~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~~~a").holds(aToFalse)).isTrue();
        assertThat(f.parse("~~~a").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~(a & b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~(a & b)").holds(aToFalse)).isFalse();
        assertThat(f.parse("~(a & b)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~(~a & b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~(~a & b)").holds(aToFalse)).isFalse();
        assertThat(f.parse("~(~a & b)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~(a & ~b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~(a & ~b)").holds(aToFalse)).isFalse();
        assertThat(f.parse("~(a & ~b)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~(~a & ~b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~(~a & ~b)").holds(aToFalse)).isFalse();
        assertThat(f.parse("~(~a & ~b)").holds(aNotBToFalse)).isFalse();
    }

    @Test
    public void testAndToFalse() throws ParserException {
        assertThat(f.parse("a & ~a").holds(emptyToFalse)).isTrue();
        assertThat(f.parse("a & ~a").holds(aToFalse)).isTrue();
        assertThat(f.parse("a & ~a").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("a & b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a & b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & b").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("a & ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a & ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & ~b").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & ~b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a & ~b & c & ~d").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & ~b & c & ~d").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & ~b & c & ~d").holds(aNotBToFalse)).isTrue();
    }

    @Test
    public void testOrToFalse() throws ParserException {
        assertThat(f.parse("a | b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a | b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a | b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a | b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a | b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a | b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("a | ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a | ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a | ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a | ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a | ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a | ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a | ~b | c | ~d").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a | ~b | c | ~d").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a | ~b | c | ~d").holds(aNotBToFalse)).isFalse();
    }

    @Test
    public void testImplicationToFalse() throws ParserException {
        assertThat(f.parse("a => a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a => a").holds(aToFalse)).isFalse();
        assertThat(f.parse("a => a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("b => b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b => b").holds(aToFalse)).isFalse();
        assertThat(f.parse("b => b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a => b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a => b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a => b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a => b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a => b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a => b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a => ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a => ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a => ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a => ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a => ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a => ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("b => a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b => a").holds(aToFalse)).isFalse();
        assertThat(f.parse("b => a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~b => a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~b => a").holds(aToFalse)).isFalse();
        assertThat(f.parse("~b => a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("b => ~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b => ~a").holds(aToFalse)).isFalse();
        assertThat(f.parse("b => ~a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~b => ~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~b => ~a").holds(aToFalse)).isFalse();
        assertThat(f.parse("~b => ~a").holds(aNotBToFalse)).isTrue();
    }

    @Test
    public void testEquivalenceToFalse() throws ParserException {
        assertThat(f.parse("a <=> a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a <=> a").holds(aToFalse)).isFalse();
        assertThat(f.parse("a <=> a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("b <=> b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b <=> b").holds(aToFalse)).isFalse();
        assertThat(f.parse("b <=> b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a <=> b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a <=> b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a <=> b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a <=> b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a <=> b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a <=> b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a <=> ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a <=> ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("a <=> ~b").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~a <=> ~b").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a <=> ~b").holds(aToFalse)).isFalse();
        assertThat(f.parse("~a <=> ~b").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("b <=> a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b <=> a").holds(aToFalse)).isFalse();
        assertThat(f.parse("b <=> a").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~b <=> a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~b <=> a").holds(aToFalse)).isFalse();
        assertThat(f.parse("~b <=> a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("b <=> ~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("b <=> ~a").holds(aToFalse)).isFalse();
        assertThat(f.parse("b <=> ~a").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("~b <=> ~a").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~b <=> ~a").holds(aToFalse)).isFalse();
        assertThat(f.parse("~b <=> ~a").holds(aNotBToFalse)).isTrue();
    }

    @Test
    public void testPBCToFalse() {
        final PBConstraint pbc01 = (PBConstraint) f.pbc(CType.EQ, 2, new Literal[]{F.A, F.B}, new int[]{2, -4});
        assertThat(pbc01.holds(emptyToFalse)).isFalse();
        assertThat(pbc01.holds(aToFalse)).isFalse();
        assertThat(pbc01.holds(aNotBToFalse)).isFalse();

        final PBConstraint pbc02 = (PBConstraint) f.pbc(CType.GT, 2, new Literal[]{F.B, F.C}, new int[]{2, 1});
        assertThat(pbc02.holds(emptyToFalse)).isFalse();
        assertThat(pbc02.holds(aToFalse)).isFalse();
        assertThat(pbc02.holds(aNotBToFalse)).isTrue();

        assertThat(F.PBC1.holds(emptyToFalse)).isFalse();
        assertThat(F.PBC1.holds(aToFalse)).isFalse();
        assertThat(F.PBC1.holds(aNotBToFalse)).isFalse();

        assertThat(F.PBC2.holds(emptyToFalse)).isFalse();
        assertThat(F.PBC2.holds(aToFalse)).isFalse();
        assertThat(F.PBC2.holds(aNotBToFalse)).isFalse();
    }

    @Test
    public void testMixedToFalse() throws ParserException {
        assertThat(f.parse("~a & (a | ~b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & (a | ~b)").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & (a | ~b)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~b & (b | ~a)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~b & (b | ~a)").holds(aToFalse)).isTrue();
        assertThat(f.parse("~b & (b | ~a)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(aToFalse)).isTrue();
        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(aNotBToFalse)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(aNotBToFalse)).isTrue();

        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(emptyToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(aToFalse)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(aNotBToFalse)).isTrue();
    }

    @Test
    public void testConstantsToTrue() {
        assertThat(f.falsum().holds(emptyToTrue)).isFalse();
        assertThat(f.falsum().holds(aToTrue)).isFalse();
        assertThat(f.falsum().holds(aNotBToTrue)).isFalse();

        assertThat(f.verum().holds(emptyToTrue)).isTrue();
        assertThat(f.verum().holds(aToTrue)).isTrue();
        assertThat(f.verum().holds(aNotBToTrue)).isTrue();
    }

    @Test
    public void testLiteralsToTrue() {
        assertThat(F.A.holds(emptyToTrue)).isFalse();
        assertThat(F.A.holds(aToTrue)).isTrue();
        assertThat(F.A.holds(aNotBToTrue)).isTrue();

        assertThat(F.NA.holds(emptyToTrue)).isFalse();
        assertThat(F.NA.holds(aToTrue)).isFalse();
        assertThat(F.NA.holds(aNotBToTrue)).isFalse();

        assertThat(F.B.holds(emptyToTrue)).isFalse();
        assertThat(F.B.holds(aToTrue)).isFalse();
        assertThat(F.B.holds(aNotBToTrue)).isFalse();

        assertThat(F.NB.holds(emptyToTrue)).isFalse();
        assertThat(F.NB.holds(aToTrue)).isFalse();
        assertThat(F.NB.holds(aNotBToTrue)).isTrue();
    }

    @Test
    public void testNotToTrue() throws ParserException {
        assertThat(f.parse("~~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~~a").holds(aToTrue)).isTrue();
        assertThat(f.parse("~~a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~~~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~~~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("~~~a").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~(a & b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~(a & b)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~(a & b)").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~(~a & b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~(~a & b)").holds(aToTrue)).isTrue();
        assertThat(f.parse("~(~a & b)").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~(a & ~b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~(a & ~b)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~(a & ~b)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~(~a & ~b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~(~a & ~b)").holds(aToTrue)).isTrue();
        assertThat(f.parse("~(~a & ~b)").holds(aNotBToTrue)).isTrue();
    }

    @Test
    public void testAndToTrue() throws ParserException {
        assertThat(f.parse("a & ~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & ~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & ~a").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a & b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a & ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & ~b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a & ~b & c & ~d").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & ~b & c & ~d").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & ~b & c & ~d").holds(aNotBToTrue)).isFalse();
    }

    @Test
    public void testOrToTrue() throws ParserException {
        assertThat(f.parse("a | b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a | b").holds(aToTrue)).isTrue();
        assertThat(f.parse("a | b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a | b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a | b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a | b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a | ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a | ~b").holds(aToTrue)).isTrue();
        assertThat(f.parse("a | ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a | ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a | ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a | ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a | ~b | c | ~d").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a | ~b | c | ~d").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a | ~b | c | ~d").holds(aNotBToTrue)).isTrue();
    }

    @Test
    public void testImplicationToTrue() throws ParserException {
        assertThat(f.parse("a => a").holds(emptyToTrue)).isTrue();
        assertThat(f.parse("a => a").holds(aToTrue)).isTrue();
        assertThat(f.parse("a => a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("b => b").holds(emptyToTrue)).isTrue();
        assertThat(f.parse("b => b").holds(aToTrue)).isTrue();
        assertThat(f.parse("b => b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("a => b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a => b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a => b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a => b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a => b").holds(aToTrue)).isTrue();
        assertThat(f.parse("~a => b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("a => ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a => ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a => ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a => ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a => ~b").holds(aToTrue)).isTrue();
        assertThat(f.parse("~a => ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("b => a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("b => a").holds(aToTrue)).isTrue();
        assertThat(f.parse("b => a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~b => a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b => a").holds(aToTrue)).isTrue();
        assertThat(f.parse("~b => a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("b => ~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("b => ~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("b => ~a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~b => ~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b => ~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("~b => ~a").holds(aNotBToTrue)).isFalse();
    }

    @Test
    public void testEquivalenceToTrue() throws ParserException {
        assertThat(f.parse("a <=> a").holds(emptyToTrue)).isTrue();
        assertThat(f.parse("a <=> a").holds(aToTrue)).isTrue();
        assertThat(f.parse("a <=> a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("b <=> b").holds(emptyToTrue)).isTrue();
        assertThat(f.parse("b <=> b").holds(aToTrue)).isTrue();
        assertThat(f.parse("b <=> b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("a <=> b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a <=> b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a <=> b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a <=> b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a <=> b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a <=> b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("a <=> ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a <=> ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("a <=> ~b").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~a <=> ~b").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a <=> ~b").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a <=> ~b").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("b <=> a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("b <=> a").holds(aToTrue)).isFalse();
        assertThat(f.parse("b <=> a").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~b <=> a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b <=> a").holds(aToTrue)).isFalse();
        assertThat(f.parse("~b <=> a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("b <=> ~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("b <=> ~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("b <=> ~a").holds(aNotBToTrue)).isTrue();

        assertThat(f.parse("~b <=> ~a").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b <=> ~a").holds(aToTrue)).isFalse();
        assertThat(f.parse("~b <=> ~a").holds(aNotBToTrue)).isFalse();
    }

    @Test
    public void testPBCToTrue() {
        final PBConstraint pbc01 = (PBConstraint) f.pbc(CType.EQ, 2, new Literal[]{F.A, F.B}, new int[]{2, -4});
        assertThat(pbc01.holds(emptyToTrue)).isFalse();
        assertThat(pbc01.holds(aToTrue)).isFalse();
        assertThat(pbc01.holds(aNotBToTrue)).isTrue();

        final PBConstraint pbc02 = (PBConstraint) f.pbc(CType.GT, 2, new Literal[]{F.B, F.C}, new int[]{2, 1});
        assertThat(pbc02.holds(emptyToTrue)).isFalse();
        assertThat(pbc02.holds(aToTrue)).isFalse();
        assertThat(pbc02.holds(aNotBToTrue)).isFalse();

        assertThat(F.PBC1.holds(emptyToTrue)).isFalse();
        assertThat(F.PBC1.holds(aToTrue)).isFalse();
        assertThat(F.PBC1.holds(aNotBToTrue)).isFalse();

        assertThat(F.PBC2.holds(emptyToTrue)).isFalse();
        assertThat(F.PBC2.holds(aToTrue)).isFalse();
        assertThat(F.PBC2.holds(aNotBToTrue)).isFalse();
    }

    @Test
    public void testMixedToTrue() throws ParserException {
        assertThat(f.parse("~a & (a | ~b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~b & (b | ~a)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b & (b | ~a)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~b & (b | ~a)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a & (a | ~b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~b & (b | ~a)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~b & (b | ~a)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~b & (b | ~a)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & (a | ~b) & c & (a => b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("~a & ~(a | ~b) & c & (a => b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a => ~b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a => b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & c & (a <=> ~b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b | e)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (a <=> b)").holds(aNotBToTrue)).isFalse();

        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(emptyToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(aToTrue)).isFalse();
        assertThat(f.parse("a & (a | ~b) & (3 * a + 2 * b > 4)").holds(aNotBToTrue)).isFalse();
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            final Assignment assignment = new Assignment();
            assignment.addLiteral(f.literal("v0", false));
            assignment.addLiteral(f.literal("v1", false));
            assignment.addLiteral(f.literal("v2", true));
            assignment.addLiteral(f.literal("v3", true));
            final EvaluatesToConstantPredicate falseEvaluation = new EvaluatesToConstantPredicate(false,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final EvaluatesToConstantPredicate trueEvaluation = new EvaluatesToConstantPredicate(true,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final Formula restricted = formula.restrict(assignment);
            assertThat(restricted.type() == FType.FALSE).isEqualTo(formula.holds(falseEvaluation));
            assertThat(restricted.type() == FType.TRUE).isEqualTo(formula.holds(trueEvaluation));
        }
    }

    @Test
    @RandomTag
    public void testRandom() {
        for (int i = 0; i < 1000; i++) {
            final FormulaFactory f = new FormulaFactory();
            final Assignment assignment = new Assignment();
            assignment.addLiteral(f.literal("v0", false));
            assignment.addLiteral(f.literal("v1", false));
            assignment.addLiteral(f.literal("v2", true));
            assignment.addLiteral(f.literal("v3", true));
            final EvaluatesToConstantPredicate falseEvaluation = new EvaluatesToConstantPredicate(false,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final EvaluatesToConstantPredicate trueEvaluation = new EvaluatesToConstantPredicate(true,
                    assignment.literals().stream().collect(Collectors.toMap(Literal::variable, Literal::phase)));
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(1).seed(42).build());
            final Formula formula = randomizer.formula(6);
            final Formula restricted = formula.restrict(assignment);
            assertThat(restricted.type() == FType.FALSE).isEqualTo(formula.holds(falseEvaluation));
            assertThat(restricted.type() == FType.TRUE).isEqualTo(formula.holds(trueEvaluation));
        }
    }
}
