package org.logicng;

import org.logicng.formulas.CType;
import org.logicng.formulas.Constant;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

public abstract class TestWithExampleFormulas {
    protected final FormulaFactory f = new FormulaFactory();
    protected final FormulaFactory g = new FormulaFactory();

    // Constants
    protected final Constant TRUE = this.f.verum();
    protected final Constant FALSE = this.f.falsum();

    // Literals
    protected final Variable A = this.f.variable("a");
    protected final Variable B = this.f.variable("b");
    protected final Variable C = this.f.variable("c");
    protected final Variable D = this.f.variable("d");
    protected final Variable X = this.f.variable("x");
    protected final Variable Y = this.f.variable("y");
    protected final Literal NA = this.f.literal("a", false);
    protected final Literal NB = this.f.literal("b", false);
    protected final Literal NX = this.f.literal("x", false);
    protected final Literal NY = this.f.literal("y", false);

    // Disjunctions
    protected final Formula OR1 = this.f.or(this.X, this.Y);
    protected final Formula OR2 = this.f.or(this.NX, this.NY);
    protected final Formula OR3 = this.f.or(this.f.and(this.A, this.B), this.f.and(this.NA, this.NB));

    // Conjunctions
    protected final Formula AND1 = this.f.and(this.A, this.B);
    protected final Formula AND2 = this.f.and(this.NA, this.NB);
    protected final Formula AND3 = this.f.and(this.OR1, this.OR2);

    // Negations
    protected final Formula NOT1 = this.f.not(this.AND1);
    protected final Formula NOT2 = this.f.not(this.OR1);

    // Implications
    protected final Formula IMP1 = this.f.implication(this.A, this.B);
    protected final Formula IMP2 = this.f.implication(this.NA, this.NB);
    protected final Formula IMP3 = this.f.implication(this.AND1, this.OR1);
    protected final Formula IMP4 = this.f.implication(this.f.equivalence(this.A, this.B), this.f.equivalence(this.NX, this.NY));

    // Equivalences
    protected final Formula EQ1 = this.f.equivalence(this.A, this.B);
    protected final Formula EQ2 = this.f.equivalence(this.NA, this.NB);
    protected final Formula EQ3 = this.f.equivalence(this.AND1, this.OR1);
    protected final Formula EQ4 = this.f.equivalence(this.IMP1, this.IMP2);

    // PBCs
    private final Literal[] literals = new Literal[]{this.A, this.B, this.X};
    private final int[] coefficients = new int[]{2, -4, 3};
    protected final Formula PBC1 = this.f.pbc(CType.EQ, 2, this.literals, this.coefficients);
    protected final Formula PBC2 = this.f.pbc(CType.GT, 2, this.literals, this.coefficients);
    protected final Formula PBC3 = this.f.pbc(CType.GE, 2, this.literals, this.coefficients);
    protected final Formula PBC4 = this.f.pbc(CType.LT, 2, this.literals, this.coefficients);
    protected final Formula PBC5 = this.f.pbc(CType.LE, 2, this.literals, this.coefficients);
}
