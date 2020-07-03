import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.FormulaOnSolverFunction;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.Set;

public class Test {

    public static void main(final String[] args) throws ParserException {

        final FormulaFactory f = new FormulaFactory();

        final Formula formula = f.parse("A & ~(A | B)");
        //final Formula formula = f.parse("B & A | B |  (B => ~B) ");
        //final Formula formula = f.parse("A & (A <=> B)");
        //final Formula formula = f.parse("A & B <=> ~A & ~B");
        //final Formula formula = f.parse("A <=> B");
        //final Formula formula = f.parse("~(A <=> B)");
        //final Formula formula = f.parse("~(A => B)");
        //final Formula formula = f.parse("459 & L => ~(830 | ~(M264 & M014) | M276 & M016 | M177) & ~(830 | 835 & M005 & ~(M264 & M20 & M014))");

        //final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(42).numVars(3).build());
        //for (int i = 0; i < 1000; i++) {
        //    final Formula formula = randomizer.formula(2);

        System.out.println();
        System.out.println("cnf pg");
        final Set<Formula> cnfPg = getCnf(formula, MiniSatConfig.CNFMethod.PG_ON_SOLVER);
        System.out.println();
        System.out.println("cnf pg direct");
        final Set<Formula> cnfPgDirect = getCnf(formula, MiniSatConfig.CNFMethod.DIRECT_PG_ON_SOLVER);

        if (cnfPg.size() < cnfPgDirect.size()) {
            System.out.println("<<<<");
            System.out.println(formula);
            System.out.println();
            System.out.println("cnfPg = " + cnfPg.size());
            cnfPg.forEach(System.out::println);
            System.out.println();
            System.out.println("cnfPgDirect = " + cnfPgDirect.size());
            cnfPgDirect.forEach(System.out::println);
            //break;
        }
        //}

        //test(f.parse("A | (B & (C | D))"));
        //
        //final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(8)
        //        // .weightPbc(100)
        //        .seed(42).build());
        //for (int i = 0; i < 10000; i++) {
        //    System.out.println("i = " + i);
        //
        //    final Formula formula = randomizer.formula(4);
        //    test(formula);
        //}
    }

    private static Set<Formula> getCnf(final Formula formula, final MiniSatConfig.CNFMethod cnfMethod) {
        final FormulaFactory f = formula.factory();
        final MiniSat solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(cnfMethod).build());
        solver.add(formula);
        return solver.execute(new FormulaOnSolverFunction());
    }

    private static void test(final Formula formula) {
        final FormulaFactory f = formula.factory();
        final SATSolver minisat01 = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.DIRECT_PG_ON_SOLVER).build());
        minisat01.add(formula.nnf());
        final Set<Formula> cnf01 = minisat01.execute(new FormulaOnSolverFunction());

        final SATSolver minisat02 = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        minisat02.add(formula.nnf());
        final Set<Formula> cnf02 = minisat02.execute(new FormulaOnSolverFunction());

        //if (cnf01.size() != cnf02.size()) {
        System.out.println("formula = " + formula);
        System.out.println("formula.nnf() = " + formula.nnf());
        System.out.println("cnf01 = " + cnf01);
        System.out.println("cnf02 = " + cnf02);
        //}
    }
}