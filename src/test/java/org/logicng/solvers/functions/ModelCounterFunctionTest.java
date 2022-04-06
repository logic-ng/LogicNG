package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.backbones.Backbone;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.Pair;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

class ModelCounterFunctionTest {

    private static final FormulaFactory f = new FormulaFactory();

    public static Collection<Object[]> solvers() {
        final MiniSatConfig configNoPG1 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configNoPG2 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(true)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configNoPG3 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configNoPG4 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(true).build();
        final MiniSatConfig configNoPG5 =
                MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(true).bbCheckForComplementModelLiterals(true)
                        .bbInitialUBCheckForRotatableLiterals(true).build();
        final MiniSatConfig configPG1 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configPG2 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(true)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configPG3 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(false).build();
        final MiniSatConfig configPG4 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false)
                .bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(true).build();
        final MiniSatConfig configPG5 = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(true)
                .bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(true).build();
        final List<Pair<MiniSatConfig, String>> configs = Arrays.asList(
                new Pair<>(configNoPG1, "FF CNF -ROT -COMP -UB"),
                new Pair<>(configNoPG2, "FF CNF +ROT -COMP -UB"),
                new Pair<>(configNoPG3, "FF CNF -ROT +COMP -UB"),
                new Pair<>(configNoPG4, "FF CNF -ROT -COMP +UB"),
                new Pair<>(configNoPG5, "FF CNF +ROT +COMP +UB"),
                new Pair<>(configPG1, "PG CNF -ROT -COMP -UB"),
                new Pair<>(configPG2, "PG CNF +ROT -COMP -UB"),
                new Pair<>(configPG3, "PG CNF -ROT +COMP -UB"),
                new Pair<>(configPG4, "PG CNF -ROT -COMP +UB"),
                new Pair<>(configPG5, "PG CNF +ROT +COMP +UB")
        );
        final List<Object[]> solvers = new ArrayList<>();
        for (final Pair<MiniSatConfig, String> config : configs) {
            solvers.add(new Object[]{MiniSat.miniSat(f, config.first()), "MiniSat (" + config.second() + ")"});
            solvers.add(new Object[]{MiniSat.miniCard(f, config.first()), "MiniCard (" + config.second() + ")"});
            solvers.add(new Object[]{MiniSat.glucose(f, config.first(), GlucoseConfig.builder().build()), "Glucose (" + config.second() + ")"});
        }
        return solvers;
    }

    @Test
    public void simpleTest() throws ParserException {
        final MiniSat miniSat = MiniSat.miniSat(f);
        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Formula f1 = f.or(a, b, c);
        miniSat.add(f1);
        final BigInteger modelcount = miniSat.execute(ModelCounterFunction.builder().build());
        System.out.println(modelcount);
        System.out.println("**");
        final List<Assignment> assignments = miniSat.execute(ModelEnumerationFunction.builder().build());
        for (final Assignment assignment : assignments) {
            System.out.println(assignment);
        }
    }

    @Test
    public void simpleTestTautology() {
        final MiniSat miniSat = MiniSat.miniSat(f);
        final Variable a = f.variable("A");
        final Formula f1 = f.or(a, a.negate());
        miniSat.add(f1);
        final BigInteger modelcount = miniSat.execute(ModelCounterFunction.builder().additionalVariables(a).build());
        System.out.println(modelcount);
        System.out.println("**");
        final List<Assignment> assignments = miniSat.execute(ModelEnumerationFunction.builder().additionalVariables(a).build());
        for (final Assignment assignment : assignments) {
            System.out.println(assignment);
        }

    }


    @ParameterizedTest
    @MethodSource("solvers")
    public void test(final MiniSat solver) {
        solver.reset();
        SolverState state = null;
        if (solver.underlyingSolver() instanceof MiniSat2Solver) {
            state = solver.saveState();
        }
        solver.add(f.falsum());
        Backbone backbone = solver.backbone(v("a b c"));
        assertThat(backbone.isSat()).isFalse();
        assertThat(backbone.getCompleteBackbone()).isEmpty();
        if (solver.underlyingSolver() instanceof MiniSat2Solver) {
            solver.loadState(state);
        } else {
            solver.reset();
        }
        solver.add(f.verum());
        backbone = solver.backbone(v("a b c"));
        final BigInteger modelcount = solver.execute(ModelCounterFunction.builder().build());

        System.out.println(modelcount);
        assertThat(backbone.isSat()).isTrue();
        assertThat(backbone.getCompleteBackbone()).isEmpty();
    }

    private SortedSet<Variable> v(final String s) {
        final SortedSet<Variable> vars = new TreeSet<>();
        for (final String name : s.split(" ")) {
            vars.add(f.variable(name));
        }
        return vars;
    }

}