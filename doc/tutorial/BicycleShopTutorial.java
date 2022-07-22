package org.logicng.tutorial;

import org.logicng.backbones.Backbone;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.functions.VariableProfileFunction;
import org.logicng.propositions.ExtendedProposition;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.PropositionBackpack;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.OptimizationFunction;
import org.logicng.solvers.sat.MiniSatConfig;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class BicycleShopTutorial {

    public static void main(final String[] args) {
        // Chapter 1
        final BicycleShopData data = new BicycleShopData();

        // Chapter 2
        final List<Formula> substitutedFormulas = substituteLiterals(data);
        final List<Formula> restrictedFormulas = restrictRuleset(data, substitutedFormulas);

        // Chapter 3
        computeVariableOccurrences(data, restrictedFormulas);

        // Chapter 4
        final List<Proposition> propositions = getPropositions(data, restrictedFormulas);

        // Chapter 5
        satSolvingAssumptions(data, propositions);
        computeUnsatCore(data, propositions);
        computeBackbone(data, propositions);
        modelEnumeration(data, propositions);
        modelEnumerationProjectedAndRestricted(data, propositions);
        computeBikeWithMinMaxNumberOfFeatures(data, propositions);

        // Chapter 6
        computeBikeWithMinNumberOfFeaturesMaxSat(data, propositions);
        computeCheapestBike(data, propositions, data.f.variable("wf27"));
    }

    public static List<Formula> substituteLiterals(final BicycleShopData data) {
        final FormulaFactory f = data.f;
        final Substitution substitution = new Substitution();
        substitution.addMapping(f.variable("wf1"), f.variable("wf24"));
        substitution.addMapping(f.variable("wf2"), f.variable("wf26"));
        substitution.addMapping(f.variable("wf3"), f.variable("wf27"));
        substitution.addMapping(f.variable("wf4"), f.variable("wf29"));
        substitution.addMapping(f.variable("wf5"), f.variable("wf32"));

        substitution.addMapping(f.variable("wb1"), f.variable("wb24"));
        substitution.addMapping(f.variable("wb2"), f.variable("wb26"));
        substitution.addMapping(f.variable("wb3"), f.variable("wb27"));
        substitution.addMapping(f.variable("wb4"), f.variable("wb29"));
        substitution.addMapping(f.variable("wb5"), f.variable("wb32"));

        final List<Formula> substituted = data.formulas.stream().map(formula -> formula.substitute(substitution)).collect(Collectors.toList());
        System.out.println("\nSubstituted formulas:");
        System.out.println("=====================");
        substituted.forEach(System.out::println);
        return substituted;
    }

    public static List<Formula> restrictRuleset(final BicycleShopData data, final List<Formula> formulas) {
        final Assignment assignment = new Assignment();

        assignment.addLiteral(data.f.variable("wb24").negate());
        assignment.addLiteral(data.c3.negate()); // cheap color sold out
        assignment.addLiteral(data.r4);

        final List<Formula> restricted = formulas.stream().map(x -> x.restrict(assignment)).collect(Collectors.toList());
        System.out.println("\nRestricted formulas:");
        System.out.println("====================");
        restricted.forEach(System.out::println);
        return restricted;
    }

    public static void computeVariableOccurrences(final BicycleShopData data, final List<Formula> formulas) {
        final Map<Variable, Integer> variables2Occurrences = data.f.and(formulas).apply(new VariableProfileFunction());
        final SortedMap<Integer, List<Variable>> occurrences2Vars = new TreeMap<>();
        variables2Occurrences.forEach((var, occ) -> occurrences2Vars.computeIfAbsent(occ, y -> new ArrayList<>()).add(var));
        System.out.println("\nVariable Occurrences:");
        System.out.println("=====================");
        occurrences2Vars.forEach((occ, var) -> System.out.printf("%s: %d%n", var, occ));
    }

    public static List<Proposition> getPropositions(final BicycleShopData data, final List<Formula> formulas) {
        long id = 0;
        final List<Proposition> propositions = new ArrayList<>();
        for (final Formula formula : formulas) {
            final LocalDate date;
            if (formula.variables().contains(data.s4)) {
                // formulas for the pro saddle
                date = LocalDate.of(2021, 1, 1);
            } else if (formula.variables().contains(data.b2)) {
                // formulas for the metal strip bike bell
                date = LocalDate.of(2022, 1, 1);
            } else if (formula.type() == FType.PBC) {
                // exo and amo constraints
                date = LocalDate.of(2020, 1, 1);
            } else {
                // implications and exclusions
                date = LocalDate.of(2020, 6, 1);
            }
            final Proposition proposition = new ExtendedProposition<>(new MyBackpack(id, date), formula);
            propositions.add(proposition);
            id++;
        }
        System.out.println("\nPropositions:");
        System.out.println("=============");
        propositions.forEach(System.out::println);
        return propositions;
    }

    public static void satSolvingAssumptions(final BicycleShopData data, final List<Proposition> propositions) {
        final SATSolver solver = MiniSat.miniSat(data.f);
        propositions.forEach(solver::add);

        // Sarah's configuration
        final List<Variable> wishedFeaturesSarah = new ArrayList<>(Arrays.asList(data.f1, data.c2, data.h2, data.f.variable("wf26"), data.b3));
        Tristate sat = solver.sat(wishedFeaturesSarah);
        System.out.println("\nSarah's configuration is: " + sat);

        // Check if Sarah's configuration is complete
        wishedFeaturesSarah.addAll(
                Arrays.asList(data.frame, data.handlebar, data.saddle, data.frontWheel, data.backWheel, data.luggageRack, data.color, data.bell));
        System.out.println("\nViolated formulas from Sarah's configuration are: ");
        final Assignment assignment = new Assignment(wishedFeaturesSarah);
        for (final Formula formula : propositions.stream().map(Proposition::formula).collect(Collectors.toList())) {
            final boolean isViolated = !formula.evaluate(assignment);
            if (isViolated) {
                System.out.println(formula);
            }
        }

        // Clemens' configuration
        final List<Literal> wishedFeaturesClemens = Arrays.asList(data.f1, data.c1, data.s2, data.f.variable("wf27"), data.b2);
        sat = solver.sat(wishedFeaturesClemens);
        System.out.println("\nClemens' configuration is: " + sat);
    }

    public static void computeUnsatCore(final BicycleShopData data, final List<Proposition> propositions) {
        final SATSolver solver = MiniSat.miniSat(data.f, MiniSatConfig.builder().proofGeneration(true).build());
        propositions.forEach(solver::add);
        final SolverState initialState = solver.saveState();

        final List<Literal> wishedFeaturesClemens = Arrays.asList(data.f1, data.c1, data.s2, data.f.variable("wf27"), data.b2);
        wishedFeaturesClemens.forEach(feature -> solver.add(new StandardProposition("Order", feature)));
        solver.sat(); // the sat-call must be 'FALSE' for computing the unsat core

        final UNSATCore<Proposition> unsatCore = solver.unsatCore();
        System.out.println("\nExplanation for the conflict");
        System.out.println("============================");
        unsatCore.propositions().forEach(System.out::println);

        // Resetting the solver and checking the new configuration
        solver.loadState(initialState);
        final List<Literal> wishedFeaturesFixed = Arrays.asList(data.f1, data.c1, data.s2, data.f.variable("wf28"));
        solver.add(wishedFeaturesFixed);
        final Tristate sat = solver.sat();
        System.out.println("\nClemens' new configuration is: " + sat);
    }

    public static void computeBackbone(final BicycleShopData data, final List<Proposition> propositions) {
        final MiniSat solver = MiniSat.miniSat(data.f);
        propositions.forEach(solver::add);
        solver.add(data.f.variable("wb32"));
        final List<Variable> variablesInFormula =
                propositions.stream().map(proposition -> proposition.formula().variables()).flatMap(Collection::stream).collect(Collectors.toList());
        final Backbone backbone = solver.backbone(variablesInFormula);
        System.out.println("\nBackbone of the ruleset:");
        System.out.println("========================");
        System.out.println("Positive: " + backbone.getPositiveBackbone());
        System.out.println("Negative: " + backbone.getNegativeBackbone());
    }

    private static void modelEnumeration(final BicycleShopData data, final List<Proposition> propositions) {
        final SATSolver solver = MiniSat.miniSat(data.f);
        propositions.forEach(solver::add);
        solver.sat();

        final Assignment aModel = solver.model();
        System.out.printf("\nA model: %s", aModel);
        System.out.println("\n=====================");

        final List<Assignment> allModels = solver.enumerateAllModels();
        System.out.printf("\n# of all models: %d", allModels.size());
        System.out.println("\n=====================");

        System.out.println("\nAll models: ");
        System.out.println("=====================");
        // allModels.forEach(System.out::println);
    }

    private static void modelEnumerationProjectedAndRestricted(final BicycleShopData data, final List<Proposition> propositions) {
        final SATSolver solver = MiniSat.miniSat(data.f);
        propositions.forEach(solver::add);
        solver.add(data.f.variable("wf27"));
        solver.add(data.f1);
        solver.add(data.b3);
        solver.add(data.c1);
        solver.sat();
        final List<Assignment> models = solver.enumerateAllModels();

        System.out.println("\nAll models restricted: ");
        System.out.println("=====================");
        models.forEach(System.out::println);

        final List<Variable> variables = Arrays.asList(data.s1, data.s2, data.s3, data.s4);
        final List<Assignment> modelsProjected = solver.enumerateAllModels(variables);

        System.out.println("\nAll models projected: ");
        System.out.println("=====================");
        modelsProjected.forEach(System.out::println);
    }

    private static void computeBikeWithMinMaxNumberOfFeatures(final BicycleShopData data, final List<Proposition> propositions) {
        final SATSolver solver = MiniSat.miniSat(data.f);
        propositions.forEach(solver::add);

        final SortedSet<Variable> vars = propositions.stream() // all relevant features (without feature classes)
                .map(p -> p.formula().variables())
                .flatMap(Collection::stream)
                .filter(v -> !data.featureClasses.contains(v))
                .collect(Collectors.toCollection(TreeSet::new));

        final Assignment bikeWithMinNumberOfFeatures = solver.execute(OptimizationFunction.builder().literals(vars).minimize().build());
        final Assignment bikeWithMaxNumberOfFeatures = solver.execute(OptimizationFunction.builder().literals(vars).maximize().build());

        System.out.println(bikeWithMinNumberOfFeatures);
        System.out.println(bikeWithMaxNumberOfFeatures);

        System.out.println("\nMinimum number of features: " + bikeWithMinNumberOfFeatures.positiveVariables().size());
        System.out.println("Maximum number of features: " + bikeWithMaxNumberOfFeatures.positiveVariables().size());
    }

    public static void computeBikeWithMinNumberOfFeaturesMaxSat(final BicycleShopData data, final List<Proposition> propositions) {
        final MaxSATSolver solver = MaxSATSolver.msu3();
        propositions.forEach(x -> solver.addHardFormula(x.formula()));

        final SortedSet<Variable> vars = propositions.stream() // all relevant features (without feature classes)
                .map(p -> p.formula().variables())
                .flatMap(Collection::stream)
                .filter(v -> !data.featureClasses.contains(v))
                .collect(Collectors.toCollection(TreeSet::new));

        vars.forEach(v -> solver.addSoftFormula(v.negate(), 1));
        solver.solve();
        final Assignment model = solver.model();

        System.out.println("\nMinimum number of features: " + solver.result());
        System.out.println("=====================");
        System.out.println(model);
    }

    public static void computeCheapestBike(final BicycleShopData data, final List<Proposition> propositions, final Variable chosenFrontWheel) {
        final MaxSATSolver solver = MaxSATSolver.wbo();
        propositions.forEach(proposition -> solver.addHardFormula(proposition.formula()));
        solver.addHardFormula(chosenFrontWheel);

        solver.addSoftFormula(data.f1.negate(), 1500);
        solver.addSoftFormula(data.f2.negate(), 900);
        solver.addSoftFormula(data.f3.negate(), 400);

        solver.addSoftFormula(data.h1.negate(), 380);
        solver.addSoftFormula(data.h2.negate(), 200);
        solver.addSoftFormula(data.h3.negate(), 120);
        solver.addSoftFormula(data.h4.negate(), 300);

        solver.addSoftFormula(data.s1.negate(), 200);
        solver.addSoftFormula(data.s2.negate(), 240);
        solver.addSoftFormula(data.s3.negate(), 160);
        solver.addSoftFormula(data.s4.negate(), 300);

        solver.addSoftFormula(data.r1.negate(), 130);
        solver.addSoftFormula(data.r2.negate(), 120);
        solver.addSoftFormula(data.r3.negate(), 20);
        solver.addSoftFormula(data.r4.negate(), 100);

        solver.solve();
        final Assignment model = solver.model();
        System.out.println("\nCheapest bike: " + solver.result());
        System.out.println("=====================");
        System.out.println(model);
    }

    static class MyBackpack implements PropositionBackpack {
        private final long id;
        private final LocalDate date;

        MyBackpack(final long id, final LocalDate date) {
            this.id = id;
            this.date = date;
        }

        @Override
        public String toString() {
            return "MyBackpack{" +
                    "id=" + this.id +
                    ", date=" + this.date +
                    '}';
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            final MyBackpack that = (MyBackpack) o;
            return this.id == that.id && Objects.equals(this.date, that.date);
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.id, this.date);
        }
    }
}
