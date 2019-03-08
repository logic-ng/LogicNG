//package org.logicng.explanations.backbones;
//
//import org.logicng.configurations.ConfigurationType;
//import org.logicng.formulas.Formula;
//import org.logicng.formulas.FormulaFactory;
//import org.logicng.formulas.Variable;
//import org.logicng.solvers.MiniSat;
//import org.logicng.solvers.SATSolver;
//import org.logicng.solvers.sat.MiniSatConfig;
//
//import java.util.Collection;
//import java.util.SortedSet;
//import java.util.TreeSet;
//
///**
// * Computes a Backbone of a given formula with different algorithms.
// * @version 1.5
// * @since 1.5
// */
//public class BackboneGeneration {
//
//    private final BackboneConfig configuration;
////    private final boolean useCore;
//
//    /**
//     * Constructs a new Backbone generator.
//     */
//    public BackboneGeneration() {
//        this(new BackboneConfig.Builder().build());
//    }
//
//    /**
//     * Constructs a new Backbone generator for a given formula factory.
//     * @param factory   the given formula factory
//     */
//    public BackboneGeneration(final FormulaFactory factory) {
//        this((BackboneConfig) factory.configurationFor(ConfigurationType.BACKBONE));
//    }
//
//    /**
//     * Constructs a new Backbone generator for a given configuration.
//     * @param configuration given Backbone configuration
//     */
//    public BackboneGeneration(final BackboneConfig configuration) {
//        // TODO catch if config does not have proof generation turned on when algorithm uses cores
//        this.configuration = configuration == null ? new BackboneConfig.Builder().build() : configuration;
////        this.useCore = this.configuration.algorithm == BackboneConfig.Algorithm.CORE ||
////                this.configuration.algorithm == BackboneConfig.Algorithm.CORE_CHUNKING;
//    }
//
//    /**
//     * Computes the Backbone for the given formula.
//     * @param formula   the given formula
//     * @return the Backbone of the given formula
//     */
//    public Backbone computeBackbone(final Formula formula) {
//        return computeBackbone(formula, formula.variables());
//    }
//
//    /**
//     * Computes the Backbone for the given formula considering only the provided variables.
//     * @param formula   the given formula
//     * @param variables the given variables
//     * @return the Backbone of the given formula under consideration of only the given variables
//     */
//    public Backbone computeBackbone(final Formula formula, final Collection<Variable> variables) {
////        final MiniSatConfig config = this.useCore ? new MiniSatConfig.Builder().proofGeneration(true).build() : new MiniSatConfig.Builder().build();
//        final MiniSatConfig config = new MiniSatConfig.Builder().build();
//        final SATSolver solver = MiniSat.miniSat(formula.factory(), config);
//        solver.add(formula);
//        return computeBackbone(solver, variables);
//    }
//
//    /**
//     * Computes the Backbone for the given formulas.
//     * @param formulas   the given formulas
//     * @return the Backbone of the given formulas
//     */
//    public Backbone computeBackbone(final Collection<Formula> formulas) {
//        final SortedSet<Variable> variables = new TreeSet<>();
//        for (final Formula formula : formulas) {
//            variables.addAll(formula.variables());
//        }
//        return computeBackbone(formulas, variables);
//    }
//
//    /**
//     * Computes the Backbone for the given formulas considering only the provided variables.
//     * @param formulas   the given formulas
//     * @param variables the given variables
//     * @return the Backbone of the given formulas under consideration of only the given variables
//     */
//    public Backbone computeBackbone(final Collection<Formula> formulas, final Collection<Variable> variables) {
//        if (formulas.isEmpty()) {
//            return new Backbone();
//        }
//        final FormulaFactory f = formulas.iterator().next().factory();
////        final MiniSatConfig config = this.useCore ? new MiniSatConfig.Builder().proofGeneration(true).build() : new MiniSatConfig.Builder().build();
//        final MiniSatConfig config = new MiniSatConfig.Builder().build();
//        final SATSolver solver = MiniSat.miniSat(f, config);
//        solver.add(formulas);
//        return computeBackbone(solver, variables);
//    }
//
//    /**
//     * Computes the Backbone for the formulas on the provided SAT solver using the set algorithm considering only the
//     * given variables.
//     * @param solver   the given SAT solver
//     * @param variables the given variables
//     * @return the Backbone of the formulas on the SAT solver under consideration of only the given variables
//     */
//    public Backbone computeBackbone(final SATSolver solver, final Collection<Variable> variables) {
////        final SolverState originalState = solver.saveState();
////        final Backbone backbone;
////        switch (this.configuration.algorithm) {
////            case ENUMERATION:
////                backbone = new EnumerationAlgorithm().computeBackbone(solver, variables); // TODO initialize EnumerationAlgorithm() already in constructor?!
////                break;
////            case ITERATIVE_PLAIN:
////                backbone = new IterativePlainAlgorithm().computeBackbone(solver, variables);
////                break;
////            case ITERATIVE_ONE_TEST:
////                backbone = new IterativeOneTestAlgorithm().computeBackbone(solver, variables);
////                break;
////            case ITERATIVE_PLUS:
////                backbone = new IterativePlusAlgorithm().computeBackbone(solver, variables);
////                break;
////            case CHUNKING:
////                backbone = new ChunkingAlgorithm().computeBackbone(solver, variables);
////                break;
////            case CORE:
////                backbone = new CoreAlgorithm().computeBackbone(solver, variables);
////                break;
////            case CORE_CHUNKING:
////                backbone = new CoreChunkingAlgorithm().computeBackbone(solver, variables);
////                break;
////            default:
////                backbone = new IterativePlainAlgorithm().computeBackbone(solver, variables);
////        }
////        solver.loadState(originalState);
////        return backbone;
//        return new Backbone();
//    }
//}
