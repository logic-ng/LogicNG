package org.logicng.explanations.backbones;

import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

public class BackboneGeneration {

    private final BackboneConfig configuration;
    private final boolean useCore;

    public BackboneGeneration() {
        this(new BackboneConfig.Builder().build());
    }

    public BackboneGeneration(final FormulaFactory factory) {
        this((BackboneConfig) factory.configurationFor(ConfigurationType.BACKBONE));
    }

    public BackboneGeneration(final BackboneConfig configuration) {
        this.configuration = configuration == null ? new BackboneConfig.Builder().build() : configuration;
        this.useCore = this.configuration.algorithm == BackboneConfig.Algorithm.CORE;
    }

    public Backbone computeBackbone(final Formula formula) {
        return computeBackbone(formula, formula.variables());
    }

    public Backbone computeBackbone(final Formula formula, final Collection<Variable> variables) {
        final MiniSatConfig config = this.useCore ? new MiniSatConfig.Builder().proofGeneration(true).build() : new MiniSatConfig.Builder().build();
        final SATSolver solver = MiniSat.miniSat(formula.factory(), config);
        solver.add(formula);
        return computeBackbone(solver, variables);
    }

    public Backbone computeBackbone(final Collection<Formula> formulas) {
        final SortedSet<Variable> variables = new TreeSet<>();
        for (final Formula formula : formulas) {
            variables.addAll(formula.variables());
        }
        return computeBackbone(formulas, variables);
    }

    public Backbone computeBackbone(final Collection<Formula> formulas, final Collection<Variable> variables) {
        if (formulas.isEmpty()) {
            return new Backbone();
        }
        final FormulaFactory f = formulas.iterator().next().factory();
        final MiniSatConfig config = this.useCore ? new MiniSatConfig.Builder().proofGeneration(true).build() : new MiniSatConfig.Builder().build();
        final SATSolver solver = MiniSat.miniSat(f, config);
        solver.add(formulas);
        return computeBackbone(solver, variables);
    }

    // Solver state before and after method must be equal
    public Backbone computeBackbone(final SATSolver solver, final Collection<Variable> variables) {
        final SolverState originalState = solver.saveState();
        final Backbone backbone;
        switch (this.configuration.algorithm) {
            case ENUMERATION:
                backbone = null;
                break;
            case ITERATIVE_PLAIN:
                backbone = null;
                break;
            case ITERATIVE_ONE_TEST:
                backbone = null;
                break;
            case ITERATIVE_PLUS:
                backbone = null;
                break;
            case CHUNKING:
                backbone = null;
                break;
            case CORE:
                backbone = null;
                break;
            default:
                backbone = null;
        }
        solver.loadState(originalState);
        return backbone;
    }
}
