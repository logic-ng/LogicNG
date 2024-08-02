///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.explanations.smus;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;
import static org.logicng.util.CollectionHelper.difference;
import static org.logicng.util.CollectionHelper.nullSafe;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.Handler;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.OptimizationFunction;
import org.logicng.solvers.maxsat.OptimizationConfig;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Computation of a SMUS (smallest MUS, smallest minimal unsatisfiable set).
 * <p>
 * Implementation is based on &quot;Smallest MUS extraction with minimal
 * hitting set dualization&quot; (Ignatiev, Previti, Liffiton, &amp;
 * Marques-Silva, 2015).
 * @version 2.6.0
 * @since 2.0.0
 */
public final class SmusComputation {

    private static final String PROPOSITION_SELECTOR = "@PROPOSITION_SEL_";

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private SmusComputation() {
        // Intentionally left empty
    }

    /**
     * Computes the SMUS for the given list of propositions modulo some additional constraint.
     * @param <P>                   the subtype of the propositions
     * @param propositions          the propositions
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static <P extends Proposition> List<P> computeSmus(
            final List<P> propositions, final List<Formula> additionalConstraints, final FormulaFactory f) {
        final OptimizationConfig cfg = OptimizationConfig.sat(null);
        return computeSmus(propositions, additionalConstraints, f, cfg);
    }

    /**
     * Computes the SMUS for the given list of propositions modulo some additional constraint.
     * <p>
     * The SMUS computation can be called with an {@link OptimizationHandler}. The given handler instance will be
     * used for every subsequent {@link org.logicng.solvers.functions.OptimizationFunction} call and the handler's
     * SAT handler is used for every subsequent SAT call.
     * @param <P>                   the subtype of the propositions
     * @param propositions          the propositions
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @param handler               the handler, can be {@code null}
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static <P extends Proposition> List<P> computeSmus(
            final List<P> propositions,
            final List<Formula> additionalConstraints,
            final FormulaFactory f,
            final OptimizationHandler handler) {
        final OptimizationConfig cfg = OptimizationConfig.sat(handler);
        return computeSmus(propositions, additionalConstraints, f, cfg);
    }

    /**
     * Computes the SMUS for the given list of propositions modulo some additional constraint.
     * <p>
     * The SMUS computation can be called with an {@link OptimizationHandler}. The given handler instance will be used
     * for every subsequent {@link org.logicng.solvers.functions.OptimizationFunction} call and the handler's SAT
     * handler is used for every subsequent SAT call.
     * @param <P>                   the subtype of the propositions
     * @param propositions          the propositions
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @param config                the optimization configuration
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static <P extends Proposition> List<P> computeSmus(
            final List<P> propositions,
            final List<Formula> additionalConstraints,
            final FormulaFactory f,
            final OptimizationConfig config) {
        final Handler handler = getHandler(config);
        start(handler);
        final OptSolver growSolver = OptSolver.create(f, config);
        growSolver.addConstraint(nullSafe(additionalConstraints));
        final Map<Variable, P> propositionMapping = createPropositionsMapping(propositions, growSolver, f);
        final boolean sat = growSolver.sat(propositionMapping.keySet());
        if (sat || growSolver.aborted()) {
            return null;
        }
        final OptSolver hSolver = OptSolver.create(f, config);
        while (true) {
            final SortedSet<Variable> h = hSolver.minimize(propositionMapping.keySet());
            if (h == null || aborted(handler)) {
                return null;
            }
            final SortedSet<Variable> c = grow(growSolver, h, propositionMapping.keySet());
            if (aborted(handler)) {
                return null;
            }
            if (c == null) {
                return h.stream().map(propositionMapping::get).collect(Collectors.toList());
            }
            hSolver.addConstraint(f.or(c));
        }
    }

    /**
     * Computes the SMUS for the given list of formulas and some additional constraints.
     * @param formulas              the formulas
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static List<Formula> computeSmusForFormulas(
            final List<Formula> formulas,
            final List<Formula> additionalConstraints,
            final FormulaFactory f) {
        final OptimizationConfig cfg = OptimizationConfig.sat(null);
        return computeSmusForFormulas(formulas, additionalConstraints, f, cfg);
    }

    /**
     * Computes the SMUS for the given list of formulas and some additional constraints.
     * @param formulas              the formulas
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @param handler               the SMUS handler, can be {@code null}
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static List<Formula> computeSmusForFormulas(
            final List<Formula> formulas,
            final List<Formula> additionalConstraints,
            final FormulaFactory f,
            final OptimizationHandler handler) {
        final OptimizationConfig cfg = OptimizationConfig.sat(handler);
        return computeSmusForFormulas(formulas, additionalConstraints, f, cfg);
    }

    /**
     * Computes the SMUS for the given list of formulas and some additional constraints.
     * @param formulas              the formulas
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @param config                the optimization configuration
     * @return the SMUS or {@code null} if the given propositions are satisfiable or the handler aborted the computation
     */
    public static List<Formula> computeSmusForFormulas(
            final List<Formula> formulas,
            final List<Formula> additionalConstraints,
            final FormulaFactory f,
            final OptimizationConfig config) {
        final List<Proposition> props = formulas.stream().map(StandardProposition::new).collect(Collectors.toList());
        final List<Proposition> smus = computeSmus(props, additionalConstraints, f, config);
        return smus == null ? null : smus.stream().map(Proposition::formula).collect(Collectors.toList());
    }

    private static Handler getHandler(final OptimizationConfig config) {
        return config.getOptimizationType() == OptimizationConfig.OptimizationType.SAT_OPTIMIZATION
                ? config.getOptimizationHandler()
                : config.getMaxSATHandler();
    }

    private static <P extends Proposition> Map<Variable, P> createPropositionsMapping(
            final List<P> propositions, final OptSolver solver, final FormulaFactory f) {
        final Map<Variable, P> propositionMapping = new TreeMap<>();
        for (final P proposition : propositions) {
            final Variable selector = f.variable(PROPOSITION_SELECTOR + propositionMapping.size());
            propositionMapping.put(selector, proposition);
            solver.addConstraint(f.equivalence(selector, proposition.formula()));
        }
        return propositionMapping;
    }

    private static SortedSet<Variable> grow(final OptSolver growSolver, final SortedSet<Variable> h, final Set<Variable> variables) {
        growSolver.saveState();
        growSolver.addConstraint(h);
        final SortedSet<Variable> maxModel = growSolver.maximize(variables);
        if (maxModel == null) {
            return null;
        }
        growSolver.loadState();
        return difference(variables, maxModel, TreeSet::new);
    }

    private abstract static class OptSolver {
        protected final FormulaFactory f;
        protected final OptimizationConfig config;

        OptSolver(final FormulaFactory f, final OptimizationConfig config) {
            this.f = f;
            this.config = config;
        }

        public static OptSolver create(final FormulaFactory f, final OptimizationConfig config) {
            if (config.getOptimizationType() == OptimizationConfig.OptimizationType.SAT_OPTIMIZATION) {
                return new SatOptSolver(f, config);
            } else {
                return new MaxSatOptSolver(f, config);
            }
        }

        abstract void addConstraint(final Formula formula);

        abstract void addConstraint(final Collection<? extends Formula> formulas);

        abstract boolean sat(final Collection<Variable> variables);

        abstract void saveState();

        abstract void loadState();

        abstract SortedSet<Variable> maximize(final Collection<? extends Literal> targetLiterals);

        SortedSet<Variable> minimize(final Collection<? extends Literal> targetLiterals) {
            return maximize(FormulaHelper.negateLiterals(targetLiterals, TreeSet::new));
        }

        abstract boolean aborted();
    }

    private static class SatOptSolver extends OptSolver {
        private final MiniSat solver;
        private SolverState state;

        SatOptSolver(final FormulaFactory f, final OptimizationConfig config) {
            super(f, config);
            this.solver = MiniSat.miniSat(f);
            this.state = null;
        }

        @Override
        void addConstraint(final Formula formula) {
            this.solver.add(formula);
        }

        @Override
        void addConstraint(final Collection<? extends Formula> formulas) {
            this.solver.add(formulas);
        }

        @Override
        boolean sat(final Collection<Variable> variables) {
            final SATHandler satHandler = this.config.getOptimizationHandler() == null ? null : this.config.getOptimizationHandler().satHandler();
            return this.solver.sat(satHandler, variables) == Tristate.TRUE;
        }

        @Override
        void saveState() {
            this.state = this.solver.saveState();
        }

        @Override
        void loadState() {
            if (this.state != null) {
                this.solver.loadState(this.state);
                this.state = null;
            }
        }

        @Override
        SortedSet<Variable> maximize(final Collection<? extends Literal> targetLiterals) {
            final OptimizationFunction optFunction = OptimizationFunction.builder()
                    .handler(this.config.getOptimizationHandler())
                    .literals(targetLiterals)
                    .maximize().build();
            final Assignment model = this.solver.execute(optFunction);
            return model == null || aborted() ? null : new TreeSet<>(model.positiveVariables());
        }

        @Override
        boolean aborted() {
            return Handler.aborted(this.config.getOptimizationHandler());
        }
    }

    private static class MaxSatOptSolver extends OptSolver {
        private List<Formula> constraints;
        private int saveIdx;

        public MaxSatOptSolver(final FormulaFactory f, final OptimizationConfig config) {
            super(f, config);
            this.constraints = new ArrayList<>();
            this.saveIdx = -1;
        }

        @Override
        void addConstraint(final Formula formula) {
            this.constraints.add(formula);
        }

        @Override
        void addConstraint(final Collection<? extends Formula> formulas) {
            this.constraints.addAll(formulas);
        }

        @Override
        boolean sat(final Collection<Variable> variables) {
            final SATHandler satHandler = this.config.getMaxSATHandler() == null ? null : this.config.getMaxSATHandler().satHandler();
            final SATSolver satSolver = MiniSat.miniSat(this.f);
            satSolver.add(this.constraints);
            return satSolver.sat(satHandler, variables) == Tristate.TRUE;
        }

        @Override
        void saveState() {
            this.saveIdx = this.constraints.size();
        }

        @Override
        void loadState() {
            if (this.saveIdx != -1) {
                this.constraints = this.constraints.subList(0, this.saveIdx);
                this.saveIdx = -1;
            }
        }

        @Override
        SortedSet<Variable> maximize(final Collection<? extends Literal> targetLiterals) {
            final MaxSATSolver maxSatSolver = this.config.genMaxSATSolver(this.f);
            this.constraints.forEach(maxSatSolver::addHardFormula);
            for (final Literal lit : targetLiterals) {
                maxSatSolver.addSoftFormula(lit, 1);
            }
            final MaxSATHandler handler = this.config.getMaxSATHandler();
            final MaxSAT.MaxSATResult result = maxSatSolver.solve(handler);
            return result == MaxSAT.MaxSATResult.UNDEF || result == MaxSAT.MaxSATResult.UNSATISFIABLE || aborted()
                    ? null
                    : new TreeSet<>(maxSatSolver.model().positiveVariables());
        }

        @Override
        boolean aborted() {
            return Handler.aborted(this.config.getMaxSATHandler());
        }
    }
}
