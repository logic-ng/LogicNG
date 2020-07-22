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

package org.logicng.solvers;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

import org.logicng.cardinalityconstraints.CCEncoder;
import org.logicng.cardinalityconstraints.CCIncrementalData;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.EncodingResult;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.functions.SolverFunction;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.GlucoseSyrup;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.solvers.sat.MiniSatStyleSolver;
import org.logicng.transformations.cnf.PlaistedGreenbaumTransformationSolver;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Wrapper for the MiniSAT-style SAT solvers.
 * @version 2.0.0
 * @since 1.0
 */
public final class MiniSat extends SATSolver {

    public enum SolverStyle {MINISAT, GLUCOSE, MINICARD}

    protected final MiniSatConfig config;
    protected final MiniSatStyleSolver solver;
    protected final CCEncoder ccEncoder;
    protected final SolverStyle style;
    protected final LNGIntVector validStates;
    protected final boolean initialPhase;
    protected final boolean incremental;
    protected int nextStateId;
    protected final PlaistedGreenbaumTransformationSolver pgTransformation;
    protected final PlaistedGreenbaumTransformationSolver fullPgTransformation;
    protected boolean lastComputationWithAssumptions;

    /**
     * Constructs a new SAT solver instance.
     * @param f             the formula factory
     * @param solverStyle   the solver style
     * @param miniSatConfig the optional MiniSat configuration
     * @param glucoseConfig the optional Glucose configuration
     * @throws IllegalArgumentException if the solver style is unknown
     */
    protected MiniSat(final FormulaFactory f, final SolverStyle solverStyle, final MiniSatConfig miniSatConfig,
                      final GlucoseConfig glucoseConfig) {
        super(f);
        this.config = miniSatConfig;
        this.style = solverStyle;
        this.initialPhase = miniSatConfig.initialPhase();
        switch (solverStyle) {
            case MINISAT:
                this.solver = new MiniSat2Solver(miniSatConfig);
                break;
            case GLUCOSE:
                this.solver = new GlucoseSyrup(miniSatConfig, glucoseConfig);
                break;
            case MINICARD:
                this.solver = new MiniCard(miniSatConfig);
                break;
            default:
                throw new IllegalArgumentException("Unknown solver style: " + solverStyle);
        }
        this.result = UNDEF;
        this.incremental = miniSatConfig.incremental();
        this.validStates = new LNGIntVector();
        this.nextStateId = 0;
        this.ccEncoder = new CCEncoder(f);
        this.pgTransformation = new PlaistedGreenbaumTransformationSolver(true, this.underlyingSolver(), this.initialPhase);
        this.fullPgTransformation = new PlaistedGreenbaumTransformationSolver(false, this.underlyingSolver(), this.initialPhase);
    }

    /**
     * Returns a new MiniSat solver.
     * @param f the formula factory
     * @return the solver
     */
    public static MiniSat miniSat(final FormulaFactory f) {
        return new MiniSat(f, SolverStyle.MINISAT, MiniSatConfig.builder().build(), null);
    }

    /**
     * Returns a new MiniSat solver with a given configuration.
     * @param f      the formula factory
     * @param config the configuration
     * @return the solver
     */
    public static MiniSat miniSat(final FormulaFactory f, final MiniSatConfig config) {
        return new MiniSat(f, SolverStyle.MINISAT, config, null);
    }

    /**
     * Returns a new Glucose solver.
     * @param f the formula factory
     * @return the solver
     */
    public static MiniSat glucose(final FormulaFactory f) {
        return new MiniSat(f, SolverStyle.GLUCOSE, MiniSatConfig.builder().build(), GlucoseConfig.builder().build());
    }

    /**
     * Returns a new Glucose solver with a given configuration.
     * @param f             the formula factory
     * @param miniSatConfig the MiniSat configuration
     * @param glucoseConfig the Glucose configuration
     * @return the solver
     */
    public static MiniSat glucose(final FormulaFactory f, final MiniSatConfig miniSatConfig,
                                  final GlucoseConfig glucoseConfig) {
        return new MiniSat(f, SolverStyle.GLUCOSE, miniSatConfig, glucoseConfig);
    }

    /**
     * Returns a new MiniCard solver.
     * @param f the formula factory
     * @return the solver
     */
    public static MiniSat miniCard(final FormulaFactory f) {
        return new MiniSat(f, SolverStyle.MINICARD, MiniSatConfig.builder().build(), null);
    }

    /**
     * Returns a new MiniCard solver with a given configuration.
     * @param f      the formula factory
     * @param config the configuration
     * @return the solver
     */
    public static MiniSat miniCard(final FormulaFactory f, final MiniSatConfig config) {
        return new MiniSat(f, SolverStyle.MINICARD, config, null);
    }

    @Override
    public void add(final Formula formula, final Proposition proposition) {
        this.result = UNDEF;
        if (formula.type() == FType.PBC) {
            final PBConstraint constraint = (PBConstraint) formula;
            if (constraint.isCC()) {
                if (this.style == SolverStyle.MINICARD) {
                    if (constraint.comparator() == CType.LE) {
                        ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
                    } else if (constraint.comparator() == CType.LT && constraint.rhs() > 3) {
                        ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs() - 1);
                    } else if (constraint.comparator() == CType.EQ && constraint.rhs() == 1) {
                        ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
                        this.solver.addClause(generateClauseVector(Arrays.asList(constraint.operands())), proposition);
                    } else {
                        addFormulaAsCNF(constraint, proposition);
                    }
                } else {
                    final EncodingResult result = EncodingResult.resultForMiniSat(this.f, this, proposition);
                    this.ccEncoder.encode((CardinalityConstraint) constraint, result);
                }
            } else {
                addFormulaAsCNF(constraint, proposition);
            }
        } else {
            addFormulaAsCNF(formula, proposition);
        }
    }

    protected void addFormulaAsCNF(final Formula formula, final Proposition proposition) {
        if (this.config.getCnfMethod() == MiniSatConfig.CNFMethod.FACTORY_CNF) {
            this.addClauseSet(formula.cnf(), proposition);
        } else if (this.config.getCnfMethod() == MiniSatConfig.CNFMethod.PG_ON_SOLVER) {
            this.pgTransformation.addCNFtoSolver(formula, proposition);
        } else if (this.config.getCnfMethod() == MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER) {
            this.fullPgTransformation.addCNFtoSolver(formula, proposition);
        } else {
            throw new IllegalStateException("Unknown Solver CNF method: " + this.config.getCnfMethod());
        }
    }

    @Override
    public void addWithoutUnknown(final Formula formula) {
        final int nVars = this.solver.nVars();
        final Assignment restriction = new Assignment(true);
        final Map<String, Integer> map = this.solver.name2idx();
        for (final Variable var : formula.variables()) {
            final Integer index = map.get(var.name());
            if (index == null || index >= nVars) {
                restriction.addLiteral(var.negate());
            }
        }
        this.add(formula.restrict(restriction));
    }

    @Override
    public CCIncrementalData addIncrementalCC(final CardinalityConstraint cc) {
        final EncodingResult result = EncodingResult.resultForMiniSat(this.f, this, null);
        return this.ccEncoder.encodeIncremental(cc, result);
    }

    @Override
    protected void addClause(final Formula formula, final Proposition proposition) {
        this.result = UNDEF;
        final LNGIntVector ps = generateClauseVector(formula.literals());
        this.solver.addClause(ps, proposition);
    }

    @Override
    protected void addClauseWithRelaxation(final Variable relaxationVar, final Formula formula) {
        this.result = UNDEF;
        final SortedSet<Literal> literals = new TreeSet<>(formula.literals());
        literals.add(relaxationVar);
        this.solver.addClause(generateClauseVector(literals), null);
    }

    @Override
    public Tristate sat(final SATHandler handler) {
        if (lastResultIsUsable()) {
            return this.result;
        }
        this.result = this.solver.solve(handler);
        this.lastComputationWithAssumptions = false;
        return this.result;
    }

    @Override
    public Tristate sat(final SATHandler handler, final Literal literal) {
        final LNGIntVector clauseVec = new LNGIntVector(1);
        final int index = getOrAddIndex(literal);
        final int litNum = literal.phase() ? index * 2 : (index * 2) ^ 1;
        clauseVec.push(litNum);
        this.result = this.solver.solve(handler, clauseVec);
        this.lastComputationWithAssumptions = true;
        return this.result;
    }

    @Override
    public Tristate sat(final SATHandler handler, final Collection<? extends Literal> assumptions) {
        final LNGIntVector assumptionVec = generateClauseVector(assumptions);
        this.result = this.solver.solve(handler, assumptionVec);
        this.lastComputationWithAssumptions = true;
        return this.result;
    }

    @Override
    public void reset() {
        this.solver.reset();
        this.lastComputationWithAssumptions = false;
        this.pgTransformation.clearCache();
        this.fullPgTransformation.clearCache();
        this.result = UNDEF;
    }

    @Override
    public Assignment model(final Collection<Variable> variables) {
        if (this.result == UNDEF) {
            throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'sat' first.");
        }
        final LNGIntVector relevantIndices = variables == null ? null : new LNGIntVector(variables.size());
        if (relevantIndices != null) {
            for (final Variable var : variables) {
                relevantIndices.push(this.solver.idxForName(var.name()));
            }
        }
        return this.result == TRUE ? this.createAssignment(this.solver.model(), relevantIndices) : null;
    }

    @Override
    public <RESULT> RESULT execute(final SolverFunction<RESULT> function) {
        return function.apply(this, this::setResult);
    }

    @Override
    public SolverState saveState() {
        final int id = this.nextStateId++;
        this.validStates.push(id);
        return new SolverState(id, this.solver.saveState());
    }

    @Override
    public void loadState(final SolverState state) {
        int index = -1;
        for (int i = this.validStates.size() - 1; i >= 0 && index == -1; i--) {
            if (this.validStates.get(i) == state.id()) {
                index = i;
            }
        }
        if (index == -1) {
            throw new IllegalArgumentException("The given solver state is not valid anymore.");
        }
        this.validStates.shrinkTo(index + 1);
        this.solver.loadState(state.state());
        this.result = UNDEF;
        this.pgTransformation.clearCache();
        this.fullPgTransformation.clearCache();
    }

    @Override
    public SortedSet<Variable> knownVariables() {
        final SortedSet<Variable> result = new TreeSet<>();
        final int nVars = this.solver.nVars();
        for (final Map.Entry<String, Integer> entry : this.solver.name2idx().entrySet()) {
            if (entry.getValue() < nVars) {
                result.add(this.f.variable(entry.getKey()));
            }
        }
        return result;
    }

    /**
     * Generates a clause vector of a collection of literals.
     * @param literals the literals
     * @return the clause vector
     */
    protected LNGIntVector generateClauseVector(final Collection<? extends Literal> literals) {
        final LNGIntVector clauseVec = new LNGIntVector(literals.size());
        for (final Literal lit : literals) {
            final int index = getOrAddIndex(lit);
            final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
            clauseVec.push(litNum);
        }
        return clauseVec;
    }

    protected int getOrAddIndex(final Literal lit) {
        int index = this.solver.idxForName(lit.name());
        if (index == -1) {
            index = this.solver.newVar(!this.initialPhase, true);
            this.solver.addName(lit.name(), index);
        }
        return index;
    }

    /**
     * Creates an assignment from a Boolean vector of the solver.
     * @param vec             the vector of the solver
     * @param relevantIndices the solver's indices of the relevant variables for the model.  If {@code null}, all
     *                        variables are relevant.
     * @return the assignment
     */
    public Assignment createAssignment(final LNGBooleanVector vec, final LNGIntVector relevantIndices) {
        final Assignment model = new Assignment();
        if (relevantIndices == null) {
            for (int i = 0; i < vec.size(); i++) {
                final String name = this.solver.nameForIdx(i);
                if (isRelevantVariable(name)) {
                    model.addLiteral(this.f.literal(name, vec.get(i)));
                }
            }
        } else {
            for (int i = 0; i < relevantIndices.size(); i++) {
                final int index = relevantIndices.get(i);
                if (index != -1) {
                    final String name = this.solver.nameForIdx(index);
                    if (isRelevantVariable(name)) {
                        model.addLiteral(this.f.literal(name, vec.get(index)));
                    }
                }
            }
        }
        return model;
    }

    /**
     * Returns whether a variable on the solver is relevant or an auxiliary variable.
     * @param name the name of the variable
     * @return true if it is relevant, false if it is an auxiliary variable
     */
    public boolean isRelevantVariable(final String name) {
        return this.config.isAuxiliaryVariablesInModels() || (!name.startsWith(FormulaFactory.CNF_PREFIX) &&
                !name.startsWith(FormulaFactory.CC_PREFIX) && !name.startsWith(FormulaFactory.PB_PREFIX));
    }

    /**
     * Returns the underlying core solver.
     * <p>
     * ATTENTION: by influencing the underlying solver directly, you can mess things up completely!  You should really
     * know, what you are doing.
     * @return the underlying core solver
     */
    public MiniSatStyleSolver underlyingSolver() {
        return this.solver;
    }

    /**
     * Returns the initial phase of literals of this solver.
     * @return the initial phase of literals of this solver
     */
    public boolean initialPhase() {
        return this.initialPhase;
    }

    @Override
    public String toString() {
        return String.format("%s{result=%s, incremental=%s}", this.solver.getClass().getSimpleName(), this.result, this.incremental);
    }

    protected boolean lastResultIsUsable() {
        return this.result != UNDEF && !this.lastComputationWithAssumptions;
    }

    /**
     * Returns this solver's configuration.
     * @return this solver's configuration
     */
    public MiniSatConfig getConfig() {
        return this.config;
    }

    @Override
    public void setSelectionOrder(final List<? extends Literal> selectionOrder) {
        this.solver.setSelectionOrder(selectionOrder);
    }

    @Override
    public void resetSelectionOrder() {
        this.solver.resetSelectionOrder();
    }

    /**
     * Returns this solver's style.
     * @return this solver's style
     */
    public SolverStyle getStyle() {
        return this.style;
    }

    /**
     * Returns whether this solver is incremental
     * @return {@code true} if this solver is incremental, {@code false} otherwise
     */
    public boolean isIncremental() {
        return this.incremental;
    }

    /**
     * Returns the current result, e.g. the result of the last {@link #sat()} call.
     * @return the current result
     */
    public Tristate getResult() {
        return this.result;
    }

    protected void setResult(final Tristate tristate) {
        this.result = tristate;
    }

    /**
     * Returns whether the last computation was using assumption literals.
     * @return {@code true} if the last computation used assumption literals, {@code false} otherwise
     */
    public boolean isLastComputationWithAssumptions() {
        return this.lastComputationWithAssumptions;
    }
}
