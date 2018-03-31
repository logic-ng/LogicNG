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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.logicng.cardinalityconstraints.CCEncoder;
import org.logicng.cardinalityconstraints.CCIncrementalData;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.EncodingResult;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.unsatcores.UNSATCore;
import org.logicng.explanations.unsatcores.drup.DRUPTrim;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.GlucoseSyrup;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * Wrapper for the MiniSAT-style SAT solvers.
 * @version 1.3.1
 * @since 1.0
 */
public final class MiniSat extends SATSolver {

  private enum SolverStyle {MINISAT, GLUCOSE, MINICARD}

  private final MiniSatConfig config;
  private final MiniSatStyleSolver solver;
  private final CCEncoder ccEncoder;
  private final SolverStyle style;
  private final LNGIntVector validStates;
  private final boolean initialPhase;
  private final boolean incremental;
  private int nextStateId;

  /**
   * Constructs a new SAT solver instance.
   * @param f           the formula factory
   * @param solverStyle the solver style
   * @throws IllegalArgumentException if the solver style is unknown
   */
  private MiniSat(final FormulaFactory f, final SolverStyle solverStyle, final MiniSatConfig miniSatConfig,
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
  }

  /**
   * Returns a new MiniSat solver.
   * @param f the formula factory
   * @return the solver
   */
  public static MiniSat miniSat(final FormulaFactory f) {
    return new MiniSat(f, SolverStyle.MINISAT, new MiniSatConfig.Builder().build(), null);
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
    return new MiniSat(f, SolverStyle.GLUCOSE, new MiniSatConfig.Builder().build(), new GlucoseConfig.Builder().build());
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
    return new MiniSat(f, SolverStyle.MINICARD, new MiniSatConfig.Builder().build(), null);
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
    if (formula.type() == FType.PBC) {
      final PBConstraint constraint = (PBConstraint) formula;
      this.result = UNDEF;
      if (constraint.isCC()) {
        if (this.style == SolverStyle.MINICARD) {
          if (constraint.comparator() == CType.LE)
            ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
          else if (constraint.comparator() == CType.LT && constraint.rhs() > 3)
            ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs() - 1);
          else if (constraint.comparator() == CType.EQ && constraint.rhs() == 1) {
            ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
            this.solver.addClause(generateClauseVector(Arrays.asList(constraint.operands())), proposition);
          } else
            this.addClauseSet(constraint.cnf(), proposition);
        } else {
          final EncodingResult result = EncodingResult.resultForMiniSat(this.f, this);
          this.ccEncoder.encode(constraint, result);
        }
      } else
        this.addClauseSet(constraint.cnf(), proposition);
    } else
      this.addClauseSet(formula.cnf(), proposition);
  }

  @Override
  public void addWithoutUnknown(final Formula formula) {
    final int nVars = this.solver.nVars();
    final Assignment restriction = new Assignment(true);
    final Map<String, Integer> map = this.solver.name2idx();
    for (final Variable var : formula.variables()) {
      final Integer index = map.get(var.name());
      if (index == null || index >= nVars)
        restriction.addLiteral(var.negate());
    }
    this.add(formula.restrict(restriction));
  }

  @Override
  public CCIncrementalData addIncrementalCC(final PBConstraint cc) {
    if (!cc.isCC())
      throw new IllegalArgumentException("Cannot generate an incremental cardinality constraint on a pseudo-Boolean constraint");
    final EncodingResult result = EncodingResult.resultForMiniSat(this.f, this);
    return this.ccEncoder.encodeIncremental(cc, result);
  }

  @Override
  protected void addClause(final Formula formula, final Proposition proposition) {
    this.result = UNDEF;
    this.solver.addClause(generateClauseVector(formula.literals()), proposition);
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
    if (this.result != UNDEF)
      return this.result;
    this.result = this.solver.solve(handler);
    return this.result;
  }

  @Override
  public Tristate sat(final SATHandler handler, final Literal literal) {
    final LNGIntVector clauseVec = new LNGIntVector(1);
    int index = this.solver.idxForName(literal.name());
    if (index == -1) {
      index = this.solver.newVar(!this.initialPhase, true);
      this.solver.addName(literal.name(), index);
    }
    final int litNum = literal.phase() ? index * 2 : (index * 2) ^ 1;
    clauseVec.push(litNum);
    this.result = this.solver.solve(handler, clauseVec);
    return this.result;
  }

  @Override
  public Tristate sat(final SATHandler handler, final Collection<? extends Literal> assumptions) {
    final Set<Literal> assumptionSet = new LinkedHashSet<>(assumptions);
    final LNGIntVector assumptionVec = new LNGIntVector(assumptionSet.size());
    for (final Literal literal : assumptionSet) {
      int index = this.solver.idxForName(literal.name());
      if (index == -1) {
        index = this.solver.newVar(!this.initialPhase, true);
        this.solver.addName(literal.name(), index);
      }
      final int litNum = literal.phase() ? index * 2 : (index * 2) ^ 1;
      assumptionVec.push(litNum);
    }
    this.result = this.solver.solve(handler, assumptionVec);
    return this.result;
  }

  @Override
  public void reset() {
    this.solver.reset();
    this.result = UNDEF;
  }

  @Override
  public Assignment model(final Collection<Variable> variables) {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'sat' first.");
    final LNGIntVector relevantIndices = variables == null ? null : new LNGIntVector(variables.size());
    if (relevantIndices != null) {
      for (final Variable var : variables) {
        relevantIndices.push(this.solver.idxForName(var.name()));
      }
    }
    return this.result == TRUE ? this.createAssignment(this.solver.model(), relevantIndices) : null;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables) {
    return enumerateAllModels(variables, Collections.<Variable>emptyList());
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables, final Collection<Variable> additionalVariables) {
    return enumerateAllModels(variables, additionalVariables, null);
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> literals, final ModelEnumerationHandler handler) {
    return enumerateAllModels(literals, Collections.<Variable>emptyList(), handler);
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables, final Collection<Variable> additionalVariables, final ModelEnumerationHandler handler) {
    final List<Assignment> models = new LinkedList<>();
    SolverState stateBeforeEnumeration = null;
    if (this.style == SolverStyle.MINISAT && this.incremental)
      stateBeforeEnumeration = this.saveState();
    boolean proceed = true;
    SortedSet<Variable> allVariables = new TreeSet<>();
    if (variables == null) {
      allVariables = null;
    } else {
      allVariables.addAll(variables);
      allVariables.addAll(additionalVariables);
    }
    final LNGIntVector relevantIndices = variables == null ? null : new LNGIntVector(variables.size());
    LNGIntVector relevantAllIndices = null;
    if (relevantIndices != null) {
      for (final Variable var : variables) {
        relevantIndices.push(this.solver.idxForName(var.name()));
      }
      relevantAllIndices = additionalVariables.isEmpty() ? relevantIndices : new LNGIntVector(allVariables.size());
      if (!additionalVariables.isEmpty()) {
        for (final Variable var : allVariables) {
          relevantAllIndices.push(this.solver.idxForName(var.name()));
        }
      }
    }
    while (proceed && this.sat((SATHandler) null) == TRUE) {
      final LNGBooleanVector modelFromSolver = this.solver.model();
      final Assignment model = this.createAssignment(modelFromSolver, relevantAllIndices);
      assert model != null;
      models.add(model);
      proceed = handler == null || handler.foundModel(model);
      if (model.size() > 0) {
        final LNGIntVector blockingClause = generateBlockingClause(modelFromSolver, relevantIndices);
        this.solver.addClause(blockingClause, null);
        this.result = UNDEF;
      } else {
        break;
      }
    }
    if (this.style == SolverStyle.MINISAT && this.incremental)
      this.loadState(stateBeforeEnumeration);
    return models;
  }

  /**
   * Generates a blocking clause from a given model and a set of relevant variables.
   * @param modelFromSolver the current model for which the blocking clause should be generated
   * @param relevantVars    the indices of the relevant variables.  If {@code null} all variables are relevant.
   * @return the blocking clause for the given model and relevant variables
   */
  private LNGIntVector generateBlockingClause(final LNGBooleanVector modelFromSolver, final LNGIntVector relevantVars) {
    final LNGIntVector blockingClause;
    if (relevantVars != null) {
      blockingClause = new LNGIntVector(relevantVars.size());
      for (int i = 0; i < relevantVars.size(); i++) {
        final int varIndex = relevantVars.get(i);
        if (varIndex != -1) {
          final boolean varAssignment = modelFromSolver.get(varIndex);
          blockingClause.push(varAssignment ? (varIndex * 2) ^ 1 : varIndex * 2);
        }
      }
    } else {
      blockingClause = new LNGIntVector(modelFromSolver.size());
      for (int i = 0; i < modelFromSolver.size(); i++) {
        final boolean varAssignment = modelFromSolver.get(i);
        blockingClause.push(varAssignment ? (i * 2) ^ 1 : i * 2);
      }
    }
    return blockingClause;
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
    for (int i = this.validStates.size() - 1; i >= 0 && index == -1; i--)
      if (this.validStates.get(i) == state.id())
        index = i;
    if (index == -1)
      throw new IllegalArgumentException("The given solver state is not valid anymore.");
    this.validStates.shrinkTo(index + 1);
    this.solver.loadState(state.state());
    this.result = UNDEF;
  }

  @Override
  public SortedSet<Variable> knownVariables() {
    final SortedSet<Variable> result = new TreeSet<>();
    final int nVars = this.solver.nVars();
    for (final Map.Entry<String, Integer> entry : this.solver.name2idx().entrySet())
      if (entry.getValue() < nVars)
        result.add(this.f.variable(entry.getKey()));
    return result;
  }

  @Override
  public UNSATCore<Proposition> unsatCore() {
    if (!this.config.proofGeneration())
      throw new IllegalStateException("Cannot generate an unsat core if proof generation is not turned on");
    if (this.result == TRUE)
      throw new IllegalStateException("An unsat core can only be generated if the formula is solved and is UNSAT");
    if (this.result == Tristate.UNDEF) {
      throw new IllegalStateException("Cannot generate an unsat core before the formula was solved.");
    }
    if (this.underlyingSolver() instanceof MiniCard)
      throw new IllegalStateException("Cannot compute an unsat core with MiniCard.");
    if (this.underlyingSolver() instanceof GlucoseSyrup && this.config.incremental())
      throw new IllegalStateException("Cannot compute an unsat core with Glucose in incremental mode.");

    final DRUPTrim trimmer = new DRUPTrim();

    final Map<Formula, Proposition> clause2proposition = new HashMap<>();
    final LNGVector<LNGIntVector> clauses = new LNGVector<>(this.underlyingSolver().pgOriginalClauses().size());
    for (final MiniSatStyleSolver.ProofInformation pi : this.underlyingSolver().pgOriginalClauses()) {
      clauses.push(pi.clause());
      final Formula clause = getFormulaForVector(pi.clause());
      Proposition proposition = pi.proposition();
      if (proposition == null)
        proposition = new StandardProposition(clause);
      clause2proposition.put(clause, proposition);
    }

    if (containsEmptyClause(clauses)) {
      final Proposition emptyClause = clause2proposition.get(this.f.falsum());
      return new UNSATCore<>(Collections.singletonList(emptyClause), true);
    }

    final DRUPTrim.DRUPResult result = trimmer.compute(clauses, this.underlyingSolver().pgProof());
    if (result.trivialUnsat())
      return handleTrivialCase();
    final LinkedHashSet<Proposition> propositions = new LinkedHashSet<>();
    for (final LNGIntVector vector : result.unsatCore())
      propositions.add(clause2proposition.get(getFormulaForVector(vector)));
    return new UNSATCore<>(new ArrayList<>(propositions), false);
  }

  /**
   * Generates a clause vector of a collection of literals.
   * @param literals the literals
   * @return the clause vector
   */
  private LNGIntVector generateClauseVector(final Collection<Literal> literals) {
    final LNGIntVector clauseVec = new LNGIntVector(literals.size());
    for (final Literal lit : literals) {
      int index = this.solver.idxForName(lit.name());
      if (index == -1) {
        index = this.solver.newVar(!this.initialPhase, true);
        this.solver.addName(lit.name(), index);
      }
      final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
      clauseVec.push(litNum);
    }
    return clauseVec;
  }

  /**
   * Creates an assignment from a Boolean vector of the solver.
   * @param vec             the vector of the solver
   * @param relevantIndices the solver's indices of the relevant variables for the model.  If {@code null}, all
   *                        variables are relevant.
   * @return the assignment
   */
  private Assignment createAssignment(final LNGBooleanVector vec, final LNGIntVector relevantIndices) {
    final Assignment model = new Assignment();
    if (relevantIndices == null) {
      for (int i = 0; i < vec.size(); i++) {
        model.addLiteral(this.f.literal(this.solver.nameForIdx(i), vec.get(i)));
      }
    } else {
      for (int i = 0; i < relevantIndices.size(); i++) {
        final int index = relevantIndices.get(i);
        if (index != -1) {
          model.addLiteral(this.f.literal(this.solver.nameForIdx(index), vec.get(index)));
        }
      }
    }
    return model;
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

  private UNSATCore<Proposition> handleTrivialCase() {
    final LNGVector<MiniSatStyleSolver.ProofInformation> clauses = this.underlyingSolver().pgOriginalClauses();
    for (int i = 0; i < clauses.size(); i++)
      for (int j = i + 1; j < clauses.size(); j++) {
        if (clauses.get(i).clause().size() == 1 && clauses.get(j).clause().size() == 1
                && clauses.get(i).clause().get(0) + clauses.get(j).clause().get(0) == 0) {
          final LinkedHashSet<Proposition> propositions = new LinkedHashSet<>();
          final Proposition pi = clauses.get(i).proposition();
          final Proposition pj = clauses.get(j).proposition();
          propositions.add(pi != null ? pi : new StandardProposition(getFormulaForVector(clauses.get(i).clause())));
          propositions.add(pj != null ? pj : new StandardProposition(getFormulaForVector(clauses.get(j).clause())));
          return new UNSATCore<>(new ArrayList<>(propositions), false);
        }
      }
    throw new IllegalStateException("Should be a trivial unsat core, but did not found one.");
  }

  private boolean containsEmptyClause(final LNGVector<LNGIntVector> clauses) {
    for (final LNGIntVector clause : clauses)
      if (clause.empty())
        return true;
    return false;
  }

  private Formula getFormulaForVector(final LNGIntVector vector) {
    final List<Literal> literals = new ArrayList<>(vector.size());
    for (int i = 0; i < vector.size(); i++) {
      final int lit = vector.get(i);
      final String varName = this.underlyingSolver().nameForIdx(Math.abs(lit) - 1);
      literals.add(this.f.literal(varName, lit > 0));
    }
    return this.f.or(literals);
  }

  @Override
  public String toString() {
    return String.format("MiniSat{result=%s, incremental=%s}", this.result, this.incremental);
  }
}
