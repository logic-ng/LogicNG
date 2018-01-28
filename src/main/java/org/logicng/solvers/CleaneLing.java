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
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.EncodingResult;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.unsatcores.UNSATCore;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.sat.CleaneLingConfig;
import org.logicng.solvers.sat.CleaneLingMinimalisticSolver;
import org.logicng.solvers.sat.CleaneLingSolver;
import org.logicng.solvers.sat.CleaneLingStyleSolver;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * Wrapper for the CleaneLing-style SAT solvers.
 * @version 1.3
 * @since 1.0
 */
public final class CleaneLing extends SATSolver {

  private enum SolverStyle {MINIMALISTIC, FULL}

  public static final int CLAUSE_TERMINATOR = 0;
  private final CCEncoder ccEncoder;
  private final CleaneLingStyleSolver solver;
  private SolverStyle solverStyle;
  private boolean plain;
  private SortedMap<String, Integer> name2idx;
  private SortedMap<Integer, String> idx2name;

  /**
   * Constructs a new SAT solver instance.
   * @param f           the formula factory
   * @param solverStyle the solver style
   * @param config      the configuration
   * @throws IllegalArgumentException if the solver style is unknown
   */
  private CleaneLing(final FormulaFactory f, final SolverStyle solverStyle,
                     final CleaneLingConfig config) {
    super(f);
    switch (solverStyle) {
      case MINIMALISTIC:
        this.solver = new CleaneLingMinimalisticSolver(config);
        break;
      case FULL:
        this.solver = new CleaneLingSolver(config);
        break;
      default:
        throw new IllegalArgumentException("Unknown solver style: " + solverStyle);
    }
    this.result = UNDEF;
    this.solverStyle = solverStyle;
    this.plain = config.plain();
    this.name2idx = new TreeMap<>();
    this.idx2name = new TreeMap<>();
    this.ccEncoder = new CCEncoder(f);
  }

  /**
   * Returns a new minimalistic CleaneLing solver.
   * @param f the formula factory
   * @return the solver
   */
  public static CleaneLing minimalistic(final FormulaFactory f) {
    return new CleaneLing(f, SolverStyle.MINIMALISTIC, new CleaneLingConfig.Builder().build());
  }

  /**
   * Returns a new minimalistic CleaneLing solver with a given configuration.
   * @param f      the formula factory
   * @param config the configuration
   * @return the solver
   */
  public static CleaneLing minimalistic(final FormulaFactory f, final CleaneLingConfig config) {
    return new CleaneLing(f, SolverStyle.MINIMALISTIC, config);
  }

  /**
   * Returns a new full CleaneLing solver.
   * @param f the formula factory
   * @return the solver
   */
  public static CleaneLing full(final FormulaFactory f) {
    return new CleaneLing(f, SolverStyle.FULL, new CleaneLingConfig.Builder().build());
  }

  /**
   * Returns a new full CleaneLing solver with a given configuration.
   * @param f      the formula factory
   * @param config the configuration
   * @return the solver
   */
  public static CleaneLing full(final FormulaFactory f, final CleaneLingConfig config) {
    return new CleaneLing(f, SolverStyle.FULL, config);
  }

  @Override
  public void add(final Formula formula, Proposition proposition) {
    if (formula.type() == FType.PBC) {
      final PBConstraint constraint = (PBConstraint) formula;
      this.result = UNDEF;
      if (constraint.isCC()) {
        final EncodingResult result = EncodingResult.resultForCleaneLing(this.f, this);
        ccEncoder.encode(constraint, result);
      } else
        this.addClauseSet(formula.cnf(), proposition);
    } else
      this.addClauseSet(formula.cnf(), proposition);
  }

  @Override
  public void addWithoutUnknown(final Formula formula) {
    final Assignment restriction = new Assignment(true);
    for (final Variable var : formula.variables())
      if (this.name2idx.get(var.name()) == null)
        restriction.addLiteral(var.negate());
    this.add(formula.restrict(restriction));
  }

  @Override
  public CCIncrementalData addIncrementalCC(PBConstraint cc) {
    if (!cc.isCC())
      throw new IllegalArgumentException("Cannot generate an incremental cardinality constraint on a pseudo-Boolean constraint");
    final EncodingResult result = EncodingResult.resultForCleaneLing(this.f, this);
    return ccEncoder.encodeIncremental(cc, result);
  }

  @Override
  protected void addClause(final Formula formula, final Proposition proposition) {
    this.result = UNDEF;
    addClause(formula.literals());
  }

  @Override
  protected void addClauseWithRelaxation(Variable relaxationVar, Formula formula) {
    this.result = UNDEF;
    final SortedSet<Literal> literals = new TreeSet<>(formula.literals());
    literals.add(relaxationVar);
    addClause(literals);
  }

  @Override
  public Tristate sat(final SATHandler handler) {
    if (this.result != UNDEF)
      return this.result;
    this.result = this.solver.solve(handler);
    return result;
  }

  @Override
  public Tristate sat(final SATHandler handler, final Literal literal) {
    throw new UnsupportedOperationException("CleaneLing does not support assumed literal solving.");
  }

  @Override
  public Tristate sat(final SATHandler handler, final Collection<? extends Literal> assumptions) {
    throw new UnsupportedOperationException("CleaneLing does not support solving with assumptions.");
  }

  @Override
  public void reset() {
    this.solver.reset();
    this.result = UNDEF;
  }

  @Override
  public Assignment model(Collection<Variable> variables) {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'sat' first.");
    return this.result == TRUE ? this.createAssignment(this.solver.model(), variables) : null;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables) {
    return enumerateAllModels(variables, Collections.<Variable>emptyList());
  }

  @Override
  public List<Assignment> enumerateAllModels(Collection<Variable> variables, Collection<Variable> additionalVariables) {
    if (this.solverStyle == SolverStyle.FULL && !this.plain)
      throw new UnsupportedOperationException("Model enumeration is not available if simplifications are turned on");
    List<Assignment> models = new LinkedList<>();
    SortedSet<Variable> allVariables = new TreeSet<>();
    if (variables == null) {
      allVariables = null;
    } else {
      allVariables.addAll(variables);
      allVariables.addAll(additionalVariables);
    }
    while (this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(allVariables);
      models.add(model);
      assert model != null;
      this.add(model.blockingClause(this.f, variables));
    }
    return models;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> literals, final ModelEnumerationHandler handler) {
    return enumerateAllModels(literals, Collections.<Variable>emptyList(), handler);
  }

  @Override
  public List<Assignment> enumerateAllModels(Collection<Variable> variables, Collection<Variable> additionalVariables, ModelEnumerationHandler handler) {
    if (this.solverStyle == SolverStyle.FULL && !this.plain)
      throw new UnsupportedOperationException("Model enumeration is not available if simplifications are turned on");
    List<Assignment> models = new LinkedList<>();
    boolean proceed = true;
    SortedSet<Variable> allVariables = new TreeSet<>();
    if (variables == null) {
      allVariables = null;
    } else {
      allVariables.addAll(variables);
      allVariables.addAll(additionalVariables);
    }
    while (proceed && this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(allVariables);
      models.add(model);
      proceed = handler.foundModel(model);
      assert model != null;
      this.add(model.blockingClause(this.f, variables));
    }
    return models;
  }

  @Override
  public SolverState saveState() {
    throw new UnsupportedOperationException("The CleaneLing solver does not support state loading/saving");
  }

  @Override
  public void loadState(SolverState state) {
    throw new UnsupportedOperationException("The CleaneLing solver does not support state loading/saving");
  }

  @Override
  public SortedSet<Variable> knownVariables() {
    final SortedSet<Variable> result = new TreeSet<>();
    for (final String name : this.name2idx.keySet())
      result.add(this.f.variable(name));
    return result;
  }

  @Override
  public UNSATCore<Proposition> unsatCore() {
    throw new UnsupportedOperationException("CleaneLing cannot compute unsat cores at the moment");
  }

  /**
   * Adds a collection of literals to the solver.
   * @param literals the literals
   */
  private void addClause(final Collection<Literal> literals) {
    for (Literal lit : literals) {
      Integer index = this.name2idx.get(lit.variable().name());
      if (index == null) {
        index = this.name2idx.size() + 1;
        this.name2idx.put(lit.variable().name(), index);
        this.idx2name.put(index, lit.variable().name());
      }
      this.solver.addlit(lit.phase() ? index : -index);
    }
    this.solver.addlit(CLAUSE_TERMINATOR);
  }

  /**
   * Creates an assignment from a Boolean vector of the solver.
   * @param vec       the vector of the solver
   * @param variables the variables which should appear in the model or {@code null} if all variables should
   *                  appear
   * @return the assignment
   */
  private Assignment createAssignment(final LNGBooleanVector vec, final Collection<Variable> variables) {
    final Assignment model = new Assignment();
    for (int i = 1; i < vec.size(); i++) {
      final Variable var = this.f.variable(this.idx2name.get(i));
      if (vec.get(i)) {
        if (variables == null || variables.contains(var))
          model.addLiteral(var);
      } else if (variables == null || variables.contains(var))
        model.addLiteral(var.negate());
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
  public CleaneLingStyleSolver underlyingSolver() {
    return this.solver;
  }

  /**
   * Returns the existing internal solver index for a variable or creates a new one if the variable is yet unknown.
   * @param var the variable
   * @return the (old or new) internal variable index
   */
  public int getOrCreateVarIndex(final Variable var) {
    Integer index = this.name2idx.get(var.name());
    if (index == null) {
      index = this.name2idx.size() + 1;
      this.name2idx.put(var.name(), index);
      this.idx2name.put(index, var.name());
    }
    return index;
  }

  /**
   * Creates a new variable on the solver and returns its name.
   * @param prefix the prefix of the variable name
   * @return the name of the new variable
   */
  public String createNewVariableOnSolver(final String prefix) {
    int index = this.name2idx.size() + 1;
    final String varName = prefix + "_" + index;
    this.name2idx.put(varName, index);
    this.idx2name.put(index, varName);
    return varName;
  }

  @Override
  public String toString() {
    return String.format("CleaneLing{result=%s, idx2name=%s}", this.result, this.idx2name);
  }
}
