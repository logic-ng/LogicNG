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
//  Copyright 2015 Christoph Zengler                                     //
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

import org.logicng.collections.LNGBooleanVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.sat.CleaneLingConfig;
import org.logicng.solvers.sat.CleaneLingMinimalisticSolver;
import org.logicng.solvers.sat.CleaneLingSolver;
import org.logicng.solvers.sat.CleaneLingStyleSolver;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * Wrapper for the CleaneLing-style SAT solvers.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class CleaneLing extends SATSolver {

  private enum SolverStyle {MINIMALISTIC, FULL}

  private static final int CLAUSE_TERMINATOR = 0;

  private SolverStyle solverStyle;
  private boolean plain;
  private final CleaneLingStyleSolver solver;
  private SortedMap<Variable, Integer> var2index;
  private SortedMap<Integer, Variable> index2var;

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
    this.var2index = new TreeMap<>();
    this.index2var = new TreeMap<>();
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
  protected void addClause(final Formula formula) {
    this.result = UNDEF;
    for (Literal lit : formula.literals()) {
      Integer index = this.var2index.get(lit.variable());
      if (index == null) {
        index = this.var2index.size() + 1;
        this.var2index.put(lit.variable(), index);
        this.index2var.put(index, lit.variable());
      }
      this.solver.addlit(lit.phase() ? index : -index);
    }
    this.solver.addlit(CLAUSE_TERMINATOR);
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
  public void reset() {
    this.solver.reset();
    this.result = UNDEF;
  }

  @Override
  public Assignment model(final Collection<Variable> variables) {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'sat' first.");
    return this.result == TRUE ? this.createAssignment(this.solver.model(), variables) : null;
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
    if (!vec.empty()) {
      for (int i = 1; i < vec.size(); i++) {
        final Variable var = this.index2var.get(i);
        if (vec.get(i)) {
          if (variables == null || variables.contains(var))
            model.addLiteral(var);
        } else if (variables == null || variables.contains(var))
          model.addLiteral(var.negate());
      }
    }
    return model;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables) {
    if (this.solverStyle == SolverStyle.FULL && !this.plain)
      throw new UnsupportedOperationException("Model enumeration is not available if simplifications are turned on");
    List<Assignment> models = new LinkedList<>();
    while (this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(variables);
      models.add(model);
      assert model != null;
      this.add(model.blockingClause(this.f, variables));
    }
    return models;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Variable> literals, final ModelEnumerationHandler handler) {
    if (this.solverStyle == SolverStyle.FULL && !this.plain)
      throw new UnsupportedOperationException("Model enumeration is not available if simplifications are turned on");
    List<Assignment> models = new LinkedList<>();
    boolean proceed = true;
    while (proceed && this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(literals);
      models.add(model);
      proceed = handler.foundModel(model);
      assert model != null;
      this.add(model.blockingClause(this.f, literals));
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
  public String toString() {
    return String.format("CleaneLing{result=%s, index2var=%s}", this.result, this.index2var);
  }
}
