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
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.GlucoseSyrup;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * Wrapper for the MiniSAT-style SAT solvers.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class MiniSat extends SATSolver {

  private enum SolverStyle {MINISAT, GLUCOSE, MINICARD}

  private final MiniSatStyleSolver solver;
  private final SolverStyle style;
  private boolean incremental;
  private boolean initialPhase;

  /**
   * Constructs a new SAT solver instance.
   * @param f           the formula factory
   * @param solverStyle the solver style
   * @throws IllegalArgumentException if the solver style is unknown
   */
  private MiniSat(final FormulaFactory f, final SolverStyle solverStyle, final MiniSatConfig miniSatConfig,
                  final GlucoseConfig glucoseConfig) {
    super(f);
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
  public void add(final Formula formula) {
    if (formula.type() == FType.PBC) {
      final PBConstraint constraint = (PBConstraint) formula;
      this.result = UNDEF;
      if (this.style == SolverStyle.MINICARD && constraint.isCC()) {
        if (constraint.comparator() == CType.LE)
          ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
        else if (constraint.comparator() == CType.LT && constraint.rhs() > 3)
          ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs() - 1);
        else if (constraint.comparator() == CType.EQ && constraint.rhs() == 1) {
          ((MiniCard) this.solver).addAtMost(generateClauseVector(Arrays.asList(constraint.operands())), constraint.rhs());
          this.solver.addClause(generateClauseVector(Arrays.asList(constraint.operands())));
        } else
          super.add(constraint);
      } else
        super.add(constraint);
    } else
      this.addClauseSet(formula.cnf());
  }

  @Override
  protected void addClause(final Formula formula) {
    this.result = UNDEF;
    this.solver.addClause(generateClauseVector(formula.literals()));
  }

  /**
   * Generates a clause vector of a collection of literals.
   * @param literals the literals
   * @return the clause vector
   */
  private LNGIntVector generateClauseVector(final Collection<Literal> literals) {
    final LNGIntVector clauseVec = new LNGIntVector(literals.size());
    for (Literal lit : literals) {
      int index = this.solver.idxForName(lit.name());
      if (index == -1) {
        index = this.solver.newVar(!initialPhase, true);
        this.solver.addName(lit.name(), index);
      }
      int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
      clauseVec.push(litNum);
    }
    return clauseVec;
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
    final LNGIntVector clauseVec = new LNGIntVector(1);
    int index = this.solver.idxForName(literal.name());
    if (index == -1) {
      index = this.solver.newVar(true, true);
      this.solver.addName(literal.name(), index);
    }
    int litNum = literal.phase() ? index * 2 : (index * 2) ^ 1;
    clauseVec.push(litNum);
    this.result = this.solver.solve(handler, clauseVec);
    return this.result;
  }

  @Override
  public void reset() {
    this.solver.reset();
    this.result = UNDEF;
  }

  @Override
  public Assignment model(final Collection<Literal> literals) {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'sat' first.");
    return this.result == TRUE ? this.createAssignment(this.solver.model(), literals) : null;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Literal> literals) {
    List<Assignment> models = new LinkedList<>();
    SolverState stateBeforeEnumeration = null;
    if (this.style == SolverStyle.MINISAT && incremental)
      stateBeforeEnumeration = this.saveState();
    while (this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(literals);
      models.add(model);
      this.add(model.blockingClause(this.f, literals));
    }
    if (this.style == SolverStyle.MINISAT && incremental)
      this.loadState(stateBeforeEnumeration);
    return models;
  }

  @Override
  public List<Assignment> enumerateAllModels(final Collection<Literal> literals, final ModelEnumerationHandler handler) {
    List<Assignment> models = new LinkedList<>();
    SolverState stateBeforeEnumeration = null;
    if (this.style == SolverStyle.MINISAT && incremental)
      stateBeforeEnumeration = this.saveState();
    boolean proceed = true;
    while (proceed && this.sat((SATHandler) null) == TRUE) {
      final Assignment model = this.model(literals);
      models.add(model);
      proceed = handler.foundModel(model);
      this.add(model.blockingClause(this.f, literals));
    }
    if (this.style == SolverStyle.MINISAT && incremental)
      this.loadState(stateBeforeEnumeration);
    return models;
  }

  /**
   * Creates an assignment from a Boolean vector of the solver.
   * @param vec       the vector of the solver
   * @param variables the variables which should appear in the model or {@code null} if all variables should
   *                  appear
   * @return the assignment
   */
  private Assignment createAssignment(final LNGBooleanVector vec, final Collection<Literal> variables) {
    final Assignment model = new Assignment();
    for (int i = 0; i < vec.size(); i++) {
      final Literal lit = this.f.literal(this.solver.nameForIdx(i));
      if (vec.get(i)) {
        if (variables == null || variables.contains(lit))
          model.addLiteral(lit);
      } else if (variables == null || variables.contains(lit))
        model.addLiteral(lit.negate());
    }
    return model;
  }

  @Override
  public SolverState saveState() {
    return new SolverState(this.solver.saveState());
  }

  @Override
  public void loadState(final SolverState state) {
    this.solver.loadState(state.state());
    this.result = UNDEF;
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

  @Override
  public String toString() {
    return String.format("MiniSat{result=%s, incremental=%s}", this.result, this.incremental);
  }
}
