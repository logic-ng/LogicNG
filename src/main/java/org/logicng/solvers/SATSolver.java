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

import org.logicng.cardinalityconstraints.CCIncrementalData;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.unsatcores.UNSATCore;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;

/**
 * A generic interface for LogicNG's SAT solvers.
 * @version 1.3
 * @since 1.0
 */
public abstract class SATSolver {

  protected final FormulaFactory f;
  protected Tristate result;

  /**
   * Constructor.
   * @param f the formula factory
   */
  protected SATSolver(final FormulaFactory f) {
    this.f = f;
  }

  /**
   * Adds a formula to the solver.  The formula is first converted to CNF.
   * @param formula the formula
   */
  public void add(final Formula formula) {
    add(formula, null);
  }

  /**
   * Adds a formula to the solver.  The formula is first converted to CNF.
   * @param formula     the formula
   * @param proposition the proposition of this formula
   */
  public abstract void add(final Formula formula, Proposition proposition);

  /**
   * Adds a formula to the solver, but sets all variables to false which are not known to the solver.
   * @param formula the formula
   */
  public abstract void addWithoutUnknown(final Formula formula);

  /**
   * Adds a given set of propositions to the solver.
   * @param propositions the set of propositions
   */
  public void addPropositions(final Collection<? extends Proposition> propositions) {
    for (final Proposition proposition : propositions)
      add(proposition);
  }

  /**
   * Adds a given set of propositions to the solver.
   * @param propositions the set of propositions
   */
  public void addPropositions(final Proposition... propositions) {
    for (final Proposition proposition : propositions)
      add(proposition);
  }

  /**
   * Adds a proposition to the solver.  The formulas of the proposition are first converted to CNF.
   * @param proposition the proposition
   */
  public void add(final Proposition proposition) {
    for (final Formula formula : proposition.formulas())
      this.add(formula, proposition);
  }

  /**
   * Adds a formula list to the solver.
   * @param formulas the formula list
   */
  public void add(final ImmutableFormulaList formulas) {
    for (final Formula formula : formulas)
      this.add(formula);
  }

  /**
   * Adds a collection of formulas to the solver.
   * @param formulas the collection of formulas
   */
  public void add(final Collection<? extends Formula> formulas) {
    for (final Formula formula : formulas)
      this.add(formula);
  }

  /**
   * Adds a formula to the solver and relaxes the given CNF with the given relaxation variable.
   * @param relaxationVar the relaxation variable
   * @param formula       the formula
   */
  public void addWithRelaxation(final Variable relaxationVar, final Formula formula) {
    this.addClauseSetWithRelaxation(relaxationVar, formula.cnf());
  }

  /**
   * Adds a proposition to the solver.  The formulas of the proposition are first converted to CNF.
   * @param relaxationVar the relaxation variable
   * @param proposition   the proposition
   */
  public void addWithRelaxation(final Variable relaxationVar, final Proposition proposition) {
    for (final Formula formula : proposition.formulas())
      this.addWithRelaxation(relaxationVar, formula);
  }

  /**
   * Adds a formula list to the solver.
   * @param relaxationVar the relaxation variable
   * @param formulas      the formula list
   */
  public void addWithRelaxation(final Variable relaxationVar, final ImmutableFormulaList formulas) {
    for (final Formula formula : formulas)
      this.addWithRelaxation(relaxationVar, formula);
  }

  /**
   * Adds a collection of formulas to the solver.
   * @param relaxationVar the relaxation variable
   * @param formulas      the collection of formulas
   */
  public void addWithRelaxation(final Variable relaxationVar, final Collection<? extends Formula> formulas) {
    for (final Formula formula : formulas)
      this.addWithRelaxation(relaxationVar, formula);
  }

  /**
   * Adds a cardinality constraint and returns its incremental data in order to refine the constraint on the solver.
   *
   * Usage constraints:
   * - "&lt;": Cannot be used with right hand side 2, returns null for right hand side 1, but constraint is added to solver.
   * - "&lt;=": Cannot be used with right hand side 1, returns null for right hand side 0, but constraint is added to solver.
   * - "&gt;": Returns null for right hand side 0 or number of variables -1, but constraint is added to solver. Adds false to solver for right hand side &gt;= number of variables.
   * - "&gt;=": Returns null for right hand side 1 or number of variables, but constraint is added to solver. Adds false to solver for right hand side &gt; number of variables.
   * @param cc the cardinality constraint
   * @return the incremental data of this constraint, or null if the right hand side of cc is 1
   */
  public abstract CCIncrementalData addIncrementalCC(final PBConstraint cc);


  /**
   * Adds a formula which is already in CNF to the solver.
   * @param proposition a proposition (if required for proof tracing)
   * @param formula     the formula in CNF
   */
  void addClauseSet(final Formula formula, final Proposition proposition) {
    switch (formula.type()) {
      case TRUE:
        break;
      case FALSE:
      case LITERAL:
      case OR:
        this.addClause(formula, proposition);
        break;
      case AND:
        for (final Formula op : formula)
          this.addClause(op, proposition);
        break;
      default:
        throw new IllegalArgumentException("Input formula ist not a valid CNF: " + formula);
    }
  }

  /**
   * Adds a formula which is already in CNF with a given relaxation to the solver.
   * @param relaxationVar the relaxation variable
   * @param formula       the formula in CNF
   */
  private void addClauseSetWithRelaxation(final Variable relaxationVar, final Formula formula) {
    switch (formula.type()) {
      case TRUE:
        break;
      case FALSE:
      case LITERAL:
      case OR:
        this.addClauseWithRelaxation(relaxationVar, formula);
        break;
      case AND:
        for (final Formula op : formula)
          this.addClauseWithRelaxation(relaxationVar, op);
        break;
      default:
        throw new IllegalArgumentException("Input formula ist not a valid CNF: " + formula);
    }
  }

  /**
   * Adds a formula which must be a clause to the solver.
   * @param formula     the clause
   * @param proposition a proposition (if required for proof tracing)
   */
  protected abstract void addClause(final Formula formula, final Proposition proposition);

  /**
   * Adds a formula which must be a clause to the solver.
   * @param relaxationVar the relaxation variable
   * @param formula       the clause
   */
  protected abstract void addClauseWithRelaxation(final Variable relaxationVar, final Formula formula);

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver is satisfiable, @{code Tristate.FALSE} if it is
   * unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * @return the satisfiability of the formula in the solver
   */
  public Tristate sat() {
    return this.sat((SATHandler) null);
  }

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver is satisfiable, @{code Tristate.FALSE} if it is
   * unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * @param handler the SAT handler
   * @return the satisfiability of the formula in the solver
   */
  public abstract Tristate sat(final SATHandler handler);

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver and a given literal are satisfiable,
   * {@code Tristate.FALSE} if it is unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * @param literal the assumed literal
   * @return the satisfiability of the formula in the solver
   */
  public Tristate sat(final Literal literal) {
    return this.sat(null, literal);
  }

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver and a given collection of assumed literals
   * are satisfiable, {@code Tristate.FALSE} if it is unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * The assumptions can be seen as an additional conjunction of literals.
   * Note: Use ordered collections to ensure determinism in the solving process and thus in the resulting model or conflict.
   * @param assumptions a collection of literals
   * @return the satisfiability of the formula in the solver
   */
  public Tristate sat(final Collection<Literal> assumptions) {
    return this.sat(null, assumptions);
  }

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver and a given literal are satisfiable,
   * {@code Tristate.FALSE} if it is unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * @param handler the SAT handler
   * @param literal the assumed literal
   * @return the satisfiability of the formula in the solver
   */
  public abstract Tristate sat(final SATHandler handler, final Literal literal);

  /**
   * Returns {@code Tristate.TRUE} if the current formula in the solver and a given collection of assumed literals
   * are satisfiable, {@code Tristate.FALSE} if it is unsatisfiable, or {@code UNDEF} if the solving process was aborted.
   * The assumptions can be seen as an additional conjunction of literals.
   * Note: Use ordered collections to ensure determinism in the solving process and thus in the resulting model or conflict.
   * @param handler     the SAT handler
   * @param assumptions a collection of literals
   * @return the satisfiability of the formula in the solver
   */
  public abstract Tristate sat(final SATHandler handler, final Collection<? extends Literal> assumptions);

  /**
   * Resets the SAT solver.
   */
  public abstract void reset();

  /**
   * Returns a model of the current formula on the solver.  If the formula is UNSAT, {@code null} will be returned.
   * @return a model of the current formula
   */
  public Assignment model() {
    return this.model((Collection<Variable>) null);
  }

  /**
   * Returns a model of the current formula on the solver wrt. a given set of variables. If the set
   * is {@code null}, all variables are considered relevant. If the formula is UNSAT, {@code null} will be returned.
   * The formula in the solver has to be solved first, before a model can be obtained.
   * @param variables the set of variables
   * @return a model of the current formula
   * @throws IllegalStateException if the formula is not yet solved
   */
  public Assignment model(final Variable[] variables) {
    return this.model(Arrays.asList(variables));
  }

  /**
   * Returns a model of the current formula on the solver wrt. a given set of variables. If the set
   * is {@code null}, all variables are considered relevant.
   * If the formula is UNSAT, {@code null} will be returned.
   * @param variables the set of variables
   * @return a model of the current formula
   */
  public abstract Assignment model(final Collection<Variable> variables);

  /**
   * Enumerates all models of the current formula.
   * @return the list of models
   */
  public List<Assignment> enumerateAllModels() {
    return this.enumerateAllModels((Collection<Variable>) null);
  }

  /**
   * Enumerates all models of the current formula wrt. a given set of variables.  If the set is {@code null},
   * all variables are considered relevant.
   * @param variables the set of variables
   * @return the list of models
   */
  public List<Assignment> enumerateAllModels(final Variable[] variables) {
    return this.enumerateAllModels(Arrays.asList(variables));
  }

  /**
   * Enumerates all models of the current formula wrt. a given set of variables.  If the set is {@code null},
   * all variables are considered relevant.
   * @param variables the set of variables
   * @return the list of models
   */
  public abstract List<Assignment> enumerateAllModels(final Collection<Variable> variables);

  /**
   * Enumerates all models of the current formula wrt. a given set of variables.  If the set is {@code null},
   * all variables are considered relevant. Additionally every assignment contains literals for the given additional
   * variables.
   * @param variables           the set of variables
   * @param additionalVariables the additional variables
   * @return the list of models
   */
  public abstract List<Assignment> enumerateAllModels(final Collection<Variable> variables, final Collection<Variable> additionalVariables);

  /**
   * Enumerates all models of the current formula and passes it to a model enumeration handler.
   * @param handler the model enumeration handler
   * @return the list of models
   */
  public List<Assignment> enumerateAllModels(final ModelEnumerationHandler handler) {
    return this.enumerateAllModels((Collection<Variable>) null, handler);
  }

  /**
   * Enumerates all models of the current formula wrt. a given set of variables and passes it to a model
   * enumeration handler.  If the set is {@code null}, all literals are considered relevant.
   * @param variables the set of variables
   * @param handler   the model enumeration handler
   * @return the list of models
   */
  public List<Assignment> enumerateAllModels(final Variable[] variables, final ModelEnumerationHandler handler) {
    return this.enumerateAllModels(Arrays.asList(variables), handler);
  }

  /**
   * Enumerates all models of the current formula wrt. a given set of variables  and passes it to a model
   * enumeration handler.  If the set is {@code null}, all literals are considered relevant.
   * @param variables the set of variables
   * @param handler   the model enumeration handler
   * @return the list of models
   */
  public abstract List<Assignment> enumerateAllModels(final Collection<Variable> variables, final ModelEnumerationHandler handler);

  /**
   * Enumerates all models of the current formula wrt. a given set of variables  and passes it to a model
   * enumeration handler.  If the set is {@code null}, all literals are considered relevant. Additionally every
   * assignment contains literals for the given additional variables.
   * @param variables           the set of variables
   * @param additionalVariables the additional variables
   * @param handler             the model enumeration handler
   * @return the list of models
   */
  public abstract List<Assignment> enumerateAllModels(final Collection<Variable> variables, final Collection<Variable> additionalVariables, final ModelEnumerationHandler handler);

  /**
   * Saves the current solver state.
   * @return the current solver state
   * @throws UnsupportedOperationException if the solver does not support state saving/loading
   * @throws IllegalStateException         if the solver is not in incremental mode
   */
  public abstract SolverState saveState();

  /**
   * Loads a given solver state.
   * @param state the solver state
   * @throws UnsupportedOperationException if the solver does not support state saving/loading
   * @throws IllegalStateException         if the solver is not in incremental mode
   * @throws IllegalArgumentException      if the given state has become invalid
   */
  public abstract void loadState(final SolverState state);

  /**
   * Sets the solver state to UNDEF (required if you fiddle e.g. with the underlying solver).
   */
  public void setSolverToUndef() {
    this.result = Tristate.UNDEF;
  }

  /**
   * Returns the set of variables currently known by the solver.
   * NOTE: Due to the incremental/decremental interface of some of the solvers, this set is generated each time,
   * the method is called.  So if you can maintain a list of relevant/known variables in your own application,
   * this is recommended.
   * @return the set of variables currently known by the solver
   */
  public abstract SortedSet<Variable> knownVariables();

  /**
   * Returns an unsat core of the current problem.  Only works if the SAT solver is configured to record the information
   * required to generate a proof trace and an unsat core.
   * @return the unsat core
   */
  public abstract UNSATCore<Proposition> unsatCore();

  /**
   * Returns the formula factory for this solver.
   * @return the formula factory
   */
  public FormulaFactory factory() {
    return this.f;
  }

}
