package org.logicng.explanations.backbones;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSWatcher;
import org.logicng.solvers.sat.MiniSat2Solver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;

/**
 * An extension of MiniSat to compute the backbone of a formula.
 */
public class MiniSatBackbone extends MiniSat2Solver {

  // TODO experimetnal backbone solver and not extensively tested, yet!

  private final FormulaFactory f;

  public MiniSatBackbone(FormulaFactory f) {
    this.f = f;
  }

  /**
   * Adds an arbitrary formula to the propagator.
   * @param formula the formula
   */
  public void add(final Formula formula) {
    final Formula cnf = formula.cnf();
    switch (cnf.type()) {
      case TRUE:
        break;
      case FALSE:
      case LITERAL:
      case OR:
        this.addClause(generateClauseVector(cnf), null);
        break;
      case AND:
        for (final Formula op : cnf) {
          this.addClause(generateClauseVector(op), null);
        }
        break;
      default:
        throw new IllegalStateException("Unexpected formula type in CNF: " + cnf.type());
    }
  }

  /**
   * Computes the backbone.
   * @param relevantVariables relevant variables
   * @return the backbone projected to the relevant variables
   */
  public SortedSet<Literal> compute(List<Formula> restrictions, final Collection<Variable> relevantVariables) {
    int[] state = this.saveState();
    for (Formula formula : restrictions) {
      this.add(formula.cnf());
    }

    final boolean initSAT = solve(null) == Tristate.TRUE;
    if (!initSAT) {
      loadState(state);
      return null;
    }
    final List<Integer> backboneSolverLiterals = compute(toVarIndices(relevantVariables));
    if (backboneSolverLiterals == null) {
      loadState(state);
      return null;
    } else {
      final SortedSet<Literal> backboneLiterals = new TreeSet<Literal>();
      for (final Integer lit : backboneSolverLiterals) {
        backboneLiterals.add(solverLiteralToFormula(lit));
      }
      loadState(state);
      return backboneLiterals;
    }
  }

  private List<Integer> toVarIndices(final Collection<Variable> variables) {
    final List<Integer> varIndices = new ArrayList<Integer>(variables.size());
    for (final Variable v : variables) {
      // TODO removal of unknown variables may lead to buggy result
      Integer idx = name2idx.get(v.name());
      if (idx != null) {
        varIndices.add(idx);
      }
    }
    return varIndices;
  }

  private boolean isUnit(final int lit, final MSClause clause) {
    for (int i = 0; i < clause.size(); ++i) {
      final int clauseLit = clause.get(i);
      // TODO this unit check can surely be improved
      if (lit != clauseLit && this.model.get((var(clauseLit))) != sign(clauseLit)) {
        return false;
      }
    }
    return true;
  }

  private boolean isRotatable(final int lit) {
    // Unit propagated literals cannot be rotatable
    if (v(var(lit)).reason() != null) {
      return false;
    }
    for (final MSWatcher watcher : this.watches.get(not(lit))) {
      if (isUnit(lit, watcher.clause())) {
        return false;
      }
    }
    return true;
  }

  private List<Integer> compute(final List<Integer> relevantVariables) {
    final List<Integer> backboneLiterals = new ArrayList<Integer>();
    final Stack<Integer> candidates = new Stack<Integer>();
    for (final Integer var : relevantVariables) {
      if (this.vars.get(var).level() == 0) {
        // Use UP zero literal to refine lower bound
        backboneLiterals.add(mkLit(var, !this.model.get(var)));
      } else {
        // Use initial model to refine upper bound
        // If literal is rotatable skip it
        final int lit = mkLit(var, !this.model.get(var));
        //if (!isRotatable(lit)) {
        candidates.add(lit);
        //}
      }
    }

    while (candidates.size() > 0) {
      final int lit = candidates.pop();
      final LNGIntVector assumptions = new LNGIntVector(1);
      assumptions.push(not(lit));
      final boolean sat = solve(null, assumptions) == Tristate.TRUE;
      if (!sat) {
        backboneLiterals.add(lit);
        addClause(lit, null);
      } else {
        // Use model to refine upper bound
        for (final Integer candidateLit : new ArrayList<Integer>(candidates)) {
          if (this.model.get(var(candidateLit)) == sign(candidateLit)) {
            // || isRotatable(candidateLit)) {
            candidates.remove(candidateLit);
          }
        }
      }
    }
    return backboneLiterals;
  }

  /**
   * Transforms an solver literal into the corresponding formula literal.
   * @param lit the solver literal
   * @return the formula literal
   */
  private Literal solverLiteralToFormula(final int lit) {
    return f.literal(this.nameForIdx(var(lit)), !sign(lit));
  }

  /**
   * Generates a solver vector of a clause.
   * @param clause the clause
   * @return the solver vector
   */
  private LNGIntVector generateClauseVector(final Formula clause) {
    final LNGIntVector clauseVec = new LNGIntVector(clause.numberOfOperands());
    for (final Literal lit : clause.literals()) {
      int index = this.idxForName(lit.name());
      if (index == -1) {
        index = this.newVar(true, true);
        this.addName(lit.name(), index);
      }
      final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
      clauseVec.push(litNum);
    }
    return clauseVec;
  }
}