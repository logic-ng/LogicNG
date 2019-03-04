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

  // TODO experimental backbone solver and not extensively tested yet!

  private final FormulaFactory f;
  private List<Integer> backbone;

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
    compute(toVarIndices(relevantVariables));
    if (backbone == null) {
      loadState(state);
      return null;
    } else {
      final SortedSet<Literal> backboneLiterals = new TreeSet<>();
      for (final Integer lit : backbone) {
        backboneLiterals.add(intLiteralToLiteral(lit));
      }
      loadState(state);
      return backboneLiterals;
    }
  }

  private List<Integer> toVarIndices(final Collection<Variable> variables) {
    final List<Integer> varIndices = new ArrayList<>(variables.size());
    for (final Variable v : variables) {
      // TODO removal of unknown variables may lead to buggy result
      Integer idx = name2idx.get(v.name());
      if (idx != null) {
        varIndices.add(idx);
      }
    }
    return varIndices;
  }

  private boolean isUPZeroLit(final int var) {
    return this.vars.get(var).level() == 0;
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

  private void addBackboneLiteral(final int backbonelit) {
    backbone.add(backbonelit);
    addClause(backbonelit, null);
  }

  private void compute(final List<Integer> relevantVariables) {
    backbone = new ArrayList<>();
    final Stack<Integer> candidates = new Stack<>();
    for (final Integer var : relevantVariables) {
      if (isUPZeroLit(var)) {
        // Refine lower bound
        backbone.add(mkLit(var, !this.model.get(var)));
      } else {
        // Refine upper bound
        final int lit = mkLit(var, !this.model.get(var));
        //        if (!isRotatable(lit)) {
        candidates.add(lit);
        //        }
      }
    }

    while (candidates.size() > 0) {
      final int lit = candidates.pop();
      final LNGIntVector assumptions = new LNGIntVector(1);
      assumptions.push(not(lit));
      final boolean sat = solve(null, assumptions) == Tristate.TRUE;
      if (!sat) {
        addBackboneLiteral(lit);
      } else {
        // Use model to refine upper bound
        for (final Integer candidateLit : new ArrayList<>(candidates)) {
          if (isUPZeroLit(var(candidateLit))) {
            candidates.remove(candidateLit);
            addBackboneLiteral(candidateLit);
          } else if (this.model.get(var(candidateLit)) == sign(candidateLit)) {
            candidates.remove(candidateLit);
          }
          //        }else if (isRotatable(candidateLit)) {
          //            candidates.remove(candidateLit);
          //          }
        }
      }
    }
  }

  /**
   * Transforms a solver integer literal to the corresponding formula literal.
   * @param lit the solver literal
   * @return the formula literal
   */
  private Literal intLiteralToLiteral(final int lit) {
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