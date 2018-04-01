package org.logicng.bdds.orderings;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * A depth-first-search BDD variable ordering.  Traverses the formula in a DFS manner
 * and gathers all variables in the occurrence.
 * @version 1.4.0
 * @since 1.4.0
 */
public class DFSOrdering implements VariableOrderingProvider {

  @Override
  public List<Variable> getOrder(final Formula formula) {
    final LinkedHashSet<Variable> order = new LinkedHashSet<>(formula.variables().size());
    dfs(formula, order);
    return new ArrayList<>(order);
  }

  private void dfs(final Formula formula, final LinkedHashSet<Variable> variables) {
    switch (formula.type()) {
      case LITERAL:
        variables.add(((Literal) formula).variable());
        break;
      case NOT:
        dfs(((Not) formula).operand(), variables);
        break;
      case IMPL:
      case EQUIV:
        final BinaryOperator op = (BinaryOperator) formula;
        dfs(op.left(), variables);
        dfs(op.right(), variables);
        break;
      case AND:
      case OR:
        for (final Formula operand : formula)
          dfs(operand, variables);
        break;
      case PBC:
        final PBConstraint pbc = (PBConstraint) formula;
        for (final Literal lit : pbc.operands())
          variables.add(lit.variable());
        break;
    }
  }
}
