package org.logicng.extendedFormulaFactory;

import org.logicng.formulas.CFalse;
import org.logicng.formulas.CTrue;
import org.logicng.formulas.CType;
import org.logicng.formulas.ExtendedFormulaFactory;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactoryState;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * A formula factory to test the extended formula factory
 */
public class ExTestFormulaFactory extends ExtendedFormulaFactory {

  private final List<FormulaFactoryState> states;

  private int activityCounter;

  public ExTestFormulaFactory() {
    super();
    this.states = new ArrayList<>();
    this.activityCounter = 0;
  }

  @Override
  public Formula implication(Formula left, Formula right) {
    activate();
    return super.implication(left, right);
  }

  @Override
  public Formula equivalence(Formula left, Formula right) {
    activate();
    return super.equivalence(left, right);
  }

  @Override
  public CTrue verum() {
    activate();
    return super.verum();
  }

  @Override
  public Formula not(Formula operand) {
    activate();
    return super.not(operand);
  }

  @Override
  public CFalse falsum() {
    activate();
    return super.falsum();
  }

  @Override
  public Formula and(Formula... operands) {
    activate();
    return super.and(operands);
  }

  @Override
  public Formula and(Collection<? extends Formula> operands) {
    activate();
    return super.and(operands);
  }

  @Override
  public Formula cnf(Formula... clauses) {
    activate();
    return super.cnf(clauses);
  }

  @Override
  public Formula cnf(Collection<? extends Formula> clauses) {
    activate();
    return super.cnf(clauses);
  }

  @Override
  public Formula or(Formula... operands) {
    activate();
    return super.or(operands);
  }

  @Override
  public Formula or(Collection<? extends Formula> operands) {
    activate();
    return super.or(operands);
  }

  @Override
  public Formula clause(Literal... literals) {
    activate();
    return super.clause(literals);
  }

  @Override
  public Formula clause(Collection<? extends Literal> literals) {
    activate();
    return super.clause(literals);
  }

  @Override
  public Literal literal(String name, boolean phase) {
    activate();
    return super.literal(name, phase);
  }

  @Override
  public Variable variable(String name) {
    activate();
    return super.variable(name);
  }

  @Override
  public PBConstraint pbc(CType comparator, int rhs, List<? extends Literal> literals, List<Integer> coefficients) {
    activate();
    return super.pbc(comparator, rhs, literals, coefficients);
  }

  @Override
  public PBConstraint pbc(CType comparator, int rhs, Literal[] literals, int[] coefficients) {
    activate();
    return super.pbc(comparator, rhs, literals, coefficients);
  }

  @Override
  public PBConstraint cc(CType comparator, int rhs, Collection<Variable> variables) {
    activate();
    return super.cc(comparator, rhs, variables);
  }

  @Override
  public PBConstraint cc(CType comparator, int rhs, Variable... variables) {
    activate();
    return super.cc(comparator, rhs, variables);
  }

  @Override
  public PBConstraint amo(Collection<Variable> variables) {
    activate();
    return super.amo(variables);
  }

  @Override
  public PBConstraint amo(Variable... variables) {
    activate();
    return super.amo(variables);
  }

  @Override
  public PBConstraint exo(Collection<Variable> variables) {
    activate();
    return super.exo(variables);
  }

  @Override
  public PBConstraint exo(Variable... variables) {
    activate();
    return super.exo(variables);
  }

  @Override
  public Variable newCCVariable() {
    activate();
    return super.newCCVariable();
  }

  @Override
  public Variable newPBVariable() {
    activate();
    return super.newPBVariable();
  }

  @Override
  public Variable newCNFVariable() {
    activate();
    return super.newCNFVariable();
  }

  private void activate() {
    System.out.println("Activate " + activityCounter + " (" + states.size() + ")");
    //TODO save load states and stuff
    if (activityCounter % 10 == 0) {
      states.add(this.save());
      System.out.println("Saving state");
    }
    if (activityCounter % 20 == 5) {
      int lastIndex = states.size() - 1;
      load(states.get(lastIndex));
      states.remove(lastIndex);
      System.out.println("Loading state");
    }


    activityCounter++;
  }
}
