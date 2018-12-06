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
package org.logicng.transformations.qmc;

import org.logicng.cardinalityconstraints.CCIncrementalData;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * An implementation of the Quine-McCluskey algorithm for minimizing canonical DNFs.  This implementation is fairly
 * efficient but keep in mind that Quine-McCluskey can be a very expensive procedure for large canonical DNFs.  So we
 * would not recommend using this implementation for super large DNFs.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskeyAlgorithm {

  /**
   * Computes a minimized DNF for a given formula projected to a given set of variables.  First a projected canonical
   * DNF of the formula is computed for the given variables.  Then for this canonical DNF the Quine-McCluskey algorithm
   * is computed.
   * @param formula           the formula
   * @param relevantVariables the relevant formulas for the projected canonical DNF
   * @return the minimized DNF due to Quine-McCluskey
   */
  public static Formula compute(final Formula formula, final Collection<Variable> relevantVariables) {
    final FormulaFactory f = formula.factory();
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(formula);
    final List<Assignment> models = relevantVariables == null ? solver.enumerateAllModels(formula.variables()) : solver.enumerateAllModels(relevantVariables);
    return compute(models, f);
  }

  /**
   * Computes a minimized DNF for a given formula.  First the canonical DNF of the formula is computed.  Then for
   * this canonical DNF the Quine-McCluskey algorithm is computed.
   * @param formula the formula
   * @return the minimized DNF due to Quine-McCluskey
   */
  public static Formula compute(final Formula formula) {
    return compute(formula, formula.variables());
  }

  /**
   * Computes a minimized DNF for a given set of models.  These models have to represent a canonical DNF of a formula
   * as computed e.g. by the projected model enumeration of a SAT solver {@link SATSolver#enumerateAllModels(Variable[])}
   * or by the transformation {@link org.logicng.transformations.dnf.CanonicalDNFEnumeration}.
   * @param models the models of the formula
   * @param f      the formula factory
   * @return the minimized DNF due to Quine-McCluskey
   */
  public static Formula compute(final List<Assignment> models, final FormulaFactory f) {
    if (models.isEmpty())
      return f.falsum();
    if (models.size() == 1)
      return models.get(0).formula(f);
    final List<Variable> varOrder = new ArrayList<>(models.get(0).positiveLiterals());
    varOrder.addAll(models.get(0).negativeVariables());
    Collections.sort(varOrder);
    final List<Term> terms = transformModels2Terms(models, varOrder, f);
    final LinkedHashSet<Term> primeImplicants = computePrimeImplicants(terms);
    final TermTable primeTermTable = new TermTable(primeImplicants);
    primeTermTable.simplifyTableByDominance();
    final List<Term> chosenTerms = chooseSatBased(primeTermTable, f);
    return computeFormula(chosenTerms, varOrder);
  }

  /**
   * Transforms a given list of models to a term list as used in the QMC implementation.
   * @param models   the models
   * @param varOrder the variable ordering
   * @param f        the formula factory
   * @return the list of terms
   */
  private static List<Term> transformModels2Terms(final List<Assignment> models, final List<Variable> varOrder,
                                                  final FormulaFactory f) {
    final List<Term> terms = new ArrayList<>(models.size());
    for (final Assignment model : models) {
      final List<Literal> minterm = new ArrayList<>();
      for (final Variable variable : varOrder)
        minterm.add(model.evaluateLit(variable) ? variable : variable.negate());
      terms.add(convertToTerm(minterm, f));
    }
    return terms;
  }

  /**
   * Computes the list of all prime implicants for a given set of terms.
   * @param terms the terms
   * @return all prime implicants for the given terms
   */
  static LinkedHashSet<Term> computePrimeImplicants(final List<Term> terms) {
    SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = generateInitialTermClasses(terms);
    SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = combineInTermClasses(termsInClasses);
    final LinkedHashSet<Term> primeImplicants = getUnusedTerms(termsInClasses);
    while (!newTermsInClasses.isEmpty()) {
      termsInClasses = newTermsInClasses;
      newTermsInClasses = combineInTermClasses(termsInClasses);
      primeImplicants.addAll(getUnusedTerms(termsInClasses));
    }
    return primeImplicants;
  }

  /**
   * Computes the combination of all terms in two adjacent classes.
   * @param termsInClasses the terms sorted in term classes
   * @return the combined terms
   */
  static SortedMap<Integer, LinkedHashSet<Term>> combineInTermClasses(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = new TreeMap<>();
    for (int i = 0; i < termsInClasses.lastKey(); i++) {
      final LinkedHashSet<Term> thisClass = termsInClasses.get(i);
      final LinkedHashSet<Term> otherClass = termsInClasses.get(i + 1);
      if (thisClass != null && otherClass != null) {
        for (final Term thisTerm : thisClass) {
          for (final Term otherTerm : otherClass) {
            final Term combined = thisTerm.combine(otherTerm);
            if (combined != null) {
              thisTerm.setUsed(true);
              otherTerm.setUsed(true);
              LinkedHashSet<Term> foundTerms = newTermsInClasses.get(combined.termClass());
              if (foundTerms == null) {
                foundTerms = new LinkedHashSet<>();
                newTermsInClasses.put(combined.termClass(), foundTerms);
              }
              foundTerms.add(combined);
            }
          }
        }
      }
    }
    return newTermsInClasses;
  }

  /**
   * Computes the unused terms in set of terms.  These terms are the prime implicants in the QMC.
   * @param termsInClasses the terms sorted in term classes
   * @return the unused terms in the given set of terms
   */
  private static LinkedHashSet<Term> getUnusedTerms(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final LinkedHashSet<Term> unusedTerms = new LinkedHashSet<>();
    for (final Map.Entry<Integer, LinkedHashSet<Term>> entry : termsInClasses.entrySet())
      for (final Term term : entry.getValue())
        if (!term.isUsed())
          unusedTerms.add(term);
    return unusedTerms;
  }

  /**
   * Computes and returns the initial term classes for a given list of terms.
   * @param terms the terms
   * @return the terms sorted in term classes
   */
  static SortedMap<Integer, LinkedHashSet<Term>> generateInitialTermClasses(final List<Term> terms) {
    final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = new TreeMap<>();
    for (final Term term : terms) {
      LinkedHashSet<Term> presentTerms = termsInClasses.get(term.termClass());
      if (presentTerms == null) {
        presentTerms = new LinkedHashSet<>();
        termsInClasses.put(term.termClass(), presentTerms);
      }
      presentTerms.add(term);
    }
    return termsInClasses;
  }

  /**
   * Computes the formula for a given list of chosen terms and a variable order.
   * @param chosenTerms the chosen terms
   * @param varOrder    the variable order
   * @return the formula for this term list and variable ordering
   */
  private static Formula computeFormula(final List<Term> chosenTerms, final List<Variable> varOrder) {
    final FormulaFactory f = varOrder.get(0).factory();
    final List<Formula> operands = new ArrayList<>(chosenTerms.size());
    for (final Term term : chosenTerms)
      operands.add(term.translateToFormula(varOrder));
    return f.or(operands);
  }

  /**
   * Converts a given minterm to a QMC internal term.
   * @param minterm the minterm as list of literals
   * @param f       the formula factory
   * @return the term for this minterm
   */
  static Term convertToTerm(final List<Literal> minterm, final FormulaFactory f) {
    final Tristate[] bits = new Tristate[minterm.size()];
    for (int i = 0; i < minterm.size(); i++)
      bits[i] = Tristate.fromBool(minterm.get(i).phase());
    return new Term(bits, Collections.singletonList(f.and(minterm)));
  }

  /**
   * Choose a minimal set of prime implicants from the final prime implicant table of QMC.  This implementation uses
   * a MaxSAT formulation for this variant of the SET COVER problem which should be more efficient than e.g. Petrick's
   * method for all cases in which this Quine-McCluskey implementation yields a result in reasonable time.
   * @param table the final prime term table
   * @param f     the formula factory
   * @return the list of chosen prime implicants which are minimal wrt. to their number
   */
  static List<Term> chooseSatBased(final TermTable table, final FormulaFactory f) {
    final LinkedHashMap<Variable, Term> var2Term = new LinkedHashMap<>();
    final LinkedHashMap<Formula, Variable> formula2VarMapping = new LinkedHashMap<>();
    final SATSolver satSolver = initializeSolver(table, f, var2Term, formula2VarMapping);
    if (satSolver.sat() == Tristate.FALSE)
      throw new IllegalStateException("Solver must be satisfiable after adding the initial formula.");
    return minimize(satSolver, var2Term, f);
  }

  /**
   * Initializes the SAT solver for the SET COVER problem formulation.
   * @param table              the prime implicant table
   * @param f                  the formula factory
   * @param var2Term           a mapping from selector variable to prime implicant
   * @param formula2VarMapping a mapping form minterm to variable
   * @return the initialized SAT solver
   */
  private static SATSolver initializeSolver(final TermTable table, final FormulaFactory f, final LinkedHashMap<Variable, Term> var2Term, final LinkedHashMap<Formula, Variable> formula2VarMapping) {
    final LinkedHashMap<Variable, List<Variable>> minterm2Variants = new LinkedHashMap<>();
    int count = 0;
    String prefix = "@MINTERM_SEL_";
    for (final Formula formula : table.columnHeaders()) {
      final Variable selector = f.variable(prefix + count++);
      formula2VarMapping.put(formula, selector);
      minterm2Variants.put(selector, new ArrayList<Variable>());
    }
    count = 0;
    prefix = "@TERM_SEL_";

    final SATSolver solver = MiniSat.miniSat(f);
    for (final Term term : table.lineHeaders()) {
      final Variable termSelector = f.variable(prefix + count);
      var2Term.put(termSelector, term);
      final List<Variable> mintermSelectors = new ArrayList<>();
      for (final Formula formula : term.minterms()) {
        final Variable mintermSelector = formula2VarMapping.get(formula);
        if (mintermSelector != null) {
          final Variable selectorVariant = f.variable(mintermSelector.name() + "_" + count);
          minterm2Variants.get(mintermSelector).add(selectorVariant);
          mintermSelectors.add(selectorVariant);
        }
      }
      final List<Literal> operands = new ArrayList<>();
      for (int i = 0; i < mintermSelectors.size(); i++) {
        final Variable mintermSelector = mintermSelectors.get(i);
        solver.add(f.clause(termSelector.negate(), mintermSelector));
        operands.add(mintermSelector.negate());
        for (int j = i + 1; j < mintermSelectors.size(); j++) {
          final Variable mintermSelector2 = mintermSelectors.get(j);
          solver.add(f.or(mintermSelector.negate(), mintermSelector2));
          solver.add(f.or(mintermSelector2.negate(), mintermSelector));
        }
      }
      operands.add(termSelector);
      solver.add(f.clause(operands));
      count++;
    }
    for (final List<Variable> variables : minterm2Variants.values())
      solver.add(f.clause(variables));
    return solver;
  }

  /**
   * Performs the minimization via incremental cardinality constraints.
   * @param satSolver the SAT solver
   * @param var2Term  a mapping from selector variable to prime implicant
   * @param f         the formula factory
   * @return a minimal set of prime implicants to cover all minterms
   */
  private static List<Term> minimize(final SATSolver satSolver, final LinkedHashMap<Variable, Term> var2Term, final FormulaFactory f) {
    final Assignment initialModel = satSolver.model();
    List<Variable> currentTermVars = computeCurrentTermVars(initialModel, var2Term.keySet());
    if (currentTermVars.size() == 2) {
      satSolver.add(f.amo(var2Term.keySet()));
      if (satSolver.sat() == Tristate.TRUE)
        currentTermVars = computeCurrentTermVars(satSolver.model(), var2Term.keySet());
    } else {
      final CCIncrementalData incData = satSolver.addIncrementalCC(f.cc(CType.LE, currentTermVars.size() - 1, var2Term.keySet()));
      while (satSolver.sat() == Tristate.TRUE) {
        currentTermVars = computeCurrentTermVars(satSolver.model(), var2Term.keySet());
        incData.newUpperBoundForSolver(currentTermVars.size() - 1);
      }
    }
    return computeTerms(currentTermVars, var2Term);
  }

  /**
   * Computes the terms from a given list of selector variables
   * @param currentTermVars the list of selector variables
   * @param var2Term        a mapping from selector variable to prime implicant
   * @return the terms for the given list
   */
  private static List<Term> computeTerms(final List<Variable> currentTermVars, final LinkedHashMap<Variable, Term> var2Term) {
    final List<Term> terms = new ArrayList<>(currentTermVars.size());
    for (final Variable currentTermVar : currentTermVars)
      terms.add(var2Term.get(currentTermVar));
    return terms;
  }

  /**
   * Computes the prime implicant selector variables for a given model.
   * @param model the model
   * @param vars  the list of selector variables
   * @return the selector variables in the given model
   */
  private static List<Variable> computeCurrentTermVars(final Assignment model, final Collection<Variable> vars) {
    final List<Variable> result = new ArrayList<>();
    for (final Variable var : vars)
      if (model.evaluateLit(var))
        result.add(var);
    return result;
  }
}
