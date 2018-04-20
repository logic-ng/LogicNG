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
 * An implementation of the Quine-McCluskey algorithm for minimizing canonical DNFs.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskeyAlgorithm {

  static Formula compute(final Formula formula) {
    final FormulaFactory f = formula.factory();
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(formula);
    final List<Assignment> models = solver.enumerateAllModels();
    return compute(models, f);
  }

  static Formula compute(final List<Assignment> models, final FormulaFactory f) {
    if (models.isEmpty())
      return f.falsum();
    if (models.size() == 1)
      return models.get(0).formula(f);
    final List<Variable> varOrder = new ArrayList<>(models.get(0).positiveLiterals());
    varOrder.addAll(models.get(0).negativeVariables());
    Collections.sort(varOrder);
    final long t1 = System.currentTimeMillis();
    final List<Term> terms = transformModels2Terms(models, varOrder, f);
    final long t2 = System.currentTimeMillis();
    final LinkedHashSet<Term> primeImplicants = computePrimeImplicants(terms);
    final long t3 = System.currentTimeMillis();
    final TermTable primeTermTable = new TermTable(primeImplicants);
    final long t4 = System.currentTimeMillis();
    primeTermTable.simplifyTableByDominance();
    final long t5 = System.currentTimeMillis();
    final List<Term> chosenTerms = chooseSatBased(primeTermTable, f);
    final long t6 = System.currentTimeMillis();

    System.out.println("Time 1: " + (t2 - t1) + " ms.");
    System.out.println("Time 2: " + (t3 - t2) + " ms.");
    System.out.println("Time 3: " + (t4 - t3) + " ms.");
    System.out.println("Time 4: " + (t5 - t4) + " ms.");
    System.out.println("Time 5: " + (t6 - t5) + " ms.");
    return computeFormula(chosenTerms, varOrder);
  }

  private static List<Term> transformModels2Terms(final List<Assignment> models, final List<Variable> varOrder,
                                                  final FormulaFactory f) {
    final List<Term> terms = new ArrayList<>(models.size());
    if (models.isEmpty())
      return terms;
    for (final Assignment model : models) {
      final List<Literal> minterm = new ArrayList<>();
      for (final Variable variable : varOrder)
        minterm.add(model.evaluateLit(variable) ? variable : variable.negate());
      terms.add(convertToTerm(minterm, f));
    }
    return terms;
  }

  static LinkedHashSet<Term> computePrimeImplicants(final List<Term> terms) {
    SortedMap<Integer, LinkedHashSet<Term>> termsInClasses = generateInitialTermClasses(terms);
    SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = uniteInTermClasses(termsInClasses);
    final LinkedHashSet<Term> primeImplicants = getUnusedTerms(termsInClasses);
    while (!newTermsInClasses.isEmpty()) {
      termsInClasses = newTermsInClasses;
      newTermsInClasses = uniteInTermClasses(termsInClasses);
      primeImplicants.addAll(getUnusedTerms(termsInClasses));
    }
    return primeImplicants;
  }

  static SortedMap<Integer, LinkedHashSet<Term>> uniteInTermClasses(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final SortedMap<Integer, LinkedHashSet<Term>> newTermsInClasses = new TreeMap<>();
    for (int i = 0; i < termsInClasses.lastKey(); i++) {
      final LinkedHashSet<Term> thisClass = termsInClasses.get(i);
      final LinkedHashSet<Term> otherClass = termsInClasses.get(i + 1);
      if (thisClass != null && otherClass != null) {
        for (final Term thisTerm : thisClass) {
          for (final Term otherTerm : otherClass) {
            final Term unite = thisTerm.unite(otherTerm);
            if (unite != null) {
              thisTerm.setUsed(true);
              otherTerm.setUsed(true);
              LinkedHashSet<Term> foundTerms = newTermsInClasses.get(unite.termClass());
              if (foundTerms == null) {
                foundTerms = new LinkedHashSet<>();
                newTermsInClasses.put(unite.termClass(), foundTerms);
              }
              foundTerms.add(unite);
            }
          }
        }
      }
    }
    return newTermsInClasses;
  }

  private static LinkedHashSet<Term> getUnusedTerms(final SortedMap<Integer, LinkedHashSet<Term>> termsInClasses) {
    final LinkedHashSet<Term> unusedTerms = new LinkedHashSet<>();
    for (final Map.Entry<Integer, LinkedHashSet<Term>> entry : termsInClasses.entrySet())
      for (final Term term : entry.getValue())
        if (!term.isUsed())
          unusedTerms.add(term);
    return unusedTerms;
  }

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

  private static Formula computeFormula(final List<Term> chosenTerms, final List<Variable> varOrder) {
    final FormulaFactory f = varOrder.get(0).factory();
    final List<Formula> operands = new ArrayList<>(chosenTerms.size());
    for (final Term term : chosenTerms)
      operands.add(term.translateToFormula(varOrder));
    return f.or(operands);
  }

  static Term convertToTerm(final List<Literal> minterm, final FormulaFactory f) {
    final Tristate[] bits = new Tristate[minterm.size()];
    for (int i = 0; i < minterm.size(); i++)
      bits[i] = Tristate.fromBool(minterm.get(i).phase());
    return new Term(bits, Collections.singletonList(f.and(minterm)));
  }

  static List<Term> chooseSatBased(final TermTable table, final FormulaFactory f) {
    final LinkedHashMap<Variable, Term> var2Term = new LinkedHashMap<>();
    final LinkedHashMap<Formula, Variable> formula2VarMapping = new LinkedHashMap<>();
    final SATSolver satSolver = initializeSolver(table, f, var2Term, formula2VarMapping);
    if (satSolver.sat() == Tristate.FALSE)
      throw new IllegalStateException("Solver must be satisfiable after adding the initial formula.");
    return minimize(satSolver, var2Term, f);
  }

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

  private static List<Term> computeTerms(final List<Variable> currentTermVars, final LinkedHashMap<Variable, Term> var2Term) {
    final List<Term> terms = new ArrayList<>(currentTermVars.size());
    for (final Variable currentTermVar : currentTermVars)
      terms.add(var2Term.get(currentTermVar));
    return terms;
  }

  private static List<Variable> computeCurrentTermVars(final Assignment model, final Collection<Variable> vars) {
    final List<Variable> result = new ArrayList<>();
    for (final Variable var : vars)
      if (model.evaluateLit(var))
        result.add(var);
    return result;
  }
}
