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
//  Copyright 2015-20xx Christoph Zengler                                //
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

package org.logicng.modelcounting;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.knowledgecompilation.dnnf.DnnfFactory;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfModelCountFunction;
import org.logicng.transformations.PureExpansionTransformation;
import org.logicng.transformations.cnf.CNFConfig;
import org.logicng.transformations.cnf.CNFEncoder;
import org.logicng.util.FormulaHelper;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * A model counter for large formulas.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class ModelCounter {

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private ModelCounter() {
        // Intentionally left empty
    }

    /**
     * Computes the model count for a given set of formulas (interpreted as conjunction)
     * and a set of relevant variables.  This set can only be a superset of the original
     * formulas' variables.  No projected model counting is supported.
     * @param formulas  the list of formulas
     * @param variables the relevant variables
     * @return the model count of the formulas for the variables
     */
    public static BigInteger count(final Collection<Formula> formulas, final SortedSet<Variable> variables) {
        if (!variables.containsAll(FormulaHelper.variables(formulas))) {
            throw new IllegalArgumentException("Expected variables to contain all of the formulas' variables.");
        }
        if (variables.isEmpty()) {
            final List<Formula> remainingConstants = formulas.stream().filter(formula -> formula.type() != FType.TRUE).collect(Collectors.toList());
            return remainingConstants.isEmpty() ? BigInteger.ONE : BigInteger.ZERO;
        }
        final FormulaFactory f = variables.first().factory();
        final List<Formula> cnfs = encodeAsCnf(formulas, f);
        final SimplificationResult simplification = simplify(cnfs);
        final BigInteger count = count(simplification.simplifiedFormulas, f);
        final SortedSet<Variable> dontCareVariables = simplification.getDontCareVariables(variables);
        return count.multiply(BigInteger.valueOf(2).pow(dontCareVariables.size()));
    }

    private static List<Formula> encodeAsCnf(final Collection<Formula> formulas, final FormulaFactory f) {
        final PureExpansionTransformation expander = new PureExpansionTransformation();
        final List<Formula> expandedFormulas = formulas.stream().map(formula -> formula.transform(expander)).collect(Collectors.toList());
        final CNFEncoder cnfEncoder = new CNFEncoder(f, CNFConfig.builder()
                .algorithm(CNFConfig.Algorithm.ADVANCED)
                .fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.TSEITIN).build());
        return expandedFormulas.stream().map(cnfEncoder::encode).collect(Collectors.toList());
    }

    private static SimplificationResult simplify(final Collection<Formula> formulas) {
        final Assignment simpleBackbone = new Assignment();
        final SortedSet<Variable> backboneVariables = new TreeSet<>();
        for (final Formula formula : formulas) {
            if (formula.type() == FType.LITERAL) {
                final Literal lit = (Literal) formula;
                simpleBackbone.addLiteral(lit);
                backboneVariables.add(lit.variable());
            }
        }
        final List<Formula> simplified = new ArrayList<>();
        for (final Formula formula : formulas) {
            final Formula restrict = formula.restrict(simpleBackbone);
            if (restrict.type() != FType.TRUE) {
                simplified.add(restrict);
            }
        }
        return new SimplificationResult(simplified, backboneVariables);
    }

    private static BigInteger count(final Collection<Formula> formulas, final FormulaFactory f) {
        final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromFormulas(formulas);
        final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
        final List<List<Formula>> components = ConnectedComponentsComputation.splitFormulasByComponent(formulas, ccs);
        final DnnfFactory factory = new DnnfFactory();
        BigInteger count = BigInteger.ONE;
        for (final List<Formula> component : components) {
            final Dnnf dnnf = factory.compile(f.and(component));
            count = count.multiply(dnnf.execute(DnnfModelCountFunction.get()));
        }
        return count;
    }

    private static class SimplificationResult {
        private final List<Formula> simplifiedFormulas;
        private final SortedSet<Variable> backboneVariables;

        public SimplificationResult(final List<Formula> simplifiedFormulas, final SortedSet<Variable> backboneVariables) {
            this.simplifiedFormulas = simplifiedFormulas;
            this.backboneVariables = backboneVariables;
        }

        public SortedSet<Variable> getDontCareVariables(final SortedSet<Variable> variables) {
            final SortedSet<Variable> dontCareVariables = new TreeSet<>(variables);
            dontCareVariables.removeAll(FormulaHelper.variables(this.simplifiedFormulas));
            dontCareVariables.removeAll(this.backboneVariables);
            return dontCareVariables;
        }
    }
}
