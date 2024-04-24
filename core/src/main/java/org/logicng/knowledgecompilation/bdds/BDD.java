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

package org.logicng.knowledgecompilation.bdds;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.datastructures.BDDNode;
import org.logicng.knowledgecompilation.bdds.functions.BDDCNFFunction;
import org.logicng.knowledgecompilation.bdds.functions.BDDDNFFunction;
import org.logicng.knowledgecompilation.bdds.functions.BDDFunction;
import org.logicng.knowledgecompilation.bdds.functions.BDDModelEnumerationFunction;
import org.logicng.knowledgecompilation.bdds.functions.LngBDDFunction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDConstruction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDReordering;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * The internal representation of a BDD.
 * @version 2.4.0
 * @since 1.4.0
 */
public class BDD {

    private final int index;
    protected final BDDKernel kernel;
    protected final BDDConstruction construction;
    protected final BDDOperations operations;

    /**
     * Constructs a new BDD with a given index.
     * @param index  the index
     * @param kernel the kernel of this BDD
     */
    public BDD(final int index, final BDDKernel kernel) {
        this.index = index;
        this.kernel = kernel;
        this.construction = new BDDConstruction(kernel);
        this.operations = new BDDOperations(kernel);
    }

    /**
     * Returns the index of this BDD.
     * <p>
     * The index marks the entry point of this BDD in the {@link #underlyingKernel() underlying kernel}.
     * @return the index of this BDD
     */
    public int index() {
        return this.index;
    }

    /**
     * Returns the BDD Kernel of this factory.  The Kernel should only be accessed when you know, what you are doing.
     * @return the BDD Kernel
     */
    public BDDKernel underlyingKernel() {
        return this.kernel;
    }

    /**
     * Applies a given function on this BDD and returns the result.
     * @param function the function
     * @param <T>      the result type of the function
     * @return the result of the function application
     */
    public <T> T apply(final BDDFunction<T> function) {
        return function.apply(this);
    }

    /**
     * Returns a formula representation of this BDD.  This is done by using the Shannon expansion.
     * @return the formula for this BDD
     */
    public Formula toFormula() {
        return this.operations.toFormula(this.index, true);
    }

    /**
     * Returns a formula representation of this BDD.  This is done by using the Shannon expansion.
     * If {@code followPathsToTrue} is activated, the paths leading to the {@code true} terminal are followed to generate the formula.
     * If {@code followPathsToTrue} is deactivated, the paths leading to the {@code false} terminal are followed to generate the formula and the resulting formula is negated.
     * Depending on the formula and the number of satisfying assignments, the generated formula can be more compact using the {@code true} paths
     * or {@code false} paths, respectively.
     * @param followPathsToTrue the extraction style
     * @return the formula for this BDD
     */
    public Formula toFormula(final boolean followPathsToTrue) {
        return this.operations.toFormula(this.index, followPathsToTrue);
    }

    /**
     * Returns a new BDD which is the negation of this BDD.
     * @return the negation of this BDD
     */
    public BDD negate() {
        return new BDD(this.kernel.addRef(this.construction.not(this.index), null), this.kernel);
    }

    /**
     * Returns a new BDD which is the implication of this BDD to the given other BDD.  Both BDDs must use the same kernel.
     * @param other the other BDD
     * @return the implication from this BDD to the other BDD
     * @throws IllegalArgumentException if the two BDDs don't have the same kernel
     */
    public BDD implies(final BDD other) {
        if (other.kernel != this.kernel) {
            throw new IllegalArgumentException("Only BDDs with the same kernel can be processed");
        }
        return new BDD(this.kernel.addRef(this.construction.implication(this.index, other.index), null), this.kernel);
    }

    /**
     * Returns a new BDD which is the implication of the other given BDD to this BDD.  Both BDDs must use the same kernel.
     * @param other the other BDD
     * @return the implication from the other BDD to this BDD
     * @throws IllegalArgumentException if the two BDDs don't have the same kernel
     */
    public BDD impliedBy(final BDD other) {
        if (other.kernel != this.kernel) {
            throw new IllegalArgumentException("Only BDDs with the same kernel can be processed");
        }
        return new BDD(this.kernel.addRef(this.construction.implication(other.index, this.index), null), this.kernel);
    }

    /**
     * Returns a new BDD which is the equivalence of this BDD and the other given BDD.  Both BDDs must use the same kernel.
     * @param other the other BDD
     * @return the equivalence of this and the other BDD
     * @throws IllegalArgumentException if the two BDDs don't have the same kernel
     */
    public BDD equivalence(final BDD other) {
        if (other.kernel != this.kernel) {
            throw new IllegalArgumentException("Only BDDs with the same kernel can be processed");
        }
        return new BDD(this.kernel.addRef(this.construction.equivalence(this.index, other.index), null), this.kernel);
    }

    /**
     * Returns a new BDD which is the conjunction of this BDD and the given other BDD.  Both BDDs must use the same kernel.
     * @param other the other BDD
     * @return the conjunction of the two BDDs
     * @throws IllegalArgumentException if the two BDDs don't have the same kernel
     */
    public BDD and(final BDD other) {
        if (other.kernel != this.kernel) {
            throw new IllegalArgumentException("Only BDDs with the same kernel can be processed");
        }
        return new BDD(this.kernel.addRef(this.construction.and(this.index, other.index), null), this.kernel);
    }

    /**
     * Returns a new BDD which is the disjunction of this BDD and the given other BDD.  Both BDDs must use the same kernel.
     * @param other the other BDD
     * @return the disjunction of the two BDDs
     * @throws IllegalArgumentException if the two BDDs don't have the same kernel
     */
    public BDD or(final BDD other) {
        if (other.kernel != this.kernel) {
            throw new IllegalArgumentException("Only BDDs with the same kernel can be processed");
        }
        return new BDD(this.kernel.addRef(this.construction.or(this.index, other.index), null), this.kernel);
    }

    /**
     * Returns {@code true} if this BDD is a tautology, {@code false} otherwise.
     * @return {@code true} if this BDD is a tautology, {@code false} otherwise
     */
    public boolean isTautology() {
        return this.index == BDDKernel.BDD_TRUE;
    }

    /**
     * Returns {@code true} if this BDD is a contradiction, {@code false} otherwise.
     * @return {@code true} if this BDD is a contradiction, {@code false} otherwise
     */
    public boolean isContradiction() {
        return this.index == BDDKernel.BDD_FALSE;
    }

    /**
     * Returns the model count of this BDD.
     * @return the model count
     */
    public BigInteger modelCount() {
        return this.operations.satCount(this.index);
    }

    /**
     * Enumerates all models of this BDD.
     * @return the list of all models
     */
    public List<Assignment> enumerateAllModels() {
        return enumerateAllModels((Collection<Variable>) null);
    }

    /**
     * Enumerates all models of this BDD wrt. a given set of variables.
     * @param variables the variables
     * @return the list of all models
     */
    public List<Assignment> enumerateAllModels(final Variable... variables) {
        return this.enumerateAllModels(Arrays.asList(variables));
    }

    /**
     * Enumerates all models of this BDD wrt. a given set of variables.
     * @param variables the variables
     * @return the list of all models
     */
    public List<Assignment> enumerateAllModels(final Collection<Variable> variables) {
        return this.apply(new BDDModelEnumerationFunction(variables));
    }

    /**
     * Returns a CNF formula for this BDD.
     * @return the CNF for the formula represented by this BDD
     */
    public Formula cnf() {
        return this.apply(BDDCNFFunction.get());
    }

    /**
     * Returns the number of clauses for the CNF formula of the BDD.
     * @return the number of clauses for the CNF formula of the BDD
     */
    public BigInteger numberOfClausesCNF() {
        return this.operations.pathCountZero(this.index);
    }

    /**
     * Returns a DNF formula for this BDD.
     * @return the DNF for the formula represented by this BDD
     */
    public Formula dnf() {
        return this.apply(BDDDNFFunction.get());
    }

    /**
     * Restricts the BDD.
     * @param restriction the restriction
     * @return the restricted BDD
     */
    public BDD restrict(final Collection<Literal> restriction) {
        final BDD resBDD = BDDFactory.build(this.kernel.factory().and(restriction), this.kernel, null);
        return new BDD(this.construction.restrict(this.index, resBDD.index), this.kernel);
    }

    /**
     * Restricts the BDD.
     * @param restriction the restriction
     * @return the restricted BDD
     */
    public BDD restrict(final Literal... restriction) {
        return restrict(Arrays.asList(restriction));
    }

    /**
     * Existential quantifier elimination for a given set of variables.
     * @param variables the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public BDD exists(final Collection<Variable> variables) {
        final BDD resBDD = BDDFactory.build(this.kernel.factory().and(variables), this.kernel);
        return new BDD(this.construction.exists(this.index, resBDD.index), this.kernel);
    }

    /**
     * Existential quantifier elimination for a given set of variables.
     * @param variables the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public BDD exists(final Variable... variables) {
        return exists(Arrays.asList(variables));
    }

    /**
     * Universal quantifier elimination for a given set of variables.
     * @param variables the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public BDD forall(final Collection<Variable> variables) {
        final BDD resBDD = BDDFactory.build(this.kernel.factory().and(variables), this.kernel);
        return new BDD(this.construction.forAll(this.index, resBDD.index), this.kernel);
    }

    /**
     * Universal quantifier elimination for a given set of variables.
     * @param variables the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public BDD forall(final Variable... variables) {
        return forall(Arrays.asList(variables));
    }

    /**
     * Returns an arbitrary model of this BDD or {@code null} if there is none.
     * @return an arbitrary model of this BDD
     */
    public Assignment model() {
        return createAssignment(this.operations.satOne(this.index));
    }

    /**
     * Returns an arbitrary model of this BDD which contains at least the given variables or {@code null} if there is
     * none.  If a variable is a don't care variable, it will be assigned with the given default value.
     * @param defaultValue the default value for don't care variables
     * @param variables    the set of variable which has to be contained in the model
     * @return an arbitrary model of this BDD
     */
    public Assignment model(final boolean defaultValue, final Collection<Variable> variables) {
        final int varBDD = BDDFactory.build(this.kernel.factory().and(variables), this.kernel, null).index;
        final int pol = defaultValue ? BDDKernel.BDD_TRUE : BDDKernel.BDD_FALSE;
        final int modelBDD = this.operations.satOneSet(this.index, varBDD, pol);
        return createAssignment(modelBDD);
    }

    /**
     * Returns an arbitrary model of this BDD which contains at least the given variables or {@code null} if there is
     * none.  If a variable is a don't care variable, it will be assigned with the given default value.
     * @param defaultValue the default value for don't care variables
     * @param variables    the set of variable which has to be contained in the model
     * @return an arbitrary model of this BDD
     */
    public Assignment model(final boolean defaultValue, final Variable... variables) {
        return model(defaultValue, Arrays.asList(variables));
    }

    /**
     * Returns a full model of this BDD or {@code null} if there is none.
     * @return a full model of this BDD
     */
    public Assignment fullModel() {
        return createAssignment(this.operations.fullSatOne(this.index));
    }

    /**
     * Returns the number of paths leading to the terminal 'one' node.
     * @return the number of paths leading to the terminal 'one' node
     */
    public BigInteger pathCountOne() {
        return this.operations.pathCountOne(this.index);
    }

    /**
     * Returns the number of paths leading to the terminal 'zero' node.
     * @return the number of paths leading to the terminal 'zero' node
     */
    public BigInteger pathCountZero() {
        return this.operations.pathCountZero(this.index);
    }

    /**
     * Returns all the variables this BDD depends on.
     * @return all the variables that this BDD depends on
     */
    public SortedSet<Variable> support() {
        final int supportBDD = this.operations.support(this.index);
        final Assignment assignment = createAssignment(supportBDD);
        assert assignment == null || assignment.negativeLiterals().isEmpty();
        return assignment == null ? Collections.emptySortedSet() : new TreeSet<>(assignment.positiveVariables());
    }

    /**
     * Returns the number of distinct nodes for this BDD.
     * @return the number of distinct nodes
     */
    public int nodeCount() {
        return this.operations.nodeCount(this.index);
    }

    /**
     * Returns how often each variable occurs in this BDD.
     * @return how often each variable occurs in the BDD
     */
    public SortedMap<Variable, Integer> variableProfile() {
        final int[] varProfile = this.operations.varProfile(this.index);
        final SortedMap<Variable, Integer> profile = new TreeMap<>();
        for (int i = 0; i < varProfile.length; i++) {
            profile.put(this.kernel.getVariableForIndex(i), varProfile[i]);
        }
        return profile;
    }

    /**
     * Returns the variable order of this BDD.
     * @return the variable order
     */
    public List<Variable> getVariableOrder() {
        final List<Variable> order = new ArrayList<>();
        for (final int i : this.kernel.getCurrentVarOrder()) {
            order.add(this.kernel.getVariableForIndex(i));
        }
        return order;
    }

    /**
     * Swaps two variables in a BDD.
     * Beware that if the {@link #kernel BDDKernel} of this BDD was used for multiple
     * BDDs, the variables are swapped in <b>all</b> of these BDDs.
     * @param first  the first variable to swap
     * @param second the second variable to swap
     * @deprecated dangerous API, will be removed in version 3.0, use {@link BDDKernel#swapVariables} instead
     */
    @Deprecated
    public void swapVariables(final Variable first, final Variable second) {
        this.kernel.swapVariables(first, second);
    }

    /**
     * Returns the reordering object for the BDD kernel.
     * @return the reordering object
     * @deprecated the relevant methods should now be access via the {@link #underlyingKernel() kernel}:
     *         <ul>
     *             <li>Add a variable block: {@link BDDKernel#addVariableBlock}</li>
     *             <li>Add blocks for all variables: {@link BDDKernel#addAllVariablesAsBlock}</li>
     *             <li>Instant reordering: {@link BDDKernel#reorder}</li>
     *         </ul>
     */
    @Deprecated
    public BDDReordering getReordering() {
        return this.kernel.getReordering();
    }

    /**
     * Returns a LogicNG internal BDD data structure of this BDD.
     * @return the BDD as LogicNG data structure
     */
    public BDDNode toLngBdd() {
        return this.apply(LngBDDFunction.get());
    }

    /**
     * Creates an assignment from a BDD.
     * @param modelBDD the BDD
     * @return the assignment
     * @throws IllegalStateException if the BDD does not represent a unique model
     */
    private Assignment createAssignment(final int modelBDD) {
        if (modelBDD == BDDKernel.BDD_FALSE) {
            return null;
        }
        if (modelBDD == BDDKernel.BDD_TRUE) {
            return new Assignment();
        }
        final List<int[]> nodes = this.operations.allNodes(modelBDD);
        final Assignment assignment = new Assignment();
        for (final int[] node : nodes) {
            final Variable variable = this.kernel.getVariableForIndex(node[1]);
            if (node[2] == BDDKernel.BDD_FALSE) {
                assignment.addLiteral(variable);
            } else if (node[3] == BDDKernel.BDD_FALSE) {
                assignment.addLiteral(variable.negate());
            } else {
                throw new IllegalStateException("Expected that the model BDD has one unique path through the BDD.");
            }
        }
        return assignment;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.index, this.kernel);
    }

    @Override
    public boolean equals(final Object other) {
        return this == other || other instanceof BDD
                && this.index == ((BDD) other).index
                && Objects.equals(this.kernel, ((BDD) other).kernel);
    }

    @Override
    public String toString() {
        return "BDD{" + this.index + "}";
    }
}
