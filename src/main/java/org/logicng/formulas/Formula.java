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

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.CacheEntry;
import org.logicng.functions.LiteralsFunction;
import org.logicng.functions.NumberOfAtomsFunction;
import org.logicng.functions.NumberOfNodesFunction;
import org.logicng.functions.VariablesFunction;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrdering;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrderingProvider;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.DNFPredicate;
import org.logicng.predicates.NNFPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.transformations.NNFTransformation;

import java.util.SortedSet;
import java.util.stream.Stream;

/**
 * Super class for formulas.
 * @version 2.3.0
 * @since 1.0
 */
public abstract class Formula implements Iterable<Formula> {

    protected final FType type;
    protected final FormulaFactory f;

    /**
     * Constructs a new formula.
     * @param type the type of the formula
     * @param f    the factory which created this formula
     */
    protected Formula(final FType type, final FormulaFactory f) {
        this.type = type;
        this.f = f;
    }

    /**
     * Returns the type of this formula.
     * @return the type of this formula
     */
    public FType type() {
        return this.type;
    }

    /**
     * Returns the factory of this formula.
     * @return the factory of this formula
     */
    public FormulaFactory factory() {
        return this.f;
    }

    /**
     * Returns the number of atomic formulas of this formula.  An atomic formula is a predicate (constants and literals)
     * or a pseudo-Boolean constraint.
     * @return the number of atomic formulas of this formula.
     */
    public long numberOfAtoms() {
        return NumberOfAtomsFunction.get().apply(this, true);
    }

    /**
     * Returns the number of nodes of this formula.
     * @return the number of nodes of this formula.
     */
    public long numberOfNodes() {
        return NumberOfNodesFunction.get().apply(this, true);
    }

    /**
     * Returns the number of operands of this formula.
     * @return the number of operands of this formula
     */
    public abstract int numberOfOperands();

    /**
     * Returns the number of internal nodes of this formula.
     * @return the number of internal nodes of this formula.
     */
    public long numberOfInternalNodes() {
        return this.f.numberOfNodes(this);
    }

    /**
     * Returns whether this formula is a constant formula ("True" or "False").
     * @return {@code true} if this formula is a constant formula, {@code false} otherwise
     */
    public abstract boolean isConstantFormula();

    /**
     * Returns whether this formula is an atomic formula (constant, literal, pseudo Boolean constraint), or not.
     * @return {@code true} if this formula is an atomic formula, {@code false} otherwise
     */
    public abstract boolean isAtomicFormula();

    /**
     * Returns all variables occurring in this formula.  Returns an unmodifiable set, so do not try to change the variable
     * set manually.
     * @return all variables occurring in this formula
     */
    public SortedSet<Variable> variables() {
        return VariablesFunction.get().apply(this, true);
    }

    /**
     * Returns all literals occurring in this formula.  Returns an unmodifiable set, so do not try to change the literal
     * set manually.
     * @return all literals occurring in this formula
     */
    public SortedSet<Literal> literals() {
        return LiteralsFunction.get().apply(this, true);
    }

    /**
     * Returns {@code true} if a given variable name is found in this formula, {@code false} otherwise.
     * @param variable the variable to search for
     * @return {@code true} if a given variable is found in this formula
     */
    public boolean containsVariable(final String variable) {
        return this.containsVariable(this.f.variable(variable));
    }

    /**
     * Returns {@code true} if a given variable name is found in this formula, {@code false} otherwise.
     * @param variable the variable to search for
     * @return {@code true} if a given variable is found in this formula
     */
    public abstract boolean containsVariable(final Variable variable);

    /**
     * Evaluates this formula with a given assignment.  A literal not covered by the assignment evaluates
     * to {@code false} if it is positive, otherwise it evaluates to {@code true}.
     * @param assignment the given assignment
     * @return the result of the evaluation, {@code true} or {@code false}
     */
    public abstract boolean evaluate(final Assignment assignment);

    /**
     * Restricts this formula with a given assignment.
     * @param assignment the given assignment
     * @return a new restricted formula
     */
    public abstract Formula restrict(final Assignment assignment);

    /**
     * Returns {@code true} if this formula contains a given node, {@code false} otherwise.
     * <p>
     * In particular, a {@code Literal} node {@code ~a} does NOT contain the node {@code a}.
     * @param formula the node
     * @return {@code true} if this formula contains a given node
     */
    public abstract boolean containsNode(final Formula formula);

    /**
     * Returns {@code true} if this formula is in NNF, otherwise {@code false}
     * @return {@code true} if this formula is in NNF, otherwise {@code false}
     * @see NNFPredicate the NNF predicate
     */
    public boolean isNNF() {
        return this.holds(NNFPredicate.get());
    }

    /**
     * Returns {@code true} if this formula is in DNF, otherwise {@code false}
     * @return {@code true} if this formula is in DNF, otherwise {@code false}
     * @see DNFPredicate the DNF predicate
     */
    public boolean isDNF() {
        return this.holds(DNFPredicate.get());
    }

    /**
     * Returns {@code true} if this formula is in CNF, otherwise {@code false}
     * @return {@code true} if this formula is in CNF, otherwise {@code false}
     * @see CNFPredicate the CNF predicate
     */
    public boolean isCNF() {
        return this.holds(CNFPredicate.get());
    }

    /**
     * Performs a simultaneous substitution on this formula given a single mapping from variable to formula.
     * @param variable the variable
     * @param formula  the formula
     * @return a new substituted formula
     */
    public Formula substitute(final Variable variable, final Formula formula) {
        final Substitution subst = new Substitution();
        subst.addMapping(variable, formula);
        return this.substitute(subst);
    }

    /**
     * Performs a given substitution on this formula.
     * @param substitution the substitution
     * @return a new substituted formula
     */
    public abstract Formula substitute(final Substitution substitution);

    /**
     * Returns a negated copy of this formula.
     * @return a negated copy of this formula
     */
    public abstract Formula negate();

    /**
     * Returns a copy of this formula which is in NNF.
     * @return a copy of this formula which is in NNF
     */
    public Formula nnf() {
        return transform(NNFTransformation.get(), true);
    }

    /**
     * Returns a copy of this formula which is in CNF.  The algorithm which is used for the default CNF transformation
     * can be configured in the {@link FormulaFactory}.
     * <p>
     * Be aware that the default algorithm for the CNF transformation may result in a CNF containing additional auxiliary
     * variables with prefix {@value FormulaFactory#CNF_PREFIX}.  Also, the result may not be a semantically equivalent CNF
     * but an equisatisfiable CNF.
     * <p>
     * If the introduction of auxiliary variables is unwanted, you can choose one of the algorithms
     * {@link org.logicng.transformations.cnf.CNFConfig.Algorithm#FACTORIZATION} and
     * {@link org.logicng.transformations.cnf.CNFConfig.Algorithm#BDD}.  Both algorithms provide CNF conversions without
     * the introduction of auxiliary variables and the result is a semantically equivalent CNF.
     * <p>
     * Since CNF is the input for the SAT or MaxSAT solvers, it has a special treatment here.  For other conversions, use
     * the according formula functions.
     * @return a copy of this formula which is in CNF
     */
    public Formula cnf() {
        return this.f.cnfEncoder().encode(this);
    }

    /**
     * Returns whether this formula implies the given other formula, i.e. `this =&gt; other` is a tautology.
     * A new SAT solver is used to check this tautology.  If you want to have more influence on the solver
     * (e.g. which solver type or configuration) you must use the {@link TautologyPredicate} directly on the
     * implication.
     * @param other the formula which should be checked if it is implied by this formula
     * @return true when this formula implies the given other formula, false otherwise
     */
    public boolean implies(final Formula other) {
        return this.f.implication(this, other).holds(new TautologyPredicate(this.f));
    }

    /**
     * Returns whether this formula is implied by the given other formula, i.e. `other =&gt; this` is a tautology.
     * A new SAT solver is used to check this tautology.  If you want to have more influence on the solver
     * (e.g. which solver type or configuration) you must use the {@link TautologyPredicate} directly on the
     * implication.
     * @param other the formula which should be checked if it implies this formula
     * @return true when this formula is implied by the given other formula, false otherwise
     */
    public boolean isImpliedBy(final Formula other) {
        return this.f.implication(other, this).holds(new TautologyPredicate(this.f));
    }

    /**
     * Returns whether this formula is equivalent to the given other formula, i.e. `other &lt;=&gt; this` is a tautology.
     * A new SAT solver is used to check this tautology.  If you want to have more influence on the solver
     * (e.g. which solver type or configuration) you must use the {@link TautologyPredicate} directly on the
     * equivalence.
     * @param other the formula which should be checked if it is equivalent with this formula
     * @return true when this formula is equivalent to the given other formula, false otherwise
     */
    public boolean isEquivalentTo(final Formula other) {
        return this.f.equivalence(this, other).holds(new TautologyPredicate(this.f));
    }

    /**
     * Generates a BDD from this formula with a given variable ordering.  This is done by generating a new BDD factory,
     * generating the variable order for this formula, and building a new BDD.  If more sophisticated operations should
     * be performed on the BDD or more than one formula should be constructed on the BDD, an own instance of
     * {@link BDDFactory} should be created and used.
     * @param variableOrdering the variable ordering
     * @return the BDD for this formula with the given ordering
     */
    public BDD bdd(final VariableOrdering variableOrdering) {
        final Formula formula = this.nnf();
        final int varNum = formula.variables().size();
        final BDDKernel kernel;
        if (variableOrdering == null) {
            kernel = new BDDKernel(this.f, varNum, varNum * 30, varNum * 20);
        } else {
            final VariableOrderingProvider provider = variableOrdering.provider();
            kernel = new BDDKernel(this.f, provider.getOrder(formula), varNum * 30, varNum * 20);
        }
        return BDDFactory.build(formula, kernel, null);
    }

    /**
     * Generates a BDD from this formula with no given variable ordering.  This is done by generating a new BDD factory
     * and building a new BDD.  If more sophisticated operations should be performed on the BDD or more than one
     * formula should be constructed on the BDD, an own instance of * {@link BDDFactory} should be created and used.
     * @return the BDD for this formula
     */
    public BDD bdd() {
        return bdd(null);
    }

    /**
     * Transforms this formula with a given formula transformator and caches the result.
     * @param transformation the formula transformator
     * @return the transformed formula
     */
    public Formula transform(final FormulaTransformation transformation) {
        return transformation.apply(this, true);
    }

    /**
     * Transforms this formula with a given formula transformator.
     * @param transformation the formula transformator
     * @param cache          indicates whether the result (and associated predicates) should be cached in this formula's cache.
     * @return the transformed formula
     */
    public Formula transform(final FormulaTransformation transformation, final boolean cache) {
        return transformation.apply(this, cache);
    }

    /**
     * Evaluates a given predicate on this formula, caches the result, and returns {@code true} if the predicate holds,
     * {@code false} otherwise.
     * @param predicate the predicate
     * @return {@code true} if the predicate holds, {@code false} otherwise
     */
    public boolean holds(final FormulaPredicate predicate) {
        return predicate.test(this, true);
    }

    /**
     * Evaluates a given predicate on this formula and returns {@code true} if the predicate holds, {@code false} otherwise.
     * @param predicate the predicate
     * @param cache     indicates whether the result should be cached in this formula's cache
     * @return {@code true} if the predicate holds, {@code false} otherwise
     */
    public boolean holds(final FormulaPredicate predicate, final boolean cache) {
        return predicate.test(this, cache);
    }

    /**
     * Applies a given function on this formula and returns the result.
     * @param function the function
     * @param cache    indicates whether the result should be cached in this formula's cache
     * @param <T>      the result type of the function
     * @return the result of the function application
     */
    public <T> T apply(final FormulaFunction<T> function, final boolean cache) {
        return function.apply(this, cache);
    }

    /**
     * Applies a given function on this formula, caches and returns the result.
     * @param function the function
     * @param <T>      the result type of the function
     * @return the result of the function application
     */
    public <T> T apply(final FormulaFunction<T> function) {
        return function.apply(this, true);
    }

    /**
     * Returns an entry of the transformation cache of this formula.
     * @param key the cache key
     * @return the cache value or {@code null} if the key is not found
     */
    public Formula transformationCacheEntry(final CacheEntry key) {
        return this.f.transformationCacheEntry(this, key);
    }

    /**
     * Sets an entry in the transformation cache of this formula
     * @param key   the cache key
     * @param value the cache value
     */
    public void setTransformationCacheEntry(final CacheEntry key, final Formula value) {
        this.f.setTransformationCacheEntry(this, key, value);
    }

    /**
     * Returns an entry of the predicate cache of this formula.
     * @param key the cache key
     * @return the cache value (which is {@code UNDEF} if nothing is present)
     */
    public Tristate predicateCacheEntry(final CacheEntry key) {
        return this.f.predicateCacheEntry(this, key);
    }

    /**
     * Sets an entry in the predicate cache of this formula
     * @param key   the cache key
     * @param value the cache value
     */
    public void setPredicateCacheEntry(final CacheEntry key, final boolean value) {
        this.f.setPredicateCacheEntry(this, key, value);
    }

    /**
     * Sets an entry in the predicate cache of this formula
     * @param key   the cache key
     * @param value the cache value
     */
    public void setPredicateCacheEntry(final CacheEntry key, final Tristate value) {
        this.f.setPredicateCacheEntry(this, key, value);
    }

    /**
     * Returns an entry of the function cache of this formula.
     * @param key the cache key
     * @return the cache value or {@code null} if the key is not found
     */
    public Object functionCacheEntry(final CacheEntry key) {
        return this.f.functionCacheEntry(this, key);
    }

    /**
     * Sets an entry in the function cache of this formula
     * @param key   the cache key
     * @param value the cache value
     */
    public void setFunctionCacheEntry(final CacheEntry key, final Object value) {
        this.f.setFunctionCacheEntry(this, key, value);
    }

    /**
     * Clears the transformation and function cache of the formula.
     */
    public void clearCaches() {
        this.f.clearCaches(this);
    }

    /**
     * Returns a stream of this formula's operands.
     * <p>
     * Most times streams have worse performance then iterating over the formula per iterator.
     * Since internally formulas store their operands, a costly call to {@code Arrays.stream()}
     * is necessary.  So if performance matters - avoid using streams.
     * @return the stream
     */
    public abstract Stream<Formula> stream();

    @Override
    public String toString() {
        return this.f.string(this);
    }
}
