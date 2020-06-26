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

import static org.logicng.formulas.cache.TransformationCacheEntry.NNF;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.SortedSet;
import java.util.stream.Stream;

/**
 * Super class for Boolean n-ary operators.
 * @version 2.0.0
 * @since 1.0
 */
public abstract class NAryOperator extends Formula {

    protected final Formula[] operands;
    private volatile int hashCode;

    /**
     * Constructor.
     * @param type     the operator's type
     * @param operands the list of operands
     * @param f        the factory which created this instance
     */
    NAryOperator(final FType type, final Collection<? extends Formula> operands, final FormulaFactory f) {
        super(type, f);
        this.operands = operands.toArray(new Formula[0]);
        this.hashCode = 0;
    }

    @Override
    public long numberOfAtoms() {
        if (this.numberOfAtoms != -1) {
            return this.numberOfAtoms;
        }
        this.numberOfAtoms = 0;
        for (final Formula f : this.operands) {
            this.numberOfAtoms += f.numberOfAtoms();
        }
        return this.numberOfAtoms;
    }

    @Override
    public long numberOfNodes() {
        if (this.numberOfNodes != -1) {
            return this.numberOfNodes;
        }
        this.numberOfNodes = 1;
        for (final Formula f : this.operands) {
            this.numberOfNodes += f.numberOfNodes();
        }
        return this.numberOfNodes;
    }

    @Override
    public int numberOfOperands() {
        return this.operands.length;
    }

    @Override
    public boolean isConstantFormula() {
        return false;
    }

    @Override
    public boolean isAtomicFormula() {
        return false;
    }

    @Override
    public SortedSet<Variable> variables() {
        if (this.variables == null) {
            this.variables = Collections.unmodifiableSortedSet(FormulaHelper.variables(this.operands));
        }
        return this.variables;
    }

    @Override
    public SortedSet<Literal> literals() {
        return Collections.unmodifiableSortedSet(FormulaHelper.literals(this.operands));
    }

    @Override
    public boolean containsVariable(final Variable variable) {
        for (final Formula op : this.operands) {
            if (op.containsVariable(variable)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Formula restrict(final Assignment assignment) {
        final LinkedHashSet<Formula> nops = new LinkedHashSet<>();
        for (final Formula op : this.operands) {
            nops.add(op.restrict(assignment));
        }
        return this.f.naryOperator(this.type, nops);
    }

    @Override
    public boolean containsNode(final Formula formula) {
        if (this.equals(formula)) {
            return true;
        }
        if (this.type != formula.type) {
            for (final Formula op : this.operands) {
                if (op.containsNode(formula)) {
                    return true;
                }
            }
            return false;
        }
        final List<Formula> fOps = new ArrayList<>(formula.numberOfOperands());
        for (final Formula op : formula) {
            fOps.add(op);
        }
        for (final Formula op : this.operands) {
            fOps.remove(op);
            if (op.containsNode(formula)) {
                return true;
            }
        }
        return fOps.isEmpty();
    }

    @Override
    public Formula substitute(final Substitution substitution) {
        final LinkedHashSet<Formula> nops = new LinkedHashSet<>();
        for (final Formula op : this.operands) {
            nops.add(op.substitute(substitution));
        }
        return this.f.naryOperator(this.type, nops);
    }

    @Override
    public Formula negate() {
        return this.f.not(this);
    }

    @Override
    public Formula nnf() {
        Formula nnf = this.transformationCache.get(NNF);
        if (nnf == null) {
            final LinkedHashSet<Formula> nops = new LinkedHashSet<>();
            for (final Formula op : this.operands) {
                nops.add(op.nnf());
            }
            nnf = this.f.naryOperator(this.type, nops);
            this.transformationCache.put(NNF, nnf);
        }
        return nnf;
    }

    /**
     * Helper method for generating the hashcode.
     * @param shift shift value
     * @return hashcode
     */
    protected int hashCode(final int shift) {
        if (this.hashCode == 0) {
            int temp = 1;
            for (final Formula formula : this.operands) {
                temp += formula.hashCode();
            }
            temp *= shift;
            this.hashCode = temp;
        }
        return this.hashCode;
    }

    protected boolean compareOperands(final Formula[] other) {
        if (this.operands.length != other.length) {
            return false;
        }
        for (final Formula op1 : this.operands) {
            boolean found = false;
            for (final Formula op2 : other) {
                if (op1.equals(op2)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    @Override
    public Iterator<Formula> iterator() {
        return new Iterator<Formula>() {
            private int i;

            @Override
            public boolean hasNext() {
                return this.i < NAryOperator.this.operands.length;
            }

            @Override
            public Formula next() {
                if (this.i == NAryOperator.this.operands.length) {
                    throw new NoSuchElementException();
                }
                return NAryOperator.this.operands[this.i++];
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    @Override
    public Stream<Formula> stream() {
        return Stream.of(this.operands);
    }
}
