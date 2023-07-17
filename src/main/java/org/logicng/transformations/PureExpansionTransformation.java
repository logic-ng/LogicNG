// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import static org.logicng.util.FormulaHelper.literalsAsVariables;

import org.logicng.cardinalityconstraints.CCAMOPure;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.List;

/**
 * Transformation of a formula to a formula with expanded at-most-one and
 * exactly-one cardinality constraints. Each subformula of the formula that is a
 * pseudo-Boolean constraint of type AMO or EXO gets replaced by a pure encoding
 * such that the resulting formula is equivalent and free of pseudo-Boolean
 * constraints.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class PureExpansionTransformation implements FormulaTransformation {

    private final CCAMOPure amoEncoder = new CCAMOPure();
    private static final PureExpansionTransformation INSTANCE = new PureExpansionTransformation();

    /**
     * Constructs a new transformation instance.
     * @deprecated In the next version, the standard constructor will be
     *             replaced by a private constructor. In order to instantiate an
     *             object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public PureExpansionTransformation() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static PureExpansionTransformation get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return formula;
            case NOT:
                final Not not = (Not) formula;
                return f.not(apply(not.operand(), cache));
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                final List<Formula> newOps = new ArrayList<>(nary.numberOfOperands());
                for (final Formula op : nary) {
                    newOps.add(apply(op, cache));
                }
                return f.naryOperator(formula.type(), newOps);
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                final Formula newLeft = apply(binary.left(), cache);
                final Formula newRight = apply(binary.right(), cache);
                return f.binaryOperator(formula.type(), newLeft, newRight);
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                if (pbc.isAmo() || pbc.isExo()) {
                    final EncodingResult encodingResult = EncodingResult.resultForFormula(f);
                    final Variable[] vars = literalsAsVariables(pbc.operands());
                    this.amoEncoder.build(encodingResult, vars);
                    final List<Formula> encoding = encodingResult.result();
                    if (pbc.isExo()) {
                        encoding.add(f.or(vars));
                    }
                    return f.and(encoding);
                } else {
                    throw new UnsupportedOperationException(
                            "Pure encoding for a PBC of type other than AMO or EXO is currently not supported.");
                }
            default:
                throw new IllegalStateException("Unknown formula type: " + formula.type());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
