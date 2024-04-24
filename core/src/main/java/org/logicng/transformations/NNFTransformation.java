package org.logicng.transformations;

import static org.logicng.formulas.FType.dual;
import static org.logicng.formulas.cache.TransformationCacheEntry.NNF;

import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Transformation of a formula in NNF.
 * @version 2.2.0
 * @since 2.2.0
 */
public class NNFTransformation implements FormulaTransformation {

    private static final NNFTransformation INSTANCE = new NNFTransformation();

    /**
     * Private constructor.
     */
    private NNFTransformation() {
        // Intentionally left empty.
    }

    /**
     * Returns the singleton of this transformation.
     * @return the transformation instance
     */
    public static NNFTransformation get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return applyRec(formula, true);
    }

    private Formula applyRec(final Formula formula, final boolean polarity) {
        final FormulaFactory f = formula.factory();
        Formula nnf;
        if (polarity) {
            nnf = formula.transformationCacheEntry(NNF);
            if (nnf != null) {
                return nnf;
            }
        }
        final FType type = formula.type();
        switch (type) {
            case TRUE:
            case FALSE:
            case LITERAL:
                nnf = polarity ? formula : formula.negate();
                break;
            case NOT:
                nnf = applyRec(((Not) formula).operand(), !polarity);
                break;
            case OR:
            case AND:
                nnf = applyRec(formula.iterator(), formula.type(), polarity, f);
                break;
            case EQUIV:
                final Equivalence equiv = (Equivalence) formula;
                if (polarity) {
                    nnf = f.and(f.or(applyRec(equiv.left(), false), applyRec(equiv.right(), true)),
                            f.or(applyRec(equiv.left(), true), applyRec(equiv.right(), false)));
                } else {
                    nnf = f.and(f.or(applyRec(equiv.left(), false), applyRec(equiv.right(), false)),
                            f.or(applyRec(equiv.left(), true), applyRec(equiv.right(), true)));
                }
                break;
            case IMPL:
                final Implication impl = (Implication) formula;
                if (polarity) {
                    nnf = f.or(applyRec(impl.left(), false), applyRec(impl.right(), true));
                } else {
                    nnf = f.and(applyRec(impl.left(), true), applyRec(impl.right(), false));
                }
                break;
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                if (polarity) {
                    final List<Formula> encoding = pbc.getEncoding();
                    nnf = applyRec(encoding.iterator(), FType.AND, true, f);
                } else {
                    nnf = applyRec(pbc.negate(), true);
                }
                break;
            default:
                throw new IllegalStateException("Unknown formula type = " + type);
        }
        if (polarity) {
            formula.setTransformationCacheEntry(NNF, nnf);
        }
        return nnf;
    }

    private Formula applyRec(final Iterator<Formula> formulas, final FType type, final boolean polarity, final FormulaFactory f) {
        final LinkedHashSet<Formula> nops = new LinkedHashSet<>();
        while (formulas.hasNext()) {
            final Formula formula = formulas.next();
            nops.add(applyRec(formula, polarity));
        }
        return f.naryOperator(polarity ? type : dual(type), nops);
    }
}
