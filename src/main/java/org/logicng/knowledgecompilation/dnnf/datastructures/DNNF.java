package org.logicng.knowledgecompilation.dnnf.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.functions.DNNFFunction;

import java.util.SortedSet;

/**
 * A DNNF - Decomposable Negation Normal Form.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class DNNF {

    private final SortedSet<Variable> originalVariables;
    private final Formula formula;

    /**
     * Constructs a new DNNF.
     * @param originalVariables the set of original variables
     * @param dnnf              the formula of the DNNF
     */
    public DNNF(final SortedSet<Variable> originalVariables, final Formula dnnf) {
        this.originalVariables = originalVariables;
        this.formula = dnnf;
    }

    /**
     * Executes a given DNNF function on this DNNF.
     * @param function the function
     * @param <RESULT> the result type
     * @return the result of the function application
     */
    public <RESULT> RESULT execute(final DNNFFunction<RESULT> function) {
        return function.apply(this.originalVariables, this.formula);
    }

    /**
     * Returns the formula of the DNNF.
     * @return the formula
     */
    public Formula formula() {
        return this.formula;
    }

    /**
     * Returns the original variables of the formula
     * @return the original variables
     */
    public SortedSet<Variable> getOriginalVariables() {
        return this.originalVariables;
    }
}
