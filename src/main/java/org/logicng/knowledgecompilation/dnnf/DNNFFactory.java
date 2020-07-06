package org.logicng.knowledgecompilation.dnnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.datastructures.DNNF;
import org.logicng.knowledgecompilation.dnnf.datastructures.dtree.MinFillDTreeGenerator;
import org.logicng.transformations.cnf.CNFSubsumption;
import org.logicng.transformations.simplification.BackboneSimplifier;

import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A DNNF factory that can be used to compute DNNFs from formulas.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class DNNFFactory {

    private final BackboneSimplifier backboneSimplifier;
    private final CNFSubsumption subsumption;

    /**
     * Constructs a new DNNF factory instance.
     */
    public DNNFFactory() {
        this.backboneSimplifier = new BackboneSimplifier();
        this.subsumption = new CNFSubsumption();
    }

    /**
     * Compiles the given formula to a DNNF instance.
     * @param formula the formula
     * @return the compiled DNNF
     */
    public DNNF compile(final Formula formula) {
        final SortedSet<Variable> originalVariables = new TreeSet<>(formula.variables());
        final Formula cnf = formula.cnf();
        originalVariables.addAll(cnf.variables());
        final Formula simplifedFormula = simplifyFormula(cnf);
        final DNNFCompiler compiler = new DNNFCompiler(simplifedFormula);
        final Formula dnnf = compiler.compile(new MinFillDTreeGenerator());
        return new DNNF(originalVariables, dnnf);
    }

    private Formula simplifyFormula(final Formula formula) {
        return formula.transform(this.backboneSimplifier).transform(this.subsumption);
    }
}
