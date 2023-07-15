// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
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
public class DnnfFactory {

    protected final BackboneSimplifier backboneSimplifier;
    protected final CNFSubsumption subsumption;

    /**
     * Constructs a new DNNF factory instance.
     */
    public DnnfFactory() {
        this.backboneSimplifier = BackboneSimplifier.get();
        this.subsumption = CNFSubsumption.get();
    }

    /**
     * Compiles the given formula to a DNNF instance.
     * @param formula the formula
     * @return the compiled DNNF
     */
    public Dnnf compile(final Formula formula) {
        final SortedSet<Variable> originalVariables = new TreeSet<>(formula.variables());
        final Formula cnf = formula.cnf();
        originalVariables.addAll(cnf.variables());
        final Formula simplifedFormula = simplifyFormula(cnf);
        final DnnfCompiler compiler = new DnnfCompiler(simplifedFormula);
        final Formula dnnf = compiler.compile(new MinFillDTreeGenerator());
        return new Dnnf(originalVariables, dnnf);
    }

    protected Formula simplifyFormula(final Formula formula) {
        return formula.transform(this.backboneSimplifier).transform(this.subsumption);
    }
}
