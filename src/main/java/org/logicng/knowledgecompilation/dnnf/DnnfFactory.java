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
        this.backboneSimplifier = new BackboneSimplifier();
        this.subsumption = new CNFSubsumption();
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
