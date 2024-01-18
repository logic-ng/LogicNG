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

package org.logicng.knowledgecompilation.dnnf.functions;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A DNNF function which counts models.
 * @version 2.5.0
 * @since 2.5.0
 */
public final class DnnfProjectionFunction implements DnnfFunction<Dnnf> {

    private final SortedSet<Variable> variables;

    public DnnfProjectionFunction(final Collection<Variable> variables) {
        this.variables = new TreeSet<>(variables);
    }

    @Override
    public Dnnf apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        return new Dnnf(originalVariables, applyRec(formula));
    }

    private Formula applyRec(final Formula formula) {
        switch (formula.type()) {
            case TRUE:
            case FALSE:
                return formula;
            case LITERAL:
                return this.variables.contains(((Literal) formula).variable()) ? formula : formula.factory().verum();
            case OR:
                final List<Formula> newOrOps = new ArrayList<>();
                for (final Formula op : formula) {
                    newOrOps.add(applyRec(op));
                }
                return formula.factory().or(newOrOps);
            case AND:
                final List<Formula> newAndOps = new ArrayList<>();
                for (final Formula op : formula) {
                    newAndOps.add(applyRec(op));
                }
                return formula.factory().and(newAndOps);
            default:
                throw new IllegalArgumentException("Unexpected formula type: " + formula.type());
        }
    }
}
