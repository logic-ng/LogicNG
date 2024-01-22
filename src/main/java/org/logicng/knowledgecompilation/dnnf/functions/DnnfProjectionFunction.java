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
import org.logicng.util.CollectionHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A DNNF function which projects the DNNF to a given set of variables.
 * <p>
 * Not the resulting DNNF does <b>NOT</b> have deterministic property, and
 * thus, it must not be used for model counting or other DNNF operations
 * requiring a d-DNNF.
 * @version 2.5.0
 * @since 2.5.0
 */
public final class DnnfProjectionFunction implements DnnfFunction<Dnnf> {

    private final SortedSet<Variable> variables;

    /**
     * Creates a new DNNF function to project a DNNF to the given set of
     * variables.
     * @param variables the variables to project to
     */
    public DnnfProjectionFunction(final Collection<Variable> variables) {
        this.variables = new TreeSet<>(variables);
    }

    @Override
    public Dnnf apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        return new Dnnf(CollectionHelper.intersection(this.variables, originalVariables, TreeSet::new), applyRec(formula, new HashMap<>()));
    }

    private Formula applyRec(final Formula formula, final Map<Formula, Formula> cache) {
        final Formula cached = cache.get(formula);
        if (cached != null) {
            return cached;
        }
        final Formula result;
        switch (formula.type()) {
            case TRUE:
            case FALSE:
                result = formula;
                break;
            case LITERAL:
                result = this.variables.contains(((Literal) formula).variable()) ? formula : formula.factory().verum();
                break;
            case OR:
                final List<Formula> newOrOps = new ArrayList<>();
                for (final Formula op : formula) {
                    newOrOps.add(applyRec(op, cache));
                }
                result = formula.factory().or(newOrOps);
                break;
            case AND:
                final List<Formula> newAndOps = new ArrayList<>();
                for (final Formula op : formula) {
                    newAndOps.add(applyRec(op, cache));
                }
                result = formula.factory().and(newAndOps);
                break;
            default:
                throw new IllegalArgumentException("Unexpected formula type: " + formula.type());
        }
        cache.put(formula, result);
        return result;
    }
}
