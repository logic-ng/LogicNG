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

package org.logicng.transformations;

import static org.logicng.formulas.cache.TransformationCacheEntry.ANONYMIZATION;

import org.logicng.datastructures.Substitution;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Variable;

/**
 * An anonymizer replaces all variables in a formula with new variables generated from a given prefix and a counter.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class Anonymizer implements FormulaTransformation {

    private final Substitution substitution;
    private final String prefix;
    private int counter;

    /**
     * Constructs a new anonymizer with a given prefix for the newly introduced variables.
     * @param prefix       the prefix for the new variables
     * @param startCounter where should the counter start
     */
    public Anonymizer(final String prefix, final int startCounter) {
        this.prefix = prefix;
        this.substitution = new Substitution();
        this.counter = startCounter;
    }

    /**
     * Constructs a new anonymizer with a given prefix for the newly introduced variables.
     * @param prefix the prefix for the new variables
     */
    public Anonymizer(final String prefix) {
        this(prefix, 0);
    }

    /**
     * Constructs a new anonymizer with the standard variable prefix 'v'.
     */
    public Anonymizer() {
        this("v");
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (formula.variables().isEmpty()) {
            return formula;
        }
        final Formula cached = formula.transformationCacheEntry(ANONYMIZATION);
        if (cache && cached != null) {
            return cached;
        }
        for (final Variable variable : formula.variables()) {
            if (this.substitution.getSubstitution(variable) == null) {
                this.substitution.addMapping(variable, formula.factory().variable(this.prefix + this.counter++));
            }
        }
        final Formula transformed = formula.substitute(this.substitution);
        if (cache) {
            formula.setTransformationCacheEntry(ANONYMIZATION, transformed);
        }
        return transformed;
    }
}
