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

package org.logicng.predicates;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Not;

/**
 * Predicate to test if a formula contains any subformula that is a pseudo-Boolean constraint.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class ContainsPBCPredicate implements FormulaPredicate {

    private final static ContainsPBCPredicate INSTANCE = new ContainsPBCPredicate();

    /**
     * Private empty constructor.  Singleton class.
     */
    private ContainsPBCPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static ContainsPBCPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return false;
            case AND:
            case OR:
                for (final Formula op : formula) {
                    if (test(op, cache)) {
                        return true;
                    }
                }
                return false;
            case NOT:
                return test(((Not) formula).operand(), cache);
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                return test(binary.left(), cache) || test(binary.right(), cache);
            case PBC:
                return true;
            default:
                throw new IllegalArgumentException("Unknown formula type " + formula.type());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
