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

package org.logicng.formulas.printer;

import org.logicng.formulas.CType;

/**
 * The default string representation for formulas.
 * @version 1.0
 * @since 1.0
 */
public class DefaultStringRepresentation extends FormulaStringRepresentation {

    @Override
    protected String falsum() {
        return "$false";
    }

    @Override
    protected String verum() {
        return "$true";
    }

    @Override
    protected String negation() {
        return "~";
    }

    @Override
    protected String implication() {
        return " => ";
    }

    @Override
    protected String equivalence() {
        return " <=> ";
    }

    @Override
    protected String and() {
        return " & ";
    }

    @Override
    protected String or() {
        return " | ";
    }

    @Override
    protected String pbComparator(final CType comparator) {
        switch (comparator) {
            case EQ:
                return " = ";
            case LE:
                return " <= ";
            case LT:
                return " < ";
            case GE:
                return " >= ";
            case GT:
                return " > ";
            default:
                throw new IllegalArgumentException("Unknown pseudo-Boolean comparison: " + comparator);
        }
    }

    @Override
    protected String pbMul() {
        return "*";
    }

    @Override
    protected String pbAdd() {
        return " + ";
    }

    @Override
    protected String lbr() {
        return "(";
    }

    @Override
    protected String rbr() {
        return ")";
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
