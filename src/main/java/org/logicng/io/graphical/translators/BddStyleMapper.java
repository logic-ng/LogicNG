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

package org.logicng.io.graphical.translators;

import org.logicng.formulas.Variable;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDConstruction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

public abstract class BddStyleMapper implements StyleMapper<Integer> {

    private final BDDKernel kernel;
    private final BDDConstruction bddConstruction;

    public BddStyleMapper(final BDDKernel kernel) {
        this.kernel = kernel;
        this.bddConstruction = new BDDConstruction(kernel);
    }

    @Override
    public abstract GraphicalNodeStyle computeStyle(final Integer index);

    protected Variable variable(final int index) {
        return this.kernel.getVariableForIndex(this.bddConstruction.bddVar(index));
    }

    protected boolean isFalse(final int index) {
        return index == BDDKernel.BDD_FALSE;
    }

    protected boolean isTrue(final int index) {
        return index == BDDKernel.BDD_TRUE;
    }
}
