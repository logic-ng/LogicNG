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

package org.logicng.knowledgecompilation.bdds.io;

import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

/**
 * A writer which generates a flat integer array representation of a BDD, matching the internal BDD
 * representation of the Wolfram language (e.g. {@code BooleanFunction["BDD" -> {...}]}).
 * @version 2.6.2
 * @since 2.6.2
 */ 
public final class BDDWolframWriter {

    private BDDWolframWriter() {
        // Intentionally left empty.
    }

    /**
     * Generates a flat integer array representation of the given BDD. The format encodes a canonical
     * directed acyclic graph with complemented edges, matching the internal BDD representation of the
     * Wolfram language.
     * @param bdd the BDD which should be written
     * @return the flat integer array representation of the BDD
     */
    public static int[] write(final BDD bdd) {
        final BDDKernel kernel = bdd.underlyingKernel();
        final BDDOperations operations = new BDDOperations(kernel);
        return operations.toArrayRepresentation(bdd.index(), bdd.getVariableOrder().size());
    }
}
