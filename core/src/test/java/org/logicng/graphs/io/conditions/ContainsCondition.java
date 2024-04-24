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

package org.logicng.graphs.io.conditions;

import org.assertj.core.api.Condition;

import java.util.List;

/**
 * A condition needed for AssertJ-Assertions for {@link org.logicng.graphs.io.GraphDotFileWriterTest} and {@link org.logicng.graphs.io.GraphDimacsFileWriterTest}.
 * @version 1.2
 * @since 1.2
 */
public class ContainsCondition extends Condition<List<? extends String>> {

    private final String element;

    public ContainsCondition(String element) {
        this.element = element;
    }

    @Override
    public boolean matches(List<? extends String> strings) {
        return strings.contains(element);
    }

}
