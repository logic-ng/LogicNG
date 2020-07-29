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

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;

/**
 * Unit tests for {@link DTreeNode}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DTreeNodeTest extends TestWithExampleFormulas {

    @Test
    public void testToString() throws ParserException {
        final DTreeNode node = new DTreeNode(new DTreeLeaf(1, this.f.parse("a | b")), new DTreeLeaf(2, this.f.parse("c | d")));
        assertThat(node.toString()).isEqualTo("DTreeNode: [DTreeLeaf: 1, a | b, DTreeLeaf: 2, c | d]");
    }
}
