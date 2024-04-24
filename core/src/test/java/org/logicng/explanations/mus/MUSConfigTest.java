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

package org.logicng.explanations.mus;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.transformations.cnf.CNFConfig;

import java.util.Arrays;

/**
 * Unit tests for the class {@link MUSConfig}.
 * @version 2.0.0
 * @since 1.1
 */
public class MUSConfigTest {

    @Test
    public void testMUSConfiguration() {
        final MUSConfig config = MUSConfig.builder().algorithm(MUSConfig.Algorithm.valueOf("DELETION")).build();
        assertThat(config.toString()).isEqualTo(String.format("MUSConfig{%nalgorithm=DELETION%n}%n"));
        assertThat(Arrays.asList(CNFConfig.Algorithm.values())).contains(CNFConfig.Algorithm.valueOf("TSEITIN"));
    }
}
