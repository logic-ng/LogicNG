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

package org.logicng.configurations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for the package configurations.
 * @version 2.0.0
 * @since 1.1
 */
public class ConfigurationsTest {

    @Test
    public void testValueOf() {
        assertThat(ConfigurationType.valueOf("CNF")).isEqualTo(ConfigurationType.CNF);
        assertThat(ConfigurationType.valueOf("GLUCOSE")).isEqualTo(ConfigurationType.GLUCOSE);
        assertThat(ConfigurationType.valueOf("MAXSAT")).isEqualTo(ConfigurationType.MAXSAT);
        assertThat(ConfigurationType.valueOf("CC_ENCODER")).isEqualTo(ConfigurationType.CC_ENCODER);
    }
}
