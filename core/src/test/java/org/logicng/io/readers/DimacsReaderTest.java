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

package org.logicng.io.readers;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;

import java.io.File;

/**
 * Unit Tests for the class {@link DimacsReader}.
 * @version 2.0.0
 * @since 1.0
 */
public class DimacsReaderTest {

    @Test
    public void testExceptionalBehavior() {
        assertThatThrownBy(() -> {
            final FormulaFactory f = new FormulaFactory();
            final File file = new File("src/test/resources/dimacs/malformed/contains-line-without-zero.cnf");
            DimacsReader.readCNF(file, f, "v");
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Line '2 -3' did not end with 0.");
    }
}
