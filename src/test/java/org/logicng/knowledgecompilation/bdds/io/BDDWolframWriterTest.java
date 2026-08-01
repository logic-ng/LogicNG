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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.knowledgecompilation.bdds.BDD;

/**
 * Unit tests for {@link BDDWolframWriter}.
 * 
 * @version 2.6.2
 * @since 2.6.2
 */
public class BDDWolframWriterTest { 

	@Test
	public void testTrivialTrue() {
		final FormulaFactory f = new FormulaFactory();
		assertThat(BDDWolframWriter.write(f.verum().bdd())).containsExactly(0);
	}

	@Test
	public void testTrivialFalse() {
		final FormulaFactory f = new FormulaFactory();
		assertThat(BDDWolframWriter.write(f.falsum().bdd())).containsExactly(0);
	}

	@Test
	public void testSingleVariable() {
		final FormulaFactory f = new FormulaFactory();
		assertThat(BDDWolframWriter.write(f.variable("#1").bdd())).containsExactly(1, 0, 1, -1);
	}

	@Test
	public void testNegatedVariable() {
		final FormulaFactory f = new FormulaFactory();
		assertThat(BDDWolframWriter.write(f.literal("#1", false).bdd())).containsExactly(-1, 0, 1, -1);
	}

	@Test
	public void testConjunction() {
		final FormulaFactory f = new FormulaFactory();
		final BDD bdd = f.and(f.variable("#1"), f.variable("#2")).bdd();
		assertThat(BDDWolframWriter.write(bdd)).containsExactly(2, 0, 2, -1, 1, 1, -1);
	}

	@Test
	public void testThreeVariableFunction() {
		final FormulaFactory f = new FormulaFactory();
		final BDD bdd = f.and(f.literal("#1", false), f.variable("#2"), f.variable("#3")).bdd();
		assertThat(BDDWolframWriter.write(bdd)).containsExactly(-3, 0, 1, -2, 1, 3, -1, 2, 1, -1);
	}
}
