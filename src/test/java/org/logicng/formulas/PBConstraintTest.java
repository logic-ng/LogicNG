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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.datastructures.Tristate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link PBConstraint}.
 * @version 2.0.0
 * @since 1.0
 */
public class PBConstraintTest extends TestWithExampleFormulas {

    private static final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
    private static final FormulaFactory f2 = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());

    private final PBConstraint pb1;
    private final PBConstraint pb2;
    private final PBConstraint pb22;
    private final CardinalityConstraint cc1;
    private final CardinalityConstraint cc2;
    private final CardinalityConstraint amo1;
    private final CardinalityConstraint amo2;
    private final CardinalityConstraint exo1;
    private final CardinalityConstraint exo2;

    public PBConstraintTest() {
        final Variable[] lits1 = new Variable[]{f.variable("a")};
        final List<Literal> lits2 = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Variable> litsCC2 = Arrays.asList(f.variable("a"), f2.variable("b"), f.variable("c"));
        final int[] coeffs1 = new int[]{3};
        final List<Integer> coeffs2 = Arrays.asList(3, -2, 7);
        this.pb1 = (PBConstraint) f.pbc(CType.LE, 2, lits1, coeffs1);
        this.pb2 = (PBConstraint) f.pbc(CType.LE, 8, lits2, coeffs2);
        this.pb22 = (PBConstraint) f2.pbc(CType.LE, 8, lits2, coeffs2);
        this.cc1 = (CardinalityConstraint) f.cc(CType.LT, 1, lits1);
        this.cc2 = (CardinalityConstraint) f.cc(CType.GE, 2, litsCC2);
        this.amo1 = (CardinalityConstraint) f.amo(lits1);
        this.amo2 = (CardinalityConstraint) f.amo(litsCC2);
        this.exo1 = (CardinalityConstraint) f.exo(lits1);
        this.exo2 = (CardinalityConstraint) f.exo(litsCC2);
    }

    @Test
    public void testIllegalPB() {
        final List<Literal> lits = Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Integer> coeffs = Arrays.asList(3, -2, 7, 2);
        assertThatThrownBy(() -> f.pbc(CType.EQ, 3, lits, coeffs)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testType() {
        assertThat(this.pb1.type()).isEqualTo(FType.PBC);
        assertThat(this.pb2.type()).isEqualTo(FType.PBC);
        assertThat(this.cc1.type()).isEqualTo(FType.PBC);
        assertThat(this.cc2.type()).isEqualTo(FType.PBC);
        assertThat(this.amo1.type()).isEqualTo(FType.PBC);
        assertThat(this.amo2.type()).isEqualTo(FType.PBC);
        assertThat(this.exo1.type()).isEqualTo(FType.PBC);
        assertThat(this.exo2.type()).isEqualTo(FType.PBC);
    }

    @Test
    public void testGetters() {
        final Literal[] lits1 = new Literal[]{f.variable("a")};
        final Literal[] lits2 = new Literal[]{f2.variable("a"), f.literal("b", false), f.variable("c")};
        final Literal[] litsCC2 = new Literal[]{f.variable("a"), f.variable("b"), f2.variable("c")};
        final int[] coeffs1 = new int[]{3};
        final int[] coeffs2 = new int[]{3, -2, 7};

        final int[] coeffsCC1 = new int[]{1};
        final int[] coeffsCC2 = new int[]{1, 1, 1};

        assertThat(this.pb1.operands()).containsExactly(lits1);
        assertThat(this.pb1.coefficients()).containsExactly(coeffs1);
        assertThat(this.pb1.comparator()).isEqualTo(CType.LE);
        assertThat(this.pb1.rhs()).isEqualTo(2);
        assertThat(this.pb1.isCC()).isFalse();
        assertThat(this.pb1.isAmo()).isFalse();
        assertThat(this.pb1.isExo()).isFalse();
        assertThat(this.pb1.maxWeight()).isEqualTo(3);

        assertThat(this.pb2.operands()).containsExactly(lits2);
        assertThat(this.pb2.coefficients()).containsExactly(coeffs2);
        assertThat(this.pb2.comparator()).isEqualTo(CType.LE);
        assertThat(this.pb2.rhs()).isEqualTo(8);
        assertThat(this.pb2.isCC()).isFalse();
        assertThat(this.pb2.isAmo()).isFalse();
        assertThat(this.pb2.isExo()).isFalse();
        assertThat(this.pb2.maxWeight()).isEqualTo(7);

        assertThat(this.cc1.operands()).containsExactly(lits1);
        assertThat(this.cc1.coefficients()).containsExactly(coeffsCC1);
        assertThat(this.cc1.comparator()).isEqualTo(CType.LT);
        assertThat(this.cc1.rhs()).isEqualTo(1);
        assertThat(this.cc1.isCC()).isTrue();
        assertThat(this.cc1.isAmo()).isFalse();
        assertThat(this.cc1.isExo()).isFalse();
        assertThat(this.cc1.maxWeight()).isEqualTo(1);

        assertThat(this.cc2.operands()).containsExactly(litsCC2);
        assertThat(this.cc2.coefficients()).containsExactly(coeffsCC2);
        assertThat(this.cc2.comparator()).isEqualTo(CType.GE);
        assertThat(this.cc2.rhs()).isEqualTo(2);
        assertThat(this.cc2.isCC()).isTrue();
        assertThat(this.cc2.isAmo()).isFalse();
        assertThat(this.cc2.isExo()).isFalse();
        assertThat(this.cc2.maxWeight()).isEqualTo(1);

        assertThat(this.amo1.operands()).containsExactly(lits1);
        assertThat(this.amo1.coefficients()).containsExactly(coeffsCC1);
        assertThat(this.amo1.comparator()).isEqualTo(CType.LE);
        assertThat(this.amo1.rhs()).isEqualTo(1);
        assertThat(this.amo1.isCC()).isTrue();
        assertThat(this.amo1.isAmo()).isTrue();
        assertThat(this.amo1.isExo()).isFalse();
        assertThat(this.amo1.maxWeight()).isEqualTo(1);

        assertThat(this.amo2.operands()).containsExactly(litsCC2);
        assertThat(this.amo2.coefficients()).containsExactly(coeffsCC2);
        assertThat(this.amo2.comparator()).isEqualTo(CType.LE);
        assertThat(this.amo2.rhs()).isEqualTo(1);
        assertThat(this.amo2.isCC()).isTrue();
        assertThat(this.amo2.isAmo()).isTrue();
        assertThat(this.amo2.isExo()).isFalse();
        assertThat(this.amo2.maxWeight()).isEqualTo(1);

        assertThat(this.exo1.operands()).containsExactly(lits1);
        assertThat(this.exo1.coefficients()).containsExactly(coeffsCC1);
        assertThat(this.exo1.comparator()).isEqualTo(CType.EQ);
        assertThat(this.exo1.rhs()).isEqualTo(1);
        assertThat(this.exo1.isCC()).isTrue();
        assertThat(this.exo1.isAmo()).isFalse();
        assertThat(this.exo1.isExo()).isTrue();
        assertThat(this.exo1.maxWeight()).isEqualTo(1);

        assertThat(this.exo2.operands()).containsExactly(litsCC2);
        assertThat(this.exo2.coefficients()).containsExactly(coeffsCC2);
        assertThat(this.exo2.comparator()).isEqualTo(CType.EQ);
        assertThat(this.exo2.rhs()).isEqualTo(1);
        assertThat(this.exo2.isCC()).isTrue();
        assertThat(this.exo2.isAmo()).isFalse();
        assertThat(this.exo2.isExo()).isTrue();
        assertThat(this.exo2.maxWeight()).isEqualTo(1);
    }

    @Test
    public void testIsCC() {
        final Literal[] lits1 = new Literal[]{f2.variable("a"), f.variable("b")};
        final Literal[] lits2 = new Literal[]{f2.variable("a"), f.literal("b", false)};
        final int[] coeffs1 = new int[]{1, 1};
        final int[] coeffs2 = new int[]{-1, 1};

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits1, coeffs1)).isCC()).isTrue();

        // Corner cases
        assertThat(((PBConstraint) f.pbc(CType.LE, 0, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.LT, 1, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.GE, 0, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.GT, -1, lits1, coeffs1)).isCC()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 0, lits1, coeffs1)).isCC()).isTrue();

        assertThat(((PBConstraint) f.pbc(CType.LE, -1, lits1, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 0, lits1, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, -1, lits1, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, -2, lits1, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, -1, lits1, coeffs1)).isCC()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits1, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits1, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits1, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits1, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits1, coeffs2)).isCC()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs1)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs1)).isCC()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs2)).isCC()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs2)).isCC()).isFalse();
    }

    @Test
    public void testIsAmo() {
        final Literal[] lits1 = new Literal[]{f2.variable("a"), f.variable("b")};
        final Literal[] lits2 = new Literal[]{f2.variable("a"), f.literal("b", false)};
        final int[] coeffs1 = new int[]{1, 1};
        final int[] coeffs2 = new int[]{-1, 1};

        assertThat(((PBConstraint) f.pbc(CType.LE, 1, lits1, coeffs1)).isAmo()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.LT, 2, lits1, coeffs1)).isAmo()).isTrue();
        assertThat(((PBConstraint) f.pbc(CType.LT, 1, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 1, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 1, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 1, lits1, coeffs1)).isAmo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 3, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 3, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 3, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 3, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 3, lits1, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 3, lits1, coeffs1)).isAmo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits1, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits1, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits1, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits1, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits1, coeffs2)).isAmo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs1)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs1)).isAmo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs2)).isAmo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs2)).isAmo()).isFalse();
    }

    @Test
    public void testIsExo() {
        final Literal[] lits1 = new Literal[]{f2.variable("a"), f.variable("b")};
        final Literal[] lits2 = new Literal[]{f2.variable("a"), f.literal("b", false)};
        final int[] coeffs1 = new int[]{1, 1};
        final int[] coeffs2 = new int[]{-1, 1};

        assertThat(((PBConstraint) f.pbc(CType.LE, 1, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 2, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 1, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 1, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 1, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 1, lits1, coeffs1)).isExo()).isTrue();

        assertThat(((PBConstraint) f.pbc(CType.LE, 3, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 3, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 3, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 3, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 3, lits1, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 3, lits1, coeffs1)).isExo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits1, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits1, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits1, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits1, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits1, coeffs2)).isExo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs1)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs1)).isExo()).isFalse();

        assertThat(((PBConstraint) f.pbc(CType.LE, 4, lits2, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.LT, 4, lits2, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GE, 4, lits2, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.GT, 4, lits2, coeffs2)).isExo()).isFalse();
        assertThat(((PBConstraint) f.pbc(CType.EQ, 4, lits2, coeffs2)).isExo()).isFalse();
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.pb1.numberOfAtoms()).isEqualTo(1);
        assertThat(this.pb2.numberOfAtoms()).isEqualTo(1);
        assertThat(this.cc1.numberOfAtoms()).isEqualTo(1);
        assertThat(this.cc2.numberOfAtoms()).isEqualTo(1);
        assertThat(this.amo1.numberOfAtoms()).isEqualTo(1);
        assertThat(this.amo2.numberOfAtoms()).isEqualTo(1);
        assertThat(this.exo1.numberOfAtoms()).isEqualTo(1);
        assertThat(this.exo2.numberOfAtoms()).isEqualTo(1);
        assertThat(this.exo2.numberOfAtoms()).isEqualTo(1);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.pb1.numberOfNodes()).isEqualTo(2);
        assertThat(this.pb2.numberOfNodes()).isEqualTo(4);
        assertThat(this.cc1.numberOfNodes()).isEqualTo(2);
        assertThat(this.cc2.numberOfNodes()).isEqualTo(4);
        assertThat(this.amo1.numberOfNodes()).isEqualTo(2);
        assertThat(this.amo2.numberOfNodes()).isEqualTo(4);
        assertThat(this.exo1.numberOfNodes()).isEqualTo(2);
        assertThat(this.exo2.numberOfNodes()).isEqualTo(4);
        assertThat(this.exo2.numberOfNodes()).isEqualTo(4);
    }

    @Test
    public void testVariables() {
        final SortedSet<Variable> lits1 = new TreeSet<>(Collections.singletonList(f.variable("a")));
        final SortedSet<Variable> lits2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.variable("b"), f.variable("c")));
        assertThat(this.pb1.variables()).isEqualTo(lits1);
        assertThat(this.pb1.variables()).isEqualTo(lits1);
        assertThat(this.pb2.variables()).isEqualTo(lits2);
        assertThat(this.cc1.variables()).isEqualTo(lits1);
        assertThat(this.cc2.variables()).isEqualTo(lits2);
        assertThat(this.amo1.variables()).isEqualTo(lits1);
        assertThat(this.amo2.variables()).isEqualTo(lits2);
        assertThat(this.exo1.variables()).isEqualTo(lits1);
        assertThat(this.exo2.variables()).isEqualTo(lits2);
    }

    @Test
    public void testLiterals() {
        final SortedSet<Variable> lits1 = new TreeSet<>(Collections.singletonList(f.variable("a")));
        final SortedSet<Literal> lits2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c")));
        final SortedSet<Variable> litsCC2 = new TreeSet<>(Arrays.asList(f.variable("a"), f.variable("b"), f.variable("c")));
        assertThat(this.pb1.literals()).isEqualTo(lits1);
        assertThat(this.pb2.literals()).isEqualTo(lits2);
        assertThat(this.cc1.literals()).isEqualTo(lits1);
        assertThat(this.cc2.literals()).isEqualTo(litsCC2);
        assertThat(this.amo1.literals()).isEqualTo(lits1);
        assertThat(this.amo2.literals()).isEqualTo(litsCC2);
        assertThat(this.exo1.literals()).isEqualTo(lits1);
        assertThat(this.exo2.literals()).isEqualTo(litsCC2);
    }

    @Test
    public void testContains() {
        assertThat(this.pb2.containsVariable(f.variable("a"))).isTrue();
        assertThat(this.pb2.containsVariable(f.variable("b"))).isTrue();
        assertThat(this.pb2.containsVariable(f.variable("c"))).isTrue();
        assertThat(this.pb2.containsVariable(f.variable("d"))).isFalse();
        assertThat(this.pb2.containsVariable(f.variable("x"))).isFalse();
    }

    @Test
    public void testEvaluate() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Integer> coeffs = Arrays.asList(2, -2, 3);
        final Assignment a1 = new Assignment();
        a1.addLiteral(f.variable("a"));
        a1.addLiteral(f.variable("b"));
        a1.addLiteral(f.literal("c", false));
        final Assignment a2 = new Assignment();
        a2.addLiteral(f.variable("a"));
        a2.addLiteral(f.literal("b", false));
        a2.addLiteral(f.literal("c", false));
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.EQ, 2, lits, coeffs);
        final PBConstraint pb3 = (PBConstraint) f.pbc(CType.GE, 1, lits, coeffs);
        final PBConstraint pb4 = (PBConstraint) f.pbc(CType.GT, 0, lits, coeffs);
        final PBConstraint pb5 = (PBConstraint) f.pbc(CType.LE, 1, lits, coeffs);
        final PBConstraint pb6 = (PBConstraint) f.pbc(CType.LT, 2, lits, coeffs);
        assertThat(pb1.evaluate(a1)).isTrue();
        assertThat(pb1.evaluate(a2)).isFalse();
        assertThat(pb3.evaluate(a1)).isTrue();
        assertThat(pb3.evaluate(a2)).isFalse();
        assertThat(pb4.evaluate(a1)).isTrue();
        assertThat(pb4.evaluate(a2)).isFalse();
        assertThat(pb5.evaluate(a1)).isFalse();
        assertThat(pb5.evaluate(a2)).isTrue();
        assertThat(pb6.evaluate(a1)).isFalse();
        assertThat(pb6.evaluate(a2)).isTrue();
    }

    @Test
    public void testRestrict() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Literal> litsA1 = Arrays.asList(f.literal("b", false), f.variable("c"));
        final List<Variable> litsA2 = Collections.singletonList(f.variable("c"));
        final List<Integer> coeffs = Arrays.asList(2, -2, 3);
        final List<Integer> coeffA1 = Arrays.asList(-2, 3);
        final List<Integer> coeffA2 = Collections.singletonList(3);
        final Assignment a1 = new Assignment();
        a1.addLiteral(f.variable("a"));
        final Assignment a2 = new Assignment();
        a2.addLiteral(f.variable("a"));
        a2.addLiteral(f.literal("b", false));
        final Assignment a3 = new Assignment();
        a3.addLiteral(f.variable("a"));
        a3.addLiteral(f.literal("b", false));
        a3.addLiteral(f.variable("c"));
        final Assignment a4 = new Assignment();
        a4.addLiteral(f.literal("a", false));
        a4.addLiteral(f.variable("b"));
        a4.addLiteral(f.literal("c", false));
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.EQ, 2, lits, coeffs);
        assertThat(pb1.restrict(a1)).isEqualTo(f.pbc(CType.EQ, 0, litsA1, coeffA1));
        assertThat(pb1.restrict(a2)).isEqualTo(f.pbc(CType.EQ, 2, litsA2, coeffA2));
        assertThat(pb1.restrict(a3)).isEqualTo(f.falsum());
        assertThat(pb1.restrict(a4)).isEqualTo(f.falsum());
    }

    @Test
    public void testRestrictInequality() {
        final List<Literal> lits = Arrays.asList(f.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"), f.variable("e"), f.literal("f", false));
        final List<Integer> coeffs = Arrays.asList(75, 50, 201, -3, -24, 1);
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.GE, -24, lits, coeffs);
        final PBConstraint pb2 = (PBConstraint) f.pbc(CType.LE, 150, lits, coeffs);
        final Assignment a1 = new Assignment();
        a1.addLiteral(f.literal("b", false));
        a1.addLiteral(f.variable("c"));
        final Assignment a2 = new Assignment();
        a2.addLiteral(f.literal("a", false));
        a2.addLiteral(f.variable("b"));
        a2.addLiteral(f.literal("c", false));
        a2.addLiteral(f.variable("d"));
        a2.addLiteral(f.variable("e"));
        final Assignment a3 = new Assignment();
        a3.addLiteral(f.literal("c", false));

        assertThat(pb1.restrict(a1)).isEqualTo(f.verum());
        assertThat(pb2.restrict(a1)).isEqualTo(f.falsum());
        assertThat(pb1.restrict(a2)).isEqualTo(f.falsum());
        assertThat(pb2.restrict(a3)).isEqualTo(f.verum());
    }

    @Test
    public void testContainsSubformula() {
        assertThat(this.pb1.containsNode(f.variable("a"))).isTrue();
        assertThat(this.pb1.containsNode(f.literal("a", false))).isFalse();
        assertThat(this.pb2.containsNode(f.literal("b", false))).isTrue();
        assertThat(this.pb2.containsNode(f.variable("b"))).isTrue();
        assertThat(this.pb2.containsNode(f.variable("d"))).isFalse();
        assertThat(this.pb1.containsNode(this.pb1)).isTrue();
        assertThat(this.pb2.containsNode(this.pb2)).isTrue();
        assertThat(this.pb2.containsNode(this.pb22)).isTrue();
    }

    @Test
    public void testSubstitute() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Literal> litsS1 = Arrays.asList(f.literal("b", false), f.variable("c"));
        final List<Variable> litsS2 = Collections.singletonList(f.variable("c"));
        final List<Literal> litsS5 = Arrays.asList(f.variable("a2"), f.literal("b2", false), f.variable("c2"));
        final List<Variable> litsS6 = Arrays.asList(f.variable("a2"), f.variable("c"));
        final List<Integer> coeffs = Arrays.asList(2, -2, 3);
        final List<Integer> coeffS1 = Arrays.asList(-2, 3);
        final List<Integer> coeffS2 = Collections.singletonList(3);
        final List<Integer> coeffS6 = Arrays.asList(2, 3);
        final Substitution s1 = new Substitution();
        s1.addMapping(f.variable("a"), f.verum());
        final Substitution s2 = new Substitution();
        s2.addMapping(f.variable("a"), f.verum());
        s2.addMapping(f.variable("b"), f.falsum());
        final Substitution s3 = new Substitution();
        s3.addMapping(f.variable("a"), f.verum());
        s3.addMapping(f.variable("b"), f.falsum());
        s3.addMapping(f.variable("c"), f.verum());
        final Substitution s4 = new Substitution();
        s4.addMapping(f.variable("a"), f.falsum());
        s4.addMapping(f.variable("b"), f.verum());
        s4.addMapping(f.variable("c"), f.falsum());
        final Substitution s5 = new Substitution();
        s5.addMapping(f.variable("a"), f.variable("a2"));
        s5.addMapping(f.variable("b"), f.variable("b2"));
        s5.addMapping(f.variable("c"), f.variable("c2"));
        s5.addMapping(f.variable("d"), f.variable("d2"));
        final Substitution s6 = new Substitution();
        s6.addMapping(f.variable("a"), f.variable("a2"));
        s6.addMapping(f.variable("b"), f.falsum());
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.EQ, 2, lits, coeffs);
        assertThat(pb1.substitute(s1)).isEqualTo(f.pbc(CType.EQ, 0, litsS1, coeffS1));
        assertThat(pb1.substitute(s2)).isEqualTo(f.pbc(CType.EQ, 2, litsS2, coeffS2));
        assertThat(pb1.substitute(s3)).isEqualTo(f.falsum());
        assertThat(this.pb2.substitute(s3)).isEqualTo(f.verum());
        assertThat(pb1.substitute(s4)).isEqualTo(f.falsum());
        assertThat(this.pb2.substitute(s4)).isEqualTo(f.verum());
        assertThat(pb1.substitute(s5)).isEqualTo(f.pbc(CType.EQ, 2, litsS5, coeffs));
        assertThat(pb1.substitute(s6)).isEqualTo(f.pbc(CType.EQ, 4, litsS6, coeffS6));
    }

    @Test
    public void testNegation() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Integer> coeffs = Arrays.asList(2, -2, 3);
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.EQ, 2, lits, coeffs);
        final PBConstraint pb3 = (PBConstraint) f.pbc(CType.GE, 1, lits, coeffs);
        final PBConstraint pb4 = (PBConstraint) f.pbc(CType.GT, 0, lits, coeffs);
        final PBConstraint pb5 = (PBConstraint) f.pbc(CType.LE, 1, lits, coeffs);
        final PBConstraint pb6 = (PBConstraint) f.pbc(CType.LT, 2, lits, coeffs);
        final PBConstraint pb7 = (PBConstraint) f.pbc(CType.EQ, -2, lits, coeffs);
        assertThat(pb1.negate()).isEqualTo(f.or(f.pbc(CType.LT, 2, lits, coeffs), f.pbc(CType.GT, 2, lits, coeffs)));
        assertThat(pb3.negate()).isEqualTo(f.pbc(CType.LT, 1, lits, coeffs));
        assertThat(pb4.negate()).isEqualTo(f.pbc(CType.LE, 0, lits, coeffs));
        assertThat(pb5.negate()).isEqualTo(f.pbc(CType.GT, 1, lits, coeffs));
        assertThat(pb6.negate()).isEqualTo(f.pbc(CType.GE, 2, lits, coeffs));
        assertThat(pb7.negate()).isEqualTo(f.or(f.pbc(CType.LT, -2, lits, coeffs), f.pbc(CType.GT, -2, lits, coeffs)));
    }

    @Test
    public void testNNF() {
        assertThat(this.pb1.nnf()).isEqualTo(f.literal("a", false));
        assertThat(this.cc1.nnf()).isEqualTo(f.literal("a", false));
        assertThat(this.amo1.nnf()).isEqualTo(f.verum());
        assertThat(this.exo1.nnf()).isEqualTo(f.variable("a"));
    }

    @Test
    public void testNormalization() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"),
                f.literal("b", false));
        final List<Integer> coeffs = Arrays.asList(2, -3, 3, 0, 1);
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.EQ, 2, lits, coeffs);
        final PBConstraint pb2 = (PBConstraint) f.pbc(CType.GE, 1, lits, coeffs);
        final PBConstraint pb3 = (PBConstraint) f.pbc(CType.GT, 0, lits, coeffs);
        final PBConstraint pb4 = (PBConstraint) f.pbc(CType.LE, 1, lits, coeffs);
        final PBConstraint pb5 = (PBConstraint) f.pbc(CType.LT, 2, lits, coeffs);
        assertThat(pb1.normalize().toString()).isEqualTo("(2*a + 2*b + 3*c <= 4) & (2*~a + 2*~b + 3*~c <= 3)");
        assertThat(pb2.normalize().toString()).isEqualTo("2*~a + 2*~b + 3*~c <= 4");
        assertThat(pb3.normalize().toString()).isEqualTo("2*~a + 2*~b + 3*~c <= 4");
        assertThat(pb4.normalize().toString()).isEqualTo("2*a + 2*b + 3*c <= 3");
        assertThat(pb5.normalize().toString()).isEqualTo("2*a + 2*b + 3*c <= 3");
    }

    @Test
    public void testNormalizationTrivial() {
        final List<Literal> lits = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"), f.variable("d"));
        final List<Integer> coeffs = Arrays.asList(2, -2, 3, 0);
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.LE, 4, lits, coeffs);
        final PBConstraint pb2 = (PBConstraint) f.pbc(CType.LE, 5, lits, coeffs);
        final PBConstraint pb3 = (PBConstraint) f.pbc(CType.LE, 7, lits, coeffs);
        final PBConstraint pb4 = (PBConstraint) f.pbc(CType.LE, 10, lits, coeffs);
        final PBConstraint pb5 = (PBConstraint) f.pbc(CType.LE, -3, lits, coeffs);
        assertThat(pb1.normalize().toString()).isEqualTo("2*a + 2*b + 3*c <= 6");
        assertThat(pb2.normalize()).isEqualTo(f.verum());
        assertThat(pb3.normalize()).isEqualTo(f.verum());
        assertThat(pb4.normalize()).isEqualTo(f.verum());
        assertThat(pb5.normalize()).isEqualTo(f.falsum());
    }

    @Test
    public void testNormalizationSimplifications() {
        List<? extends Literal> lits = Arrays.asList(f2.variable("a"), f.variable("a"), f.variable("c"), f.variable("d"));
        List<Integer> coeffs = Arrays.asList(2, -2, 4, 4);
        final PBConstraint pb1 = (PBConstraint) f.pbc(CType.LE, 4, lits, coeffs);
        assertThat(pb1.normalize().toString()).isEqualTo("c + d <= 1");
        lits = Arrays.asList(f2.variable("a"), f.literal("a", false), f.variable("c"), f.variable("d"));
        coeffs = Arrays.asList(2, 2, 4, 2);
        final PBConstraint pb2 = (PBConstraint) f.pbc(CType.LE, 4, lits, coeffs);
        assertThat(pb2.normalize().toString()).isEqualTo("2*c + d <= 1");
    }

    @Test
    public void testToString() {
        assertThat(this.pb1.toString()).isEqualTo("3*a <= 2");
        assertThat(this.pb2.toString()).isEqualTo("3*a + -2*~b + 7*c <= 8");
        assertThat(this.pb22.toString()).isEqualTo("3*a + -2*~b + 7*c <= 8");
        assertThat(this.cc1.toString()).isEqualTo("a < 1");
        assertThat(this.cc2.toString()).isEqualTo("a + b + c >= 2");
        assertThat(this.amo1.toString()).isEqualTo("a <= 1");
        assertThat(this.amo2.toString()).isEqualTo("a + b + c <= 1");
        assertThat(this.exo1.toString()).isEqualTo("a = 1");
        assertThat(this.exo2.toString()).isEqualTo("a + b + c = 1");
    }

    @Test
    public void testEquals() {
        final List<Literal> lits2 = Arrays.asList(f2.variable("a"), f.literal("b", false), f.variable("c"));
        final List<Integer> coeffs2 = Arrays.asList(3, -2, 7);
        final List<Literal> lits2alt1 = Arrays.asList(f2.variable("a"), f.literal("b", false));
        final List<Integer> coeffs2alt1 = Arrays.asList(3, -2);
        final List<Variable> lits2alt2 = Arrays.asList(f2.variable("a"), f.variable("b"), f.variable("c"));
        final List<Integer> coeffs2alt2 = Arrays.asList(3, -2, 8);
        assertThat(this.pb1).isEqualTo(this.pb1);
        assertThat(this.pb22).isEqualTo(this.pb2);
        assertThat(f.pbc(CType.LE, 8, lits2, coeffs2)).isEqualTo(this.pb2);
        assertThat(this.cc2).isNotEqualTo(this.cc1);
        assertThat(this.cc1).isNotEqualTo(null);
        assertThat(this.cc2).isNotEqualTo("String");
        assertThat("String").isNotEqualTo(this.cc2);
        assertThat(f.pbc(CType.LE, 8, lits2alt1, coeffs2alt1)).isNotEqualTo(this.pb2);
        assertThat(f.pbc(CType.LE, 8, lits2alt2, coeffs2)).isNotEqualTo(this.pb2);
        assertThat(f.pbc(CType.LE, 8, lits2, coeffs2alt2)).isNotEqualTo(this.pb2);
        assertThat(f.pbc(CType.LT, 8, lits2, coeffs2)).isNotEqualTo(this.pb2);
        assertThat(f.pbc(CType.LE, 7, lits2, coeffs2)).isNotEqualTo(this.pb2);
    }

    @Test
    public void testHash() {
        assertThat(this.pb1.hashCode()).isEqualTo(this.pb1.hashCode());
        assertThat(this.pb2.hashCode()).isEqualTo(this.pb2.hashCode());
        assertThat(this.pb22.hashCode()).isEqualTo(this.pb2.hashCode());
    }

    @Test
    public void testNumberOfInternalNodes() {
        assertThat(this.pb2.numberOfInternalNodes()).isEqualTo(1);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.pb1.numberOfOperands()).isEqualTo(0);
        assertThat(this.pb2.numberOfOperands()).isEqualTo(0);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.pb1.isConstantFormula()).isFalse();
        assertThat(this.pb2.isConstantFormula()).isFalse();
        assertThat(this.pb22.isConstantFormula()).isFalse();
        assertThat(this.cc1.isConstantFormula()).isFalse();
        assertThat(this.cc2.isConstantFormula()).isFalse();
        assertThat(this.amo1.isConstantFormula()).isFalse();
        assertThat(this.amo2.isConstantFormula()).isFalse();
        assertThat(this.exo1.isConstantFormula()).isFalse();
        assertThat(this.exo2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.pb1.isAtomicFormula()).isTrue();
        assertThat(this.pb2.isAtomicFormula()).isTrue();
        assertThat(this.pb22.isAtomicFormula()).isTrue();
        assertThat(this.cc1.isAtomicFormula()).isTrue();
        assertThat(this.cc2.isAtomicFormula()).isTrue();
        assertThat(this.amo1.isAtomicFormula()).isTrue();
        assertThat(this.amo2.isAtomicFormula()).isTrue();
        assertThat(this.exo1.isAtomicFormula()).isTrue();
        assertThat(this.exo2.isAtomicFormula()).isTrue();
    }

    @Test
    public void testEvaluateCoeffs() {
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -3, CType.EQ)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 3, CType.EQ)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -2, CType.EQ)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 2, CType.EQ)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 0, CType.EQ)).isEqualTo(Tristate.UNDEF);

        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -3, CType.GE)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 3, CType.GE)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -2, CType.GE)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 2, CType.GE)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 0, CType.GE)).isEqualTo(Tristate.UNDEF);

        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -3, CType.GT)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 3, CType.GT)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -2, CType.GT)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 2, CType.GT)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 0, CType.GT)).isEqualTo(Tristate.UNDEF);

        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -3, CType.LE)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 3, CType.LE)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -2, CType.LE)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 2, CType.LE)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 0, CType.LE)).isEqualTo(Tristate.UNDEF);

        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -3, CType.LT)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 3, CType.LT)).isEqualTo(Tristate.TRUE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, -2, CType.LT)).isEqualTo(Tristate.FALSE);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 2, CType.LT)).isEqualTo(Tristate.UNDEF);
        assertThat(PBConstraint.evaluateCoeffs(-2, 2, 0, CType.LT)).isEqualTo(Tristate.UNDEF);
    }

    @Test
    public void testTrivialTrue() {
        assertThat(f.pbc(CType.EQ, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.EQ, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.EQ, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());

        assertThat(f.pbc(CType.GT, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GT, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GT, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());

        assertThat(f.pbc(CType.GE, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.GE, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GE, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());

        assertThat(f.pbc(CType.LT, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.LT, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LT, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());

        assertThat(f.pbc(CType.LE, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LE, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LE, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
    }

    @Test
    public void testTrivialFalse() {
        assertThat(f.pbc(CType.EQ, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.EQ, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.EQ, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());

        assertThat(f.pbc(CType.GT, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GT, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GT, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());

        assertThat(f.pbc(CType.GE, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.GE, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.GE, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());

        assertThat(f.pbc(CType.LT, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
        assertThat(f.pbc(CType.LT, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LT, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());

        assertThat(f.pbc(CType.LE, 0, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LE, 1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.verum());
        assertThat(f.pbc(CType.LE, -1, new ArrayList<>(), new ArrayList<>())).isEqualTo(f.falsum());
    }

    @Test
    public void testSimplifiedToString() {
        assertThat(f.pbc(CType.EQ, 0, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.EQ, 1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.EQ, -1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.GT, 0, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.GT, 1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.GT, -1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.GE, 0, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.GE, 1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.GE, -1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.LT, 0, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.LT, 1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.LT, -1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
        assertThat(f.pbc(CType.LE, 0, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.LE, 1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$true");
        assertThat(f.pbc(CType.LE, -1, new ArrayList<>(), new ArrayList<>()).toString()).isEqualTo("$false");
    }
}
