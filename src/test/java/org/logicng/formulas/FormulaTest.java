// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_DNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.CacheEntry;
import org.logicng.io.parsers.ParserException;
import org.logicng.transformations.cnf.BDDCNFTransformation;

import java.util.Arrays;

/**
 * Test some common formula functionality.
 * @version 2.0.0
 * @since 1.0
 */
public class FormulaTest {

    private enum MyOwnCacheKey implements CacheEntry {
        MYKEY1("My Key 1"),
        MYKEY2("My Key 2");

        private final String description;

        MyOwnCacheKey(final String description) {
            this.description = description;
        }

        @Override
        public String description() {
            return "MyOwnCacheKey{description=" + this.description + "}";
        }
    }

    @Test
    public void testStringContains() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        assertThat(formula.containsVariable("a")).isTrue();
        assertThat(formula.containsVariable("b")).isTrue();
        assertThat(formula.containsVariable("x")).isFalse();
        assertThat(formula.containsVariable("y")).isFalse();
    }

    @Test
    public void testTransformationCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setTransformationCacheEntry(FACTORIZED_CNF, f.or(f.literal("a", false), f.literal("b", false)));
        assertThat(formula.transformationCacheEntry(FACTORIZED_CNF)).isEqualTo(f.or(f.literal("a", false), f.literal("b", false)));
    }

    @Test
    public void testPredicateCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setPredicateCacheEntry(IS_CNF, false);
        formula.setPredicateCacheEntry(IS_DNF, Tristate.UNDEF);
        assertThat(formula.predicateCacheEntry(IS_CNF)).isEqualTo(Tristate.FALSE);
        assertThat(formula.predicateCacheEntry(IS_DNF)).isEqualTo(Tristate.UNDEF);
    }

    @Test
    public void testFunctionCache() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
        formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY1, "key1");
        formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY2, "key2");
        assertThat(MyOwnCacheKey.MYKEY1.description).isEqualTo("My Key 1");
        assertThat(formula.functionCacheEntry(MyOwnCacheKey.MYKEY1)).isEqualTo("key1");
        assertThat(formula.functionCacheEntry(MyOwnCacheKey.MYKEY2)).isEqualTo("key2");
    }

    @Test
    public void testFType() {
        assertThat(FType.valueOf("AND")).isEqualTo(FType.AND);
        assertThat(Arrays.asList(FType.values()).contains(FType.valueOf("PBC"))).isTrue();
        assertThat(FType.values().length).isEqualTo(9);
    }

    @Test
    public void testCType() {
        assertThat(CType.valueOf("EQ")).isEqualTo(CType.EQ);
        assertThat(CType.valueOf("LE")).isEqualTo(CType.LE);
        assertThat(Arrays.asList(CType.values()).contains(CType.valueOf("GT"))).isTrue();
        assertThat(CType.values().length).isEqualTo(5);
    }

    @Test
    public void testIsSatisfiable() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("~a & ~b & (a | b)");
        assertThat(f.falsum().isSatisfiable()).isFalse();
        assertThat(f.verum().isSatisfiable()).isTrue();
        assertThat(f1.isSatisfiable()).isTrue();
        assertThat(f2.isSatisfiable()).isFalse();
    }

    @Test
    public void testIsTautology() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("(a & b) | (~a & b) | (a & ~b) | (~a & ~b)");
        assertThat(f.falsum().isTautology()).isFalse();
        assertThat(f.verum().isTautology()).isTrue();
        assertThat(f1.isTautology()).isFalse();
        assertThat(f2.isTautology()).isTrue();
    }

    @Test
    public void testIsContradiction() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("~a & ~b & (a | b)");
        assertThat(f.falsum().isContradiction()).isTrue();
        assertThat(f.verum().isContradiction()).isFalse();
        assertThat(f1.isContradiction()).isFalse();
        assertThat(f2.isContradiction()).isTrue();
    }

    @Test
    public void testImplies() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("(a | b) & (c | ~d) & (e | ~f)");
        final Formula f3 = f.parse("(a | b) & (c | d)");
        assertThat(f1.implies(f2)).isFalse();
        assertThat(f2.implies(f1)).isTrue();
        assertThat(f1.implies(f3)).isFalse();
        assertThat(f2.implies(f3)).isFalse();
        assertThat(f2.implies(f2)).isTrue();
    }

    @Test
    public void testIsImpliedBy() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("(a | b) & (c | ~d) & (e | ~f)");
        final Formula f3 = f.parse("(a | b) & (c | d)");
        assertThat(f1.isImpliedBy(f2)).isTrue();
        assertThat(f2.isImpliedBy(f1)).isFalse();
        assertThat(f1.isImpliedBy(f3)).isFalse();
        assertThat(f2.isImpliedBy(f3)).isFalse();
        assertThat(f2.isImpliedBy(f2)).isTrue();
    }

    @Test
    public void testIsEquivalentTo() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula f1 = f.parse("(a | b) & (c | ~d)");
        final Formula f2 = f.parse("(a | b) & (c | ~d) & (e | ~f)");
        final Formula f3 = f.parse("(a & c) | (a & ~d) | (b & c) | (b & ~d)");
        assertThat(f1.isEquivalentTo(f2)).isFalse();
        assertThat(f2.isEquivalentTo(f1)).isFalse();
        assertThat(f1.isEquivalentTo(f3)).isTrue();
        assertThat(f3.isEquivalentTo(f1)).isTrue();
        assertThat(f2.isEquivalentTo(f3)).isFalse();
        assertThat(f2.isEquivalentTo(f2.transform(new BDDCNFTransformation()))).isTrue();
    }
}
