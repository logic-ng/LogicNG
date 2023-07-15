// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Substitution;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.util.HashMap;

/**
 * Unit tests for {@link Anonymizer}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class AnonymizerTest {

    @Test
    public void testSimpleFormulasDefault() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer();
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("v0"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~v0"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("v0 => ~v1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("v0 <=> ~v1"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("v0 | v1 | ~v3 | v2"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("v0 & v1 & v2 & ~v3"));
    }

    @Test
    public void testSimpleFormulasOwnPrefix() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var");
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("var0"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~var0"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("var0 => ~var1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var0 <=> ~var1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var0 <=> ~var1"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("var0 | var1 | ~var3 | var2"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("var0 & var1 & var2 & ~var3"));
    }

    @Test
    public void testSimpleFormulasOwnPrefixAndCounter() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var", 10);
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("var10"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~var10"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("var10 => ~var11"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var10 <=> ~var11"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("var10 | var11 | ~var13 | var12"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("var10 & var11 & var12 & ~var13"));
    }

    @Test
    public void testSimpleFormulasOwnPrefixAndCounterWithoutCache() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var", 10);
        assertThat(p.parse("$true").transform(anonymizer, false)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer, false)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer, false)).isEqualTo(p.parse("var10"));
        assertThat(p.parse("~A").transform(anonymizer, false)).isEqualTo(p.parse("~var10"));
        assertThat(p.parse("A => ~B").transform(anonymizer, false)).isEqualTo(p.parse("var10 => ~var11"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer, false)).isEqualTo(p.parse("var10 <=> ~var11"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer, false)).isEqualTo(p.parse("var10 | var11 | ~var13 | var12"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer, false)).isEqualTo(p.parse("var10 & var11 & var12 & ~var13"));
    }

    @Test
    public void testGetSubstitution() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("v", 0);
        assertThat(anonymizer.getSubstitution()).isEqualTo(new Substitution());
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer, false)).isEqualTo(p.parse("v0 & v1 & v2 & ~v3"));
        final HashMap<Variable, Formula> mapping = new HashMap<>();
        mapping.put(f.variable("A"), f.variable("v0"));
        mapping.put(f.variable("B"), f.variable("v1"));
        mapping.put(f.variable("C"), f.variable("v2"));
        mapping.put(f.variable("D"), f.variable("v3"));
        assertThat(anonymizer.getSubstitution()).isEqualTo(new Substitution(mapping));
        assertThat(p.parse("E & A & C & ~F").transform(anonymizer, false)).isEqualTo(p.parse("v4 & v0 & v2 & ~v5"));
        mapping.put(f.variable("E"), f.variable("v4"));
        mapping.put(f.variable("F"), f.variable("v5"));
        assertThat(anonymizer.getSubstitution()).isEqualTo(new Substitution(mapping));
    }
}
