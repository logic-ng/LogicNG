// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.backbones;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

import org.assertj.core.data.MapEntry;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link Backbone}.
 * @version 2.3.1
 * @since 1.5.0
 */
public class BackboneTest {
    private final FormulaFactory f = new FormulaFactory();
    private final PropositionalParser p = new PropositionalParser(this.f);
    private final Variable a1 = this.f.variable("a1");
    private final Variable a2 = this.f.variable("a2");
    private final Variable a3 = this.f.variable("a3");
    private final Variable b1 = this.f.variable("b1");
    private final Variable b2 = this.f.variable("b2");
    private final Variable b3 = this.f.variable("b3");
    private final Variable x1 = this.f.variable("x1");
    private final Variable x2 = this.f.variable("x2");
    private final Variable x3 = this.f.variable("x3");

    @Test
    public void testToFormula() throws ParserException {
        Backbone backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), null, null);
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("a1 & a2 & a3"));
        backbone = Backbone.satBackbone(null, set(this.b1, this.b2, this.b3), null);
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("~b1 & ~b2 & ~b3"));
        backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), set(this.b1), null);
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("a1 & a2 & a3 & ~b1"));
        backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), null, set(this.x1, this.x2, this.x3));
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("a1 & a2 & a3"));
        backbone = Backbone.satBackbone(null, set(this.b1, this.b2, this.b3), set(this.x1, this.x2, this.x3));
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("~b1 & ~b2 & ~b3"));
        backbone = Backbone.satBackbone(set(this.a1), set(this.b1, this.b2, this.b3), set(this.x1));
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.p.parse("a1 & ~b1 & ~b2 & ~b3"));
    }

    @Test
    public void testToMap() {
        Backbone backbone = Backbone.satBackbone(set(this.a1, this.a2), null, null);
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.a1, TRUE),
                MapEntry.entry(this.a2, TRUE)
        );
        backbone = Backbone.satBackbone(null, set(this.b1, this.b2, this.b3), null);
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.b1, FALSE),
                MapEntry.entry(this.b2, FALSE),
                MapEntry.entry(this.b3, FALSE)
        );
        backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), set(this.b1), null);
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.a1, TRUE),
                MapEntry.entry(this.a2, TRUE),
                MapEntry.entry(this.a3, TRUE),
                MapEntry.entry(this.b1, FALSE)
        );
        backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), null, set(this.x1, this.x2, this.x3));
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.a1, TRUE),
                MapEntry.entry(this.a2, TRUE),
                MapEntry.entry(this.a3, TRUE),
                MapEntry.entry(this.x1, UNDEF),
                MapEntry.entry(this.x2, UNDEF),
                MapEntry.entry(this.x3, UNDEF)
        );
        backbone = Backbone.satBackbone(null, set(this.b1, this.b2, this.b3), set(this.x1, this.x2, this.x3));
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.b1, FALSE),
                MapEntry.entry(this.b2, FALSE),
                MapEntry.entry(this.b3, FALSE),
                MapEntry.entry(this.x1, UNDEF),
                MapEntry.entry(this.x2, UNDEF),
                MapEntry.entry(this.x3, UNDEF)
        );
        backbone = Backbone.satBackbone(set(this.a1), set(this.b1, this.b2), set(this.x1));
        assertThat(backbone.toMap()).containsExactly(
                MapEntry.entry(this.a1, TRUE),
                MapEntry.entry(this.b1, FALSE),
                MapEntry.entry(this.b2, FALSE),
                MapEntry.entry(this.x1, UNDEF)
        );
    }

    @Test
    public void testUnsatBackbone() {
        final Backbone backbone = Backbone.unsatBackbone();
        assertThat(backbone.getCompleteBackbone()).isEmpty();
        assertThat(backbone.getNegativeBackbone()).isEmpty();
        assertThat(backbone.getPositiveBackbone()).isEmpty();
        assertThat(backbone.getOptionalVariables()).isEmpty();
        assertThat(backbone.toMap()).isEmpty();
        assertThat(backbone.toFormula(this.f)).isEqualTo(this.f.falsum());
    }

    @Test
    public void testToString() {
        final Backbone backbone = Backbone.satBackbone(set(this.a1, this.a2, this.a3), set(this.b1, this.b2, this.b3), set(this.x1, this.x2, this.x3));
        assertThat(backbone.toString()).isEqualTo("Backbone{sat=true, positiveBackbone=[a1, a2, a3], negativeBackbone=[b1, b2, b3], optionalVariables=[x1, x2, x3]}");
    }

    @Test
    public void testEqualsAndHashCode() {
        final Backbone backbone1a = Backbone.satBackbone(set(this.a1, this.a2, this.a3), null, null);
        final Backbone backbone1b = Backbone.satBackbone(set(this.a1, this.a2, this.a3), null, null);
        final Backbone backbone3 = Backbone.satBackbone(set(this.a1, this.a2, this.a3), set(this.b1), null);
        final Backbone backbone5 = Backbone.satBackbone(null, set(this.b1, this.b2, this.b3), set(this.x1, this.x2, this.x3));
        final Backbone satBB = Backbone.satBackbone(Collections.emptySortedSet(), Collections.emptySortedSet(), Collections.emptySortedSet());
        final Backbone unsatBB = Backbone.unsatBackbone();

        assertThat(backbone1a.hashCode()).isEqualTo(backbone1b.hashCode());
        assertThat(backbone1a.equals(backbone1a)).isTrue();
        assertThat(backbone1a.equals(backbone1b)).isTrue();
        assertThat(backbone1a.equals(backbone3)).isFalse();
        assertThat(backbone1a.equals(backbone5)).isFalse();
        assertThat(backbone1a.equals("String")).isFalse();
        assertThat(backbone1a.equals(null)).isFalse();
        assertThat(satBB.equals(unsatBB)).isFalse();
    }

    private SortedSet<Variable> set(final Variable... variables) {
        return new TreeSet<>(Arrays.asList(variables));
    }
}
