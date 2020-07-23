package org.logicng;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class LogicNGVersionTest {
    @Test
    public void testMajor() {
        assertThat(LogicNGVersion.major("2.0.0")).isEqualTo(2);
        assertThat(LogicNGVersion.major("2.0.0-SNAPSHOT")).isEqualTo(2);
        assertThat(LogicNGVersion.major("2.1.3")).isEqualTo(2);
        assertThat(LogicNGVersion.major("42.7.19")).isEqualTo(42);
        assertThat(LogicNGVersion.major("0.0.0")).isEqualTo(0);
    }

    @Test
    public void testMinor() {
        assertThat(LogicNGVersion.minor("2.0.0")).isEqualTo(0);
        assertThat(LogicNGVersion.minor("2.3.0-SNAPSHOT")).isEqualTo(3);
        assertThat(LogicNGVersion.minor("2.1.3")).isEqualTo(1);
        assertThat(LogicNGVersion.minor("42.7.19")).isEqualTo(7);
        assertThat(LogicNGVersion.minor("0.123.0")).isEqualTo(123);
    }

    @Test
    public void testPatch() {
        assertThat(LogicNGVersion.patch("2.0.3")).isEqualTo(3);
        assertThat(LogicNGVersion.patch("2.3.3-SNAPSHOT")).isEqualTo(3);
        assertThat(LogicNGVersion.patch("2.1.0")).isEqualTo(0);
        assertThat(LogicNGVersion.patch("42.7.19")).isEqualTo(19);
        assertThat(LogicNGVersion.patch("0.123.22")).isEqualTo(22);
    }

    @Test
    public void testSnapshot() {
        assertThat(LogicNGVersion.snapshot("2.0.3")).isFalse();
        assertThat(LogicNGVersion.snapshot("2.0.3-SNAPSHOT")).isTrue();
        assertThat(LogicNGVersion.snapshot("2.0.3-HUGO")).isFalse();
    }

}
