package org.logicng.formulas;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Unit tests for formula types.
 * @version 2.0.0
 * @since 2.0.0
 */
public class FTypeTest {

    @Test
    public void testDual() {
        assertThat(FType.dual(FType.AND)).isEqualTo(FType.OR);
        assertThat(FType.dual(FType.OR)).isEqualTo(FType.AND);

        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.FALSE));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.TRUE));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.LITERAL));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.NOT));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.IMPL));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.EQUIV));
        assertThrows(IllegalArgumentException.class, () -> FType.dual(FType.PBC));
    }
}
