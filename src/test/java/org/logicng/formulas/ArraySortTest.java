package org.logicng.formulas;

import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link org.logicng.formulas.FormulaFactory.PBOperands#sortArrays(Literal[], int[])}.
 * @version 1.4.1
 * @since 1.4.1
 */
public class ArraySortTest {

    @Test
    public void testAlreadySorted() {
        final String array = "1 2 3 4 5 6 8 9";
        final Literal[] literals = literals(array);
        final int[] coefficients = coefficients(array);
        FormulaFactory.PBOperands.sortArrays(literals, coefficients);
        isSorted(literals);
        isSorted(coefficients);
    }

    @Test
    public void testAlreadyReversed() {
        final String array = "9 8 7 6 5 4 3 2 1";
        final Literal[] literals = literals(array);
        final int[] coefficients = coefficients(array);
        FormulaFactory.PBOperands.sortArrays(literals, coefficients);
        isSorted(literals);
        isSorted(coefficients);
    }

    @Test
    public void testRandom() {
        final String array = "1 7 3 4 9 8 2 5 6";
        final Literal[] literals = literals(array);
        final int[] coefficients = coefficients(array);
        FormulaFactory.PBOperands.sortArrays(literals, coefficients);
        isSorted(literals);
        isSorted(coefficients);
    }

    @Test
    public void testDoubleValues() {
        final String array = "1 7 7 3 4 9 9 8 2 1 5 9 6 3";
        final Literal[] literals = literals(array);
        final int[] coefficients = coefficients(array);
        FormulaFactory.PBOperands.sortArrays(literals, coefficients);
        isSorted(literals);
        isSorted(coefficients);
    }

    @Test
    public void testEmpty() {
        final String array = "";
        final Literal[] literals = literals(array);
        final int[] coefficients = coefficients(array);
        FormulaFactory.PBOperands.sortArrays(literals, coefficients);
        isSorted(literals);
        isSorted(coefficients);
    }

    public Literal[] literals(final String string) {
        final String[] split = string.split(" ");
        final Literal[] literals = new Literal[split.length];
        final FormulaFactory f = new FormulaFactory();
        for (int i = 0; i < split.length; i++)
            if (!split[i].isEmpty())
                literals[i] = f.variable(split[i]);
        return literals;
    }

    public int[] coefficients(final String string) {
        final String[] split = string.split(" ");
        final int[] coeffs = new int[split.length];
        for (int i = 0; i < split.length; i++)
            if (!split[i].isEmpty())
                coeffs[i] = Integer.parseInt(split[i]);
        return coeffs;
    }

    private void isSorted(final Literal[] array) {
        for (int i = 0; i < array.length - 1; i++)
            assertThat(array[i].compareTo(array[i + 1])).isLessThanOrEqualTo(0);
    }

    private void isSorted(final int[] array) {
        for (int i = 0; i < array.length - 1; i++)
            assertThat(array[i]).isLessThanOrEqualTo(array[i + 1]);
    }
}
