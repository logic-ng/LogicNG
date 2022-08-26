package org.logicng.solvers.functions.splitvariablesprovider;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class SplitVariableProviderTest extends TestWithExampleFormulas {

    @Test
    public void testFixedVariableProvider() {
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, null)).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, new TreeSet<>())).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(MiniSat.miniSat(this.f), varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
    }

    @Test
    public void testLessCommonVariablesProvider() {
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, null)).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, new TreeSet<>())).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(MiniSat.miniSat(this.f), varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
    }

    private SortedSet<Variable> varSet(final String varString) {
        return Arrays.stream(varString.split(" ")).map(this.f::variable).collect(Collectors.toCollection(TreeSet::new));
    }

}
