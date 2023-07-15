// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.graphs.io.conditions;

import org.assertj.core.api.Condition;

import java.util.List;

/**
 * A condition needed for AssertJ-Assertions for {@link org.logicng.graphs.io.GraphDotFileWriterTest} and {@link org.logicng.graphs.io.GraphDimacsFileWriterTest}.
 * @version 1.2
 * @since 1.2
 */
public class ContainsCondition extends Condition<List<? extends String>> {

    private final String element;

    public ContainsCondition(String element) {
        this.element = element;
    }

    @Override
    public boolean matches(List<? extends String> strings) {
        return strings.contains(element);
    }

}
