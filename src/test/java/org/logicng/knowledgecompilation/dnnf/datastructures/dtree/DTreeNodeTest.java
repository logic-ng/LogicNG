// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;

/**
 * Unit tests for {@link DTreeNode}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DTreeNodeTest extends TestWithExampleFormulas {

    @Test
    public void testToString() throws ParserException {
        final DTreeNode node =
                new DTreeNode(new DTreeLeaf(1, this.f.parse("a | b")), new DTreeLeaf(2, this.f.parse("c | d")));
        assertThat(node.toString()).isEqualTo("DTreeNode: [DTreeLeaf: 1, a | b, DTreeLeaf: 2, c | d]");
    }
}
