// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical.generators;

import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

/**
 * An abstract super class for a label mapper for a graphical representation of
 * a BDD.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class BddLabelMapper extends BddMapper implements LabelMapper<Integer> {

    /**
     * Constructs a new BDD label mapper for a given BDD kernel. The BDDs must
     * be constructed with this kernel.
     * @param kernel a BDD kernel
     */
    public BddLabelMapper(final BDDKernel kernel) {
        super(kernel);
    }

    @Override
    public abstract String computeLabel(final Integer content);
}
