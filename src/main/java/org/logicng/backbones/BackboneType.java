// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.backbones;

/**
 * An enumeration which type of backbone should be computed:
 * <ul>
 * <li>{@code ONLY_POSITIVE}: only variables which occur positive in every model
 * <li>{@code ONLY_NEGATIVE}: only variables which occur negative in every model
 * <li>{@code POSITIVE_AND_NEGATIVE}: variables which occur positive in every
 * model, variables which occur negative in every model and optional variables
 * (neither in the positive nor negative backbone)
 * </ul>
 * @version 1.5.0
 * @since 1.5.0
 */
public enum BackboneType {
    /**
     * Only variables in the positive backbone
     */
    ONLY_POSITIVE,

    /**
     * Only variables in the negative backbone
     */
    ONLY_NEGATIVE,

    /**
     * Variables in the positive backbone, in the negative backbone and optional
     */
    POSITIVE_AND_NEGATIVE
}
