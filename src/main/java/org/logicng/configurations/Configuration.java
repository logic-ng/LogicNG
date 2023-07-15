// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.configurations;

/**
 * A configuration in LogicNG.
 * @version 1.0
 * @since 1.0
 */
public abstract class Configuration {
    protected final ConfigurationType type;

    /**
     * Constructs a new configuration with a given type.
     * @param type the configuration type
     */
    protected Configuration(final ConfigurationType type) {
        this.type = type;
    }

    /**
     * Returns the type of this configuration.
     * @return the type of this configuration
     */
    public ConfigurationType type() {
        return this.type;
    }
}
