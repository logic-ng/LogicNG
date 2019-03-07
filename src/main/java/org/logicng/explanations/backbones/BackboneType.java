package org.logicng.explanations.backbones;

/**
 * An enumeration which type of backbone should be computed: only variables which have to be
 * positive (necessary), only variables which have to be negative (forbidden), or both of
 * them.
 * @version 1.5
 * @since 1.5
 */
public enum BackboneType {
    /**
     * Only positive (necessary) variables
     */
    ONLY_POSITIVE,

    /**
     * Only negative (forbidden) variables
     */
    ONLY_NEGATIVE,

    /**
     * Both, positive and negative variables
     */
    POSITIVE_AND_NEGATIVE
}
