package org.logicng.bdds.orderings;

/**
 * An enumeration for the different BDD variable orderings.  A variable ordering is associated
 * with its own provider which can generate orderings for this ordering.
 * @version 1.4.0
 * @since 1.4.0
 */
public enum VariableOrdering {

  DFS(DFSOrdering.class),
  BFS(BFSOrdering.class);

  private final Class<? extends VariableOrderingProvider> provider;

  /**
   * Constructs a new variable ordering with a given provider.
   * @param provider the provider
   */
  VariableOrdering(final Class<? extends VariableOrderingProvider> provider) {
    this.provider = provider;
  }

  /**
   * Returns the provider for this variable ordering.
   * @return the provider for this variable ordering
   */
  public Class<? extends VariableOrderingProvider> provider() {
    return this.provider;
  }
}
