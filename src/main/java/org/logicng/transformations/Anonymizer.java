package org.logicng.transformations;

import org.logicng.datastructures.Substitution;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Variable;

import static org.logicng.formulas.cache.TransformationCacheEntry.ANONYMIZATION;

/**
 * An anonymizer replaces all variables in a formula with new variables generated from a given prefix and a counter.
 * @version 1.4.0
 * @since 1.4.0
 */
public class Anonymizer implements FormulaTransformation {

  private final Substitution substitution;
  private final String prefix;
  private int counter;

  /**
   * Constructs a new anonymizer with a given prefix for the newly introduced variables.
   * @param prefix       the prefix for the new variables
   * @param startCounter where should the counter start
   */
  public Anonymizer(final String prefix, final int startCounter) {
    this.prefix = prefix;
    this.substitution = new Substitution();
    this.counter = startCounter;
  }

  /**
   * Constructs a new anonymizer with a given prefix for the newly introduced variables.
   * @param prefix the prefix for the new variables
   */
  public Anonymizer(final String prefix) {
    this(prefix, 0);
  }

  /**
   * Constructs a new anonymizer with the standard variable prefix 'v'.
   */
  public Anonymizer() {
    this("v");
  }

  @Override
  public Formula apply(final Formula formula, final boolean cache) {
    if (formula.variables().isEmpty())
      return formula;
    final Formula cached = formula.transformationCacheEntry(ANONYMIZATION);
    if (cache && cached != null)
      return cached;
    for (final Variable variable : formula.variables())
      if (this.substitution.getSubstitution(variable) == null)
        this.substitution.addMapping(variable, formula.factory().variable(this.prefix + this.counter++));
    final Formula transformed = formula.substitute(this.substitution);
    if (cache)
      formula.setTransformationCacheEntry(ANONYMIZATION, transformed);
    return transformed;
  }
}
