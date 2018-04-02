package org.logicng.io.parsers;

import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

/**
 * Abstract super class for parsers which have a formula factory and return a formula.
 * @version 1.4.0
 * @since 1.4.0
 */
public abstract class ParserWithFormula extends Parser {

  protected FormulaFactory f;

  /**
   * Constructor.
   * @param input the token stream (e.g. a lexer) for the parser
   */
  public ParserWithFormula(final TokenStream input) {
    super(input);
  }

  /**
   * Sets the LogicNG formula factory for this parser
   * @param f the LogicNG formula factory
   */
  public void setFormulaFactory(final FormulaFactory f) {
    this.f = f;
  }

  /**
   * Returns the parsed formula.
   * @return the parsed formula
   */
  public abstract Formula getParsedFormula();
}
