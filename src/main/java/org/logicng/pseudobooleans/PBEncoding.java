package org.logicng.pseudobooleans;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.List;

/**
 * The interface for pseudo Boolean constraint encodings.
 * @version 1.1
 * @since 1.1
 */
public interface PBEncoding {

  /**
   * Encodes a pseudo-Boolean constraint and returns its CNF encoding.
   * @param lits   the literals of the constraint
   * @param coeffs the coefficients of the constraint
   * @param rhs    the right hand side of the constraint
   * @param result the current result CNF
   * @return the CNF encoding of the constraint
   */
  List<Formula> encode(final LNGVector<Literal> lits, final LNGIntVector coeffs, int rhs, final List<Formula> result);

}
