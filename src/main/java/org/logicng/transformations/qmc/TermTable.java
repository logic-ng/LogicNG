///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-2018 Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.transformations.qmc;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;

/**
 * A term table in the Quine–McCluskey algorithm.  The term table is used
 * for the line and column dominance checks.
 * @version 1.4.0
 * @since 1.4.0
 */
class TermTable {

  private final Vector<Formula> columnHeaders;
  private final Vector<Term> lineHeaders;
  private final Vector<Vector<Boolean>> matrixLines;
  private final Vector<Vector<Boolean>> matrixColumns;

  /**
   * Constructs a prime term table from a given list of terms.
   * @param terms the terms
   */
  TermTable(final LinkedHashSet<Term> terms) {
    this.lineHeaders = new Vector<>(terms.size());
    this.columnHeaders = initializeColumnHeaders(terms);
    this.matrixLines = new Vector<>(terms.size());
    this.matrixColumns = new Vector<>(this.columnHeaders.size());
    final Vector<Boolean> matrixLineTemplate = new Vector<>(this.columnHeaders.size());
    for (int i = 0; i < this.columnHeaders.size(); i++)
      matrixLineTemplate.add(false);
    final Vector<Boolean> matrixColumnTemplate = new Vector<>(terms.size());
    for (int i = 0; i < terms.size(); i++)
      matrixColumnTemplate.add(false);
    for (int i = 0; i < this.columnHeaders.size(); i++)
      this.matrixColumns.add(new Vector<>(matrixColumnTemplate));
    int count = 0;
    for (final Term term : terms) {
      this.lineHeaders.add(term);
      final Vector<Boolean> matrixLine = new Vector<>(matrixLineTemplate);
      this.matrixLines.add(matrixLine);
      for (final Formula minterm : term.minterms()) {
        final int index = this.columnHeaders.indexOf(minterm);
        matrixLine.setElementAt(true, index);
        this.matrixColumns.get(index).setElementAt(true, count);
      }
      count++;
    }
  }

  /**
   * Initialize the column headers with the minterms
   * @param terms the terms
   * @return the column headers
   */
  private Vector<Formula> initializeColumnHeaders(final LinkedHashSet<Term> terms) {
    final LinkedHashSet<Formula> header = new LinkedHashSet<>();
    for (final Term term : terms)
      header.addAll(term.minterms());
    return new Vector<>(header);
  }

  /**
   * Simplifies this table by using column and line dominance.
   */
  void simplifyTableByDominance() {
    boolean changed;
    do {
      final boolean eliminatedColumns = eliminateColumnDominance();
      final boolean eliminatedLines = eliminateLineDominance();
      changed = eliminatedColumns || eliminatedLines;
    } while (changed);
  }

  /**
   * Performs elimination of columns by dominance.
   * @return {@code true} if a elimination was performed, {@code false} otherwise
   */
  private boolean eliminateColumnDominance() {
    final SortedSet<Integer> toEliminate = new TreeSet<>();
    for (int i = 0; i < this.matrixColumns.size(); i++)
      for (int j = i + 1; j < this.matrixColumns.size(); j++) {
        if (isSubsetOf(this.matrixColumns.get(i), this.matrixColumns.get(j)))
          toEliminate.add(j);
        else if (isSubsetOf(this.matrixColumns.get(j), this.matrixColumns.get(i)))
          toEliminate.add(i);
      }
    int count = 0;
    for (final Integer toDelete : toEliminate)
      deleteColumn(toDelete - count++);
    return !toEliminate.isEmpty();
  }

  /**
   * Performs elimination of lines by dominance.
   * @return {@code true} if a elimination was performed, {@code false} otherwise
   */
  private boolean eliminateLineDominance() {
    final SortedSet<Integer> toEliminate = new TreeSet<>();
    for (int i = 0; i < this.matrixLines.size(); i++)
      for (int j = i + 1; j < this.matrixLines.size(); j++) {
        if (isSubsetOf(this.matrixLines.get(i), this.matrixLines.get(j)))
          toEliminate.add(i);
        else if (isSubsetOf(this.matrixLines.get(j), this.matrixLines.get(i)))
          toEliminate.add(j);
      }
    int count = 0;
    for (final Integer toDelete : toEliminate)
      deleteLine(toDelete - count++);
    return !toEliminate.isEmpty();
  }

  /**
   * Returns whether vec1 is a subset of vec2.
   * @param vec1 the first vector
   * @param vec2 the second vector
   * @return {@code true} if vec1 is a subset of vec2, {@code false} otherwise
   */
  static boolean isSubsetOf(final Vector<Boolean> vec1, final Vector<Boolean> vec2) {
    for (int i = 0; i < vec1.size(); i++)
      if (vec1.get(i) && !vec2.get(i))
        return false;
    return true;
  }

  /**
   * Deletes a column at a given index.
   * @param colIndex the index
   */
  private void deleteColumn(final int colIndex) {
    this.columnHeaders.removeElementAt(colIndex);
    this.matrixColumns.removeElementAt(colIndex);
    for (final Vector<Boolean> line : this.matrixLines)
      line.removeElementAt(colIndex);
  }

  /**
   * Deletes a line at a given index.
   * @param lineIndex the index
   */
  private void deleteLine(final int lineIndex) {
    this.lineHeaders.removeElementAt(lineIndex);
    this.matrixLines.removeElementAt(lineIndex);
    for (final Vector<Boolean> column : this.matrixColumns)
      column.removeElementAt(lineIndex);
  }

  /**
   * Returns the lines of this table.
   * @return the lines of this table
   */
  Vector<Vector<Boolean>> lines() {
    return this.matrixLines;
  }

  /**
   * Returns the columns of this table.
   * @return the columns of this table
   */
  Vector<Vector<Boolean>> columns() {
    return this.matrixColumns;
  }

  /**
   * Returns the column headers of this table.
   * @return the column headers of this table
   */
  Vector<Formula> columnHeaders() {
    return this.columnHeaders;
  }

  /**
   * Returns the line headers of this table.
   * @return the line headers of this table
   */
  Vector<Term> lineHeaders() {
    return this.lineHeaders;
  }

  @Override
  public String toString() {
    final Vector<String> lineHeaderStrings = new Vector<>();
    for (final Term header : this.lineHeaders)
      lineHeaderStrings.add(formatBits(header.bits()));
    final int lineHeaderSize = lineHeaderStrings.firstElement().length();
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < lineHeaderSize; i++)
      sb.append(" ");
    sb.append(" | ");
    final LinkedHashMap<String, String> legend = new LinkedHashMap<>();
    int columnSize = 0;
    for (int i = 0; i < this.columnHeaders.size(); i++) {
      final String key = "m" + i;
      legend.put(key, this.columnHeaders.get(i).toString());
      if (key.length() > columnSize)
        columnSize = key.length();
    }
    for (final String s : legend.keySet())
      sb.append(String.format("%s | ", padRight(s, columnSize)));
    sb.append(String.format("%n"));

    for (int i = 0; i < this.matrixLines.size(); i++) {
      sb.append(String.format("%s | %s%n", lineHeaderStrings.get(i), formatMatrixLine(this.matrixLines.get(i), columnSize)));
    }
    return sb.toString();
  }

  private String formatMatrixLine(final Vector<Boolean> booleans, final int size) {
    final StringBuilder sb = new StringBuilder();
    for (final Boolean entry : booleans)
      sb.append(padRight((entry ? "X" : " "), size)).append(" | ");
    return sb.toString();
  }

  private String formatBits(final Tristate[] bits) {
    final StringBuilder sb = new StringBuilder();
    sb.append("[");
    for (int i = 0; i < bits.length; i++) {
      if (bits[i] == Tristate.TRUE)
        sb.append("1");
      else if (bits[i] == Tristate.FALSE)
        sb.append("0");
      else sb.append("-");
      if (i < bits.length - 1)
        sb.append(", ");
    }
    sb.append("]");
    return sb.toString();
  }

  private static String padRight(final String s, final int n) {
    return String.format("%1$-" + n + "s", s);
  }
}
