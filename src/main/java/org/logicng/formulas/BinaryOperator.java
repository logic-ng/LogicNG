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
//  Copyright 2015 Christoph Zengler                                     //
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

package org.logicng.formulas;

import org.logicng.datastructures.Substitution;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Super class for Boolean binary operators.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public abstract class BinaryOperator extends Formula {

  protected final Formula left;
  protected final Formula right;
  protected volatile int hashCode;

  /**
   * Constructor.
   * @param type  the type of the formula
   * @param left  the left-hand side operand
   * @param right the right-hand side operand
   * @param f     the factory which created this instance
   */
  BinaryOperator(final FType type, final Formula left, final Formula right, final FormulaFactory f) {
    super(type, f);
    this.left = left;
    this.right = right;
    this.hashCode = 0;
  }

  /**
   * Returns the left-hand side operator.
   * @return the left-hand side operator
   */
  public Formula left() {
    return this.left;
  }

  @Override
  public long numberOfAtoms() {
    if (this.numberOfAtoms != -1)
      return this.numberOfAtoms;
    this.numberOfAtoms = this.left.numberOfAtoms() + this.right.numberOfAtoms();
    return this.numberOfAtoms;
  }

  @Override
  public long numberOfNodes() {
    if (this.numberOfNodes != -1)
      return this.numberOfNodes;
    this.numberOfNodes = this.left.numberOfNodes() + this.right.numberOfNodes() + 1;
    return this.numberOfNodes;
  }

  @Override
  public int numberOfOperands() {
    return 2;
  }

  @Override
  protected void varProfileRec(SortedMap<Literal, Integer> map) {
    this.left.varProfileRec(map);
    this.right.varProfileRec(map);
  }

  @Override
  protected void litProfileRec(SortedMap<Literal, Integer> map) {
    this.left.litProfileRec(map);
    this.right.litProfileRec(map);
  }

  @Override
  public LinkedHashSet<Formula> subformulas() {
    final LinkedHashSet<Formula> set = new LinkedHashSet<>();
    set.addAll(this.left.subformulas());
    set.addAll(this.right.subformulas());
    set.add(this);
    return set;
  }

  @Override
  public SortedSet<Literal> variables() {
    if (this.variables == null) {
      final SortedSet<Literal> set = new TreeSet<>();
      set.addAll(this.left.variables());
      set.addAll(this.right.variables());
      this.variables = set;
    }
    return this.variables;
  }

  @Override
  public SortedSet<Literal> literals() {
    final SortedSet<Literal> set = new TreeSet<>();
    set.addAll(this.left.literals());
    set.addAll(this.right.literals());
    return set;
  }

  @Override
  public boolean contains(final Literal literal) {
    return this.left.contains(literal) || this.right.contains(literal);
  }

  @Override
  public boolean containsSubformula(final Formula formula) {
    return this == formula || this.equals(formula) || this.left.containsSubformula(formula) || this.right.containsSubformula(formula);
  }

  @Override
  public Formula substitute(final Substitution substitution) {
    return f.binaryOperator(type, this.left.substitute(substitution), this.right.substitute(substitution));
  }

  @Override
  public Formula negate() {
    return f.not(this);
  }

  /**
   * Generates a dot file string for a binary operator.
   * @param sb         the string builder for the dot file
   * @param ids        the mapping from formulas to node ids
   * @param op         the string representation of the binary operator
   * @param directions indicates whether left/right should be annotated to the edge
   */
  protected void generateDotString(StringBuilder sb, Map<Formula, Integer> ids, final String op, boolean directions) {
    if (!ids.containsKey(this.left))
      this.left.generateDotString(sb, ids);
    if (!ids.containsKey(this.right))
      this.right.generateDotString(sb, ids);
    final int id = ids.size();
    ids.put(this, id);
    sb.append("  id").append(id).append(" [label=\"").append(op).append("\"];\n");
    sb.append("  id").append(id).append(" -> id").append(ids.get(this.left));
    sb.append(directions ? " [label=\"l\"];\n" : ";\n");
    sb.append("  id").append(id).append(" -> id").append(ids.get(this.right));
    sb.append(directions ? " [label=\"r\"];\n" : ";\n");
  }

  /**
   * Returns the right-hand side operator.
   * @return the right-hand side operator
   */
  public Formula right() {
    return this.right;
  }

  @Override
  public Iterator<Formula> iterator() {
    return new Iterator<Formula>() {
      private int count;

      @Override
      public boolean hasNext() {
        return count < 2;
      }

      @Override
      public Formula next() {
        if (count == 0) {
          count++;
          return left;
        } else if (count == 1) {
          count++;
          return right;
        }
        throw new NoSuchElementException();
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }
}
