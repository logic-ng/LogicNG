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

package org.logicng.collections;

import java.util.Arrays;

/**
 * A simple vector for double elements implementation (inspired by MiniSat, CleaneLing, Sat4J).
 * <p>
 * In theory one could use the {@link LNGVector} also for doubles.  But Java's auto-boxing comes with such a large
 * performance penalty that for the mission critical data structures of the SAT solvers we use this specialized
 * implementation.
 * @version 1.1
 * @since 1.0
 */
public final class LNGDoubleVector {

  private double[] elements;
  private int size;

  /**
   * Creates a vector with an initial capacity of 5 elements.
   */
  public LNGDoubleVector() {
    this(5);
  }

  /**
   * Creates a vector with a given capacity.
   * @param size the capacity of the vector.
   */
  public LNGDoubleVector(int size) {
    this.elements = new double[size];
  }

  /**
   * Creates a vector with a given capacity and a given initial element.
   * @param size the capacity of the vector
   * @param pad  the initial element
   */
  public LNGDoubleVector(int size, double pad) {
    this.elements = new double[size];
    Arrays.fill(this.elements, pad);
    this.size = size;
  }

  /**
   * Copy constructor.
   * @param other the other byte vector.
   */
  public LNGDoubleVector(final LNGDoubleVector other) {
    this.elements = Arrays.copyOf(other.elements, other.size);
    this.size = other.size;
  }

  /**
   * Creates a vector with the given elements.
   * @param elems the elements
   */
  public LNGDoubleVector(final double... elems) {
    this.elements = Arrays.copyOf(elems, elems.length);
    this.size = elems.length;
  }

  /**
   * Returns whether the vector is empty or not.
   * @return {@code true} if the vector is empty, {@code false} otherwise
   */
  public boolean empty() {
    return this.size == 0;
  }

  /**
   * Returns the size of the vector.
   * @return the size of the vector
   */
  public int size() {
    return this.size;
  }

  /**
   * Returns the last element of the vector and leaves it on the vector.
   * @return the last element of the vector
   */
  public double back() {
    return this.elements[this.size - 1];
  }

  /**
   * Pushes an element at the end of the vector.
   * @param element the element to push
   */
  public void push(final double element) {
    int newSize = this.size + 1;
    this.ensure(newSize);
    this.elements[this.size++] = element;
  }

  /**
   * Pushes an element and assumes that there is enough space on the vector.
   * @param element the element to push
   * @throws ArrayIndexOutOfBoundsException if there was not enough space on the vector
   */
  public void unsafePush(final double element) {
    this.elements[this.size++] = element;
  }

  /**
   * Returns the element at a given position in the vector.
   * @param position the position
   * @return the element at the position
   * @throws ArrayIndexOutOfBoundsException if the position is not found in the vector
   */
  public double get(int position) {
    return this.elements[position];
  }

  /**
   * Sets an element at a given position in the vector.
   * @param position the position
   * @param element  the element
   * @throws ArrayIndexOutOfBoundsException if the position is not found in the vector
   */
  public void set(int position, double element) {
    this.elements[position] = element;
  }

  /**
   * Removes the last element of the vector.
   */
  public void pop() {
    this.elements[--this.size] = -1.0;
  }

  /**
   * Shrinks the vector to a given size.
   * @param newSize the new size
   */
  public void shrinkTo(int newSize) {
    if (newSize < this.size)
      this.size = newSize;
  }

  /**
   * Grows the vector to a new size and initializes the new elements with a given value.
   * @param size the new size
   * @param pad  the value for new elements
   */
  public void growTo(int size, double pad) {
    if (this.size >= size)
      return;
    this.ensure(size);
    for (int i = this.size; i < size; i++)
      this.elements[i] = pad;
    this.size = size;
  }

  /**
   * Removes a given number of elements from the vector.
   * @param num the number of elements to remove.
   * @throws ArrayIndexOutOfBoundsException if the number of elements to remove is larger than the size of the vector
   */
  public void removeElements(int num) {
    int count = num;
    while (count-- > 0)
      this.elements[--this.size] = -1;
  }

  /**
   * Clears the vector.
   */
  public void clear() {
    this.size = 0;
  }

  /**
   * Sorts this vector.
   */
  public void sort() {
    Arrays.sort(this.elements, 0, this.size);
  }

  /**
   * Sorts this vector in reverse order.
   */
  public void sortReverse() {
    Arrays.sort(this.elements, 0, this.size);
    for (int i = 0; i < this.size / 2; i++) {
      double temp = this.elements[i];
      this.elements[i] = this.elements[this.size - i - 1];
      this.elements[this.size - i - 1] = temp;
    }
  }

  /**
   * Returns this vector's contents as an array.
   * @return the array
   */
  public double[] toArray() {
    return Arrays.copyOf(this.elements, this.size);
  }

  /**
   * Ensures that this vector has the given size.  If not - the size is doubled and the old elements are copied.
   * @param newSize the size to ensure
   */
  private void ensure(final int newSize) {
    if (newSize >= this.elements.length) {
      final double[] newArray = new double[Math.max(newSize, this.size * 2)];
      System.arraycopy(this.elements, 0, newArray, 0, this.size);
      this.elements = newArray;
    }
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("[");
    for (int i = 0; i < this.size; i++) {
      sb.append(this.elements[i]);
      if (i != this.size - 1)
        sb.append(", ");
    }
    sb.append("]");
    return sb.toString();
  }
}
