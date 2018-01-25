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

package org.logicng.util;

/**
 * Data structure for a comparable pair.
 * @param <A> the type parameter of the first entry
 * @param <B> the type parameter of the second entry
 * @version 1.2
 * @since 1.2
 */
public final class ComparablePair<A extends Comparable<A>, B extends Comparable<B>> extends Pair<A, B> implements Comparable<ComparablePair<A, B>> {

  /**
   * Constructs a new comparable pair.
   * @param a the first entry
   * @param b the second entry
   */
  public ComparablePair(final A a, final B b) {
    super(a, b);
  }

  @Override
  public int compareTo(final ComparablePair<A, B> o) {
    int compare = this.a.compareTo(o.a);
    return compare != 0 ? compare : this.b.compareTo(o.b);
  }
}
