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

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * Interface for a handler for MaxSAT solvers.
 * @version 1.0
 * @since 1.0
 */
public interface MaxSATHandler {

  /**
   * Returns a SAT handler which can be used to cancel internal SAT calls of the MaxSAT solver.
   * @return a SAT handler
   */
  SATHandler satHandler();

  /**
   * This method is called when the MaxSAT solver found a new lower bound for a solution.
   * @param lowerBound the cost of the lower bound
   * @param model      the model of the lower bound, may be null if not applicable
   * @return whether solving process should be continued or not
   */
  boolean foundLowerBound(final int lowerBound, final Assignment model);

  /**
   * This method is called when the MaxSAT solver found a new upper bound for a solution.
   * @param upperBound the cost of the upper bound
   * @param model      the model of the upper bound, may be null if not applicable
   * @return whether solving process should be continued or not
   */
  boolean foundUpperBound(final int upperBound, final Assignment model);

  /**
   * This method is called when the MaxSAT solver starts solving.
   */
  void startedSolving();

  /**
   * This method is called when the MaxSAT solver finished solving.
   */
  void finishedSolving();

  /**
   * Returns the last approximation of the result or -1 if there is no approximation for the lower bound.
   * If the handler does not cancel the solving process, it is not guaranteed that this
   * method will return the optimal result. Use the respective method of the MaxSAT solver instead.
   * @return the last approximation of the result or -1 if no approximation is known
   */
  int lowerBoundApproximation();

  /**
   * Returns the last approximation of the result or -1 if there is no approximation for the lower bound.
   * If the handler does not cancel the solving process, it is not guaranteed that this
   * method will return the optimal result. Use the respective method of the MaxSAT solver instead.
   * @return the last approximation of the result or -1 if no approximation is known
   */
  int upperBoundApproximation();
}
