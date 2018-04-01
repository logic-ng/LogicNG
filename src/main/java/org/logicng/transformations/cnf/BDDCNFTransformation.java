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

package org.logicng.transformations.cnf;

import org.logicng.bdds.BDDFactory;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.predicates.CNFPredicate;
import org.logicng.transformations.UnitPropagation;

import static org.logicng.formulas.FType.LITERAL;
import static org.logicng.formulas.cache.TransformationCacheEntry.BDD_CNF;

/**
 * Transformation of a formula in CNF by converting it to a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDCNFTransformation implements FormulaTransformation {

  private final CNFPredicate cnfPredicate = new CNFPredicate();
  private final UnitPropagation up = new UnitPropagation();
  private BDDFactory bddFactory;
  private final boolean externalFactory;
  private int numNodes;
  private int cacheSize;


  /**
   * Constructs a new BDD-based CNF transformation with a given BDD factory
   * @param factory the BDD factory
   */
  public BDDCNFTransformation(final BDDFactory factory) {
    this.bddFactory = factory;
    this.externalFactory = true;
  }

  /**
   * Constructs a new BDD-based CNF transformation with a given number of nodes and cache size for the BDD factory.
   * @param numNodes  the number of nodes
   * @param cacheSize the cache size
   */
  public BDDCNFTransformation(final int numNodes, final int cacheSize) {
    this.numNodes = numNodes;
    this.cacheSize = cacheSize;
    this.externalFactory = false;
  }

  /**
   * Constructs a new BDD-based CNF transformation and determines the node size and cache size on its own.
   */
  public BDDCNFTransformation() {
    this(0, 0);
  }

  @Override
  public Formula apply(final Formula formula, final boolean cache) {
    if (formula.type().precedence() >= LITERAL.precedence())
      return formula;
    if (formula.holds(this.cnfPredicate))
      return formula;
    final Formula cached = formula.transformationCacheEntry(BDD_CNF);
    if (cache && cached != null)
      return cached;
    if (!this.externalFactory) {
      final int numVars = formula.variables().size();
      final int nodes = this.numNodes == 0 ? numVars * 30 : this.numNodes;
      final int cacheS = this.cacheSize == 0 ? numVars * 20 : this.cacheSize;
      this.bddFactory = new BDDFactory(nodes, cacheS, formula.factory());
      this.bddFactory.setNumberOfVars(numVars);
    }
    final Formula cnf = this.bddFactory.build(formula).cnf().transform(this.up);
    if (cache)
      formula.setTransformationCacheEntry(BDD_CNF, cnf);
    return cnf;
  }
}
