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
//  Copyright 2015-20xx Christoph Zengler                                //
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

package org.logicng.primecomputation;

import org.logicng.formulas.Literal;

import java.util.List;
import java.util.Objects;
import java.util.SortedSet;

/**
 * Result of a prime computation.
 * <p>
 * Contains a list of prime implicants, a list of prime implicates
 * and the coverage type specifying which list is complete.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class PrimeResult {
    private final List<SortedSet<Literal>> primeImplicants;
    private final List<SortedSet<Literal>> primeImplicates;
    private final CoverageType coverageType;

    /**
     * Constructs a new prime result.
     * @param primeImplicants the list of prime implicants
     * @param primeImplicates the list of prime implicates
     * @param coverageType    the coverage type
     */
    public PrimeResult(final List<SortedSet<Literal>> primeImplicants, final List<SortedSet<Literal>> primeImplicates,
                       final CoverageType coverageType) {
        this.primeImplicants = primeImplicants;
        this.primeImplicates = primeImplicates;
        this.coverageType = coverageType;
    }

    /**
     * Returns the list of prime implicants.
     * @return the list of prime implicants
     */
    public List<SortedSet<Literal>> getPrimeImplicants() {
        return this.primeImplicants;
    }

    /**
     * Returns the list of prime implicates.
     * @return the list of prime implicates
     */
    public List<SortedSet<Literal>> getPrimeImplicates() {
        return this.primeImplicates;
    }

    /**
     * Returns the coverage type.
     * @return the coverage type
     */
    public CoverageType getCoverageType() {
        return this.coverageType;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final PrimeResult that = (PrimeResult) o;
        return Objects.equals(this.primeImplicants, that.primeImplicants) &&
                Objects.equals(this.primeImplicates, that.primeImplicates) &&
                this.coverageType == that.coverageType;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.primeImplicants, this.primeImplicates, this.coverageType);
    }

    @Override
    public String toString() {
        return "PrimeResult{" +
                "primeImplicants=" + this.primeImplicants +
                ", primeImplicates=" + this.primeImplicates +
                ", coverageInfo=" + this.coverageType +
                '}';
    }

    /**
     * The coverage type.
     */
    public enum CoverageType {
        /**
         * Indicates that the set of prime implicants is complete,
         * i.e. there are no other prime implicants for the formula.
         */
        IMPLICANTS_COMPLETE,

        /**
         * Indicates that the set of prime implicates is complete,
         * i.e. there are no other prime implicates for the formula.
         */
        IMPLICATES_COMPLETE
    }
}
