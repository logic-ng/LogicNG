// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

/*========================================================================
           Copyright (C) 1996-2002 by Jorn Lind-Nielsen
                        All rights reserved

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, reproduce, prepare derivative
works, distribute, and display this software and its documentation
for any purpose, provided that (1) the above copyright notice and
the following two paragraphs appear in all copies of the source code
and (2) redistributions, including without limitation binaries,
reproduce these notices in the supporting documentation. Substantial
modifications to this software may be copyrighted by their authors
and need not follow the licensing terms described here, provided
that the new terms are clearly indicated in all files where they apply.

IN NO EVENT SHALL JORN LIND-NIELSEN, OR DISTRIBUTORS OF THIS
SOFTWARE BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL,
INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS
SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE AUTHORS OR ANY OF THE
ABOVE PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

JORN LIND-NIELSEN SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
========================================================================*/

package org.logicng.knowledgecompilation.bdds.jbuddy;

import java.util.Random;

/**
 * Prime number calculations
 * @version 2.0.0
 * @since 1.4.0
 */
public final class BDDPrime {

    static final int CHECKTIMES = 20;
    private final Random rng;

    /**
     * Private constructor.
     */
    public BDDPrime() {
        this.rng = new Random();
    }

    /**
     * Returns the next prime greater than the given number.
     * @param num the number
     * @return the next prime greater than the given number
     */
    public int primeGTE(int num) {
        if (isEven(num)) {
            ++num;
        }
        while (!isPrime(num)) {
            num += 2;
        }
        return num;
    }

    /**
     * Returns the next prime less than the given number.
     * @param num the number
     * @return the next prime less than the given number
     */
    public int primeLTE(int num) {
        if (isEven(num)) {
            --num;
        }
        while (!isPrime(num)) {
            num -= 2;
        }
        return num;
    }

    static boolean isEven(final int src) {
        return (src & 0x1) == 0;
    }

    boolean isPrime(final int src) {
        return !hasEasyFactors(src) && isMillerRabinPrime(src);
    }

    static boolean hasEasyFactors(final int src) {
        return hasFactor(src, 3)
                || hasFactor(src, 5)
                || hasFactor(src, 7)
                || hasFactor(src, 11)
                || hasFactor(src, 13);
    }

    boolean isMillerRabinPrime(final int src) {
        for (int n = 0; n < CHECKTIMES; ++n) {
            final int witness = random(src - 1);
            if (isWitness(witness, src)) {
                return false;
            }
        }
        return true;
    }

    static boolean isWitness(final int witness, final int src) {
        final int bitNum = numberOfBits(src - 1) - 1;
        int d = 1;
        for (int i = bitNum; i >= 0; --i) {
            final int x = d;
            d = mulmod(d, d, src);
            if (d == 1 && x != 1 && x != src - 1) {
                return true;
            }
            if (bitIsSet(src - 1, i)) {
                d = mulmod(d, witness, src);
            }
        }
        return d != 1;
    }

    static int numberOfBits(final int src) {
        int b;
        if (src == 0) {
            return 0;
        }
        for (b = 31; b > 0; --b) {
            if (bitIsSet(src, b)) {
                return b + 1;
            }
        }
        return 1;
    }

    static boolean bitIsSet(final int src, final int b) {
        return (src & (1 << b)) != 0;
    }

    static int mulmod(final int a, final int b, final int c) {
        return (int) (((long) a * (long) b) % (long) c);
    }

    int random(final int i) {
        return this.rng.nextInt(i) + 1;
    }

    static boolean hasFactor(final int src, final int n) {
        return (src != n) && (src % n == 0);
    }
}
