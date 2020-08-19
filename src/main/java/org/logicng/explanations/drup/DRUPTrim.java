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

/*
 * Copyright (c) 2014-2015, Marijn Heule and Nathan Wetzler
 * Last edit, March 4, 2015
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.explanations.drup;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the DRUP-trim tool to check satisfiability proofs and perform trimming.
 * @version 1.3
 * @since 1.3
 */
public final class DRUPTrim {

    private static final int BIGINIT = 1000000;
    private static final int UNSAT = 0;
    private static final int SAT = 1;
    private static final int EXTRA = 2;
    private static final int MARK = 3;

    private static int getHash(final int[] _marks, final int mark, final LNGIntVector input) {
        int sum = 0;
        int xor = 0;
        int prod = 1;
        for (int i = 0; i < input.size(); i++) {
            prod *= input.get(i);
            sum += input.get(i);
            xor ^= input.get(i);
            _marks[index(input.get(i))] = mark;
        }
        return Math.abs((1023 * sum + prod ^ (31 * xor)) % BIGINIT);
    }

    private static int index(final int lit) {
        return lit > 0 ? lit * 2 : (-lit * 2) ^ 1;
    }

    /**
     * Computes the DRUP result for a given problem in terms of original clauses and the generated proof.
     * @param originalProblem the clauses of the original problem
     * @param proof           the clauses of the proof
     * @return the result of the DRUP execution from which the UNSAT core can be generated
     */
    public DRUPResult compute(final LNGVector<LNGIntVector> originalProblem, final LNGVector<LNGIntVector> proof) {
        final DRUPResult result = new DRUPResult();
        final Solver s = new Solver(originalProblem, proof);
        final boolean parseReturnValue = s.parse();
        if (!parseReturnValue) {
            result.trivialUnsat = true;
            result.unsatCore = new LNGVector<>();
        } else {
            result.trivialUnsat = false;
            result.unsatCore = s.verify();
        }
        return result;
    }

    private static class Solver {

        private final LNGVector<LNGIntVector> originalProblem;
        private final LNGVector<LNGIntVector> proof;
        private final LNGVector<LNGIntVector> core;
        private final boolean delete;
        private LNGIntVector DB;
        private int nVars;
        private int nClauses;
        private int[] falseStack;
        private int[] reason;
        private int[] internalFalse;
        private int forcedPtr;
        private int processedPtr;
        private int assignedPtr;
        private LNGIntVector adlist;
        private LNGIntVector[] wlist;
        private int count;
        private int adlemmas;
        private int lemmas;
        private int time;

        private Solver(final LNGVector<LNGIntVector> originalProblem, final LNGVector<LNGIntVector> proof) {
            this.originalProblem = originalProblem;
            this.proof = proof;
            this.core = new LNGVector<>();
            this.delete = true;
        }

        private void assign(final int a) {
            this.internalFalse[index(-a)] = 1;
            this.falseStack[this.assignedPtr++] = -a;
        }

        private void addWatch(final int cPtr, final int index) {
            final int lit = this.DB.get(cPtr + index);
            this.wlist[index(lit)].push((cPtr << 1));
        }

        private void addWatchLit(final int l, final int m) {
            this.wlist[index(l)].push(m);
        }

        private void removeWatch(final int cPtr, final int index) {
            final int lit = this.DB.get(cPtr + index);
            final LNGIntVector watch = this.wlist[index(lit)];
            int watchPtr = 0;
            while (true) {
                final int _cPtr = watch.get(watchPtr++) >> 1;
                if (_cPtr == cPtr) {
                    watch.set(watchPtr - 1, this.wlist[index(lit)].back());
                    this.wlist[index(lit)].pop();
                    return;
                }
            }
        }

        private void markWatch(final int clausePtr, final int index, final int offset) {
            final LNGIntVector watch = this.wlist[index(this.DB.get(clausePtr + index))];
            final int clause = this.DB.get(clausePtr - offset - 1);
            int watchPtr = 0;
            while (true) {
                final int _clause = (this.DB.get((watch.get(watchPtr++) >> 1) - 1));
                if (_clause == clause) {
                    watch.set(watchPtr - 1, watch.get(watchPtr - 1) | 1);
                    return;
                }
            }
        }

        private void markClause(int clausePtr, final int index) {
            if ((this.DB.get(clausePtr + index - 1) & 1) == 0) {
                this.DB.set(clausePtr + index - 1, this.DB.get(clausePtr + index - 1) | 1);
                if (this.DB.get(clausePtr + 1 + index) == 0) {
                    return;
                }
                this.markWatch(clausePtr, index, -index);
                this.markWatch(clausePtr, 1 + index, -index);
            }
            while (this.DB.get(clausePtr) != 0) {
                this.internalFalse[index(this.DB.get(clausePtr++))] = MARK;
            }
        }

        private void analyze(final int clausePtr) {
            this.markClause(clausePtr, 0);
            while (this.assignedPtr > 0) {
                final int lit = this.falseStack[--this.assignedPtr];
                if ((this.internalFalse[index(lit)] == MARK) && this.reason[Math.abs(lit)] != 0) {
                    this.markClause(this.reason[Math.abs(lit)], -1);
                }
                this.internalFalse[index(lit)] = this.assignedPtr < this.forcedPtr ? 1 : 0;
            }
            this.processedPtr = this.forcedPtr;
            this.assignedPtr = this.forcedPtr;
        }

        private int propagate() {
            final int[] start = new int[2];
            int check = 0;
            int i;
            int lit;
            int _lit = 0;
            LNGIntVector watch;
            int _watchPtr = 0;
            start[0] = this.processedPtr;
            start[1] = this.processedPtr;
            boolean gotoFlipCheck = true;
            while (gotoFlipCheck) {
                gotoFlipCheck = false;
                check ^= 1;
                while (!gotoFlipCheck && start[check] < this.assignedPtr) { // While unprocessed false literals
                    lit = this.falseStack[start[check]++]; // Get first unprocessed literal
                    watch = this.wlist[index(lit)]; // Obtain the first watch pointer
                    int watchPtr = lit == _lit ? _watchPtr : 0;

                    while (watchPtr < watch.size()) { // While there are watched clauses (watched by lit)
                        if ((watch.get(watchPtr) & 1) != check) {
                            watchPtr++;
                            continue;
                        }
                        final int clausePtr = watch.get(watchPtr) / 2; // Get the clause from DB
                        if (this.internalFalse[index(-this.DB.get(clausePtr))] != 0 || this.internalFalse[index(-this.DB.get(clausePtr + 1))] != 0) {
                            watchPtr++;
                            continue;
                        }
                        if (this.DB.get(clausePtr) == lit) {
                            this.DB.set(clausePtr, this.DB.get(clausePtr + 1)); // Ensure that the other watched literal is in front
                        }
                        boolean gotoNextClause = false;
                        for (i = 2; this.DB.get(clausePtr + i) != 0; i++) { // Scan the non-watched literals
                            if (this.internalFalse[index(this.DB.get(clausePtr + i))] == 0) { // When clause[j] is not false, it is either true or unset
                                this.DB.set(clausePtr + 1, this.DB.get(clausePtr + i));
                                this.DB.set(clausePtr + i, lit); // Swap literals
                                this.addWatchLit(this.DB.get(clausePtr + 1), watch.get(watchPtr)); // Add the watch to the list of clause[1]
                                watch.set(watchPtr, this.wlist[index(lit)].back()); // Remove pointer
                                this.wlist[index(lit)].pop();
                                gotoNextClause = true;
                                break;
                            } // go to the next watched clause
                        }
                        if (!gotoNextClause) {
                            this.DB.set(clausePtr + 1, lit);
                            watchPtr++; // Set lit at clause[1] and set next watch
                            if (this.internalFalse[index(this.DB.get(clausePtr))] == 0) { // If the other watched literal is falsified,
                                this.assign(this.DB.get(clausePtr)); // A unit clause is found, and the reason is set
                                this.reason[Math.abs(this.DB.get(clausePtr))] = clausePtr + 1;
                                if (check == 0) {
                                    start[0]--;
                                    _lit = lit;
                                    _watchPtr = watchPtr;
                                    gotoFlipCheck = true;
                                    break;
                                }
                            } else {
                                this.analyze(clausePtr);
                                return UNSAT;
                            } // Found a root level conflict -> UNSAT
                        }
                    }
                } // Set position for next clause
                if (check != 0) {
                    gotoFlipCheck = true;
                }
            }
            this.processedPtr = this.assignedPtr;
            return SAT;
        }

        int matchClause(final LNGIntVector clauselist, final int[] _marks, final int mark, final LNGIntVector input) {
            int i;
            int matchsize;
            for (i = 0; i < clauselist.size(); i++) {
                matchsize = 0;
                boolean aborted = false;
                for (int l = clauselist.get(i); this.DB.get(l) != 0; l++) {
                    if (_marks[index(this.DB.get(l))] != mark) {
                        aborted = true;
                        break;
                    }
                    matchsize++;
                }
                if (!aborted && input.size() == matchsize) {
                    final int result = clauselist.get(i);
                    clauselist.set(i, clauselist.back());
                    return result;
                }
            }
            throw new IllegalStateException("Could not match deleted clause");
        }

        /**
         * Parses the input and returns {@code true} if further processing is required and {@code false} if the formula is
         * trivially UNSAT.
         * @return {@code true} if further processing is required and {@code false} if the formula is trivially UNSAT
         */
        private boolean parse() {
            this.nVars = 0;
            for (final LNGIntVector vector : this.originalProblem) {
                for (int i = 0; i < vector.size(); i++) {
                    if (Math.abs(vector.get(i)) > this.nVars) {
                        this.nVars = Math.abs(vector.get(i));
                    }
                }
            }
            this.nClauses = this.originalProblem.size();

            boolean del = false;
            int nZeros = this.nClauses;
            final LNGIntVector buffer = new LNGIntVector();

            this.DB = new LNGIntVector();

            this.count = 1;
            this.falseStack = new int[this.nVars + 1];
            this.reason = new int[this.nVars + 1];
            this.internalFalse = new int[2 * this.nVars + 3];

            this.wlist = new LNGIntVector[2 * this.nVars + 3];
            for (int i = 1; i <= this.nVars; ++i) {
                this.wlist[index(i)] = new LNGIntVector();
                this.wlist[index(-i)] = new LNGIntVector();
            }

            this.adlist = new LNGIntVector();

            final int[] marks = new int[2 * this.nVars + 3];
            int mark = 0;

            final Map<Integer, LNGIntVector> hashTable = new HashMap<>();
            LNGVector<LNGIntVector> currentFile = this.originalProblem;
            boolean fileSwitchFlag;
            int clauseNr = 0;
            while (true) {
                final int lit = 0;
                fileSwitchFlag = nZeros <= 0;
                final LNGIntVector clause = currentFile.get(clauseNr++);
                if (clause == null) {
                    this.lemmas = this.DB.size() + 1;
                    break;
                }
                final List<Integer> toks = new ArrayList<>(clause.size() - 1);
                if (fileSwitchFlag && clause.get(0) == -1) {
                    del = true;
                }
                for (int i = (fileSwitchFlag ? 1 : 0); i < clause.size(); i++) {
                    toks.add(clause.get(i));
                }
                for (final Integer l : toks) {
                    buffer.push(l);
                }
                if (clauseNr >= currentFile.size() && !fileSwitchFlag) {
                    fileSwitchFlag = true;
                    clauseNr = 0;
                    currentFile = this.proof;
                }
                if (clauseNr > currentFile.size() && fileSwitchFlag && !currentFile.empty()) {
                    break;
                }
                if (Math.abs(lit) > this.nVars) {
                    throw new IllegalStateException(String.format("Illegal literal %d due to max var %d", lit, this.nVars));
                }
                final int hash = getHash(marks, ++mark, buffer);
                if (del) {
                    if (this.delete) {
                        final int match = this.matchClause(hashTable.get(hash), marks, mark, buffer);
                        hashTable.get(hash).pop();
                        this.adlist.push((match << 1) + 1);
                    }
                    del = false;
                    buffer.clear();
                    continue;
                }
                final int clausePtr = this.DB.size() + 1;
                this.DB.push(2 * this.count++);
                for (int i = 0; i < buffer.size(); i++) {
                    this.DB.push(buffer.get(i));
                }
                this.DB.push(0);

                LNGIntVector vec = hashTable.get(hash);
                if (vec == null) {
                    vec = new LNGIntVector();
                    hashTable.put(hash, vec);
                }
                vec.push(clausePtr);

                this.adlist.push(clausePtr << 1);

                if (nZeros == 0) {
                    this.lemmas = clausePtr;
                    this.adlemmas = this.adlist.size() - 1;
                }
                if (nZeros > 0) {
                    if (buffer.empty() || ((buffer.size() == 1) && this.internalFalse[index(this.DB.get(clausePtr))] != 0)) {
                        return false;
                    } else if (buffer.size() == 1) {
                        if (this.internalFalse[index(-this.DB.get(clausePtr))] == 0) {
                            this.reason[Math.abs(this.DB.get(clausePtr))] = clausePtr + 1;
                            this.assign(this.DB.get(clausePtr));
                        }
                    } else {
                        this.addWatch(clausePtr, 0);
                        this.addWatch(clausePtr, 1);
                    }
                } else if (buffer.empty()) {
                    break;
                }
                buffer.clear();
                --nZeros;
            }
            return true;
        }

        private LNGVector<LNGIntVector> verify() {
            int ad;
            long d;
            boolean flag = false;
            int clausePtr = 0;
            int lemmasPtr = this.lemmas;
            final int lastPtr = this.lemmas;
            final int endPtr = this.lemmas;
            int checked = this.adlemmas;
            final LNGIntVector buffer = new LNGIntVector();
            this.time = this.DB.get(lemmasPtr - 1);

            boolean gotoPostProcess = false;
            if (this.processedPtr < this.assignedPtr) {
                if (this.propagate() == UNSAT) {
                    gotoPostProcess = true;
                }
            }
            this.forcedPtr = this.processedPtr;

            if (!gotoPostProcess) {
                boolean gotoVerification = false;
                while (!gotoVerification) {
                    flag = false;
                    buffer.clear();
                    this.time = this.DB.get(lemmasPtr - 1);
                    clausePtr = lemmasPtr;
                    do {
                        ad = this.adlist.get(checked++);
                        d = ad & 1;
                        final int cPtr = ad >> 1;
                        if (d != 0 && this.DB.get(cPtr + 1) != 0) {
                            if (this.reason[Math.abs(this.DB.get(cPtr))] - 1 == ad >> 1) {
                                continue;
                            }
                            this.removeWatch(cPtr, 0);
                            this.removeWatch(cPtr, 1);
                        }
                    } while (d != 0);

                    while (this.DB.get(lemmasPtr) != 0) {
                        final int lit = this.DB.get(lemmasPtr++);
                        if (this.internalFalse[index(-lit)] != 0) {
                            flag = true;
                        }
                        if (this.internalFalse[index(lit)] == 0) {
                            if (buffer.size() <= 1) {
                                this.DB.set(lemmasPtr - 1, this.DB.get(clausePtr + buffer.size()));
                                this.DB.set(clausePtr + buffer.size(), lit);
                            }
                            buffer.push(lit);
                        }
                    }

                    if (this.DB.get(clausePtr + 1) != 0) {
                        this.addWatch(clausePtr, 0);
                        this.addWatch(clausePtr, 1);
                    }

                    lemmasPtr += EXTRA;

                    if (flag) {
                        this.adlist.set(checked - 1, 0);
                    }
                    if (flag) {
                        continue;   // Clause is already satisfied
                    }
                    if (buffer.empty()) {
                        throw new IllegalStateException("Conflict claimed, but not detected");
                    }

                    if (buffer.size() == 1) {
                        this.assign(buffer.get(0));
                        this.reason[Math.abs(buffer.get(0))] = clausePtr + 1;
                        this.forcedPtr = this.processedPtr;
                        if (this.propagate() == UNSAT) {
                            gotoVerification = true;
                        }
                    }

                    if (lemmasPtr >= this.DB.size()) {
                        break;
                    }
                }
                if (!gotoVerification) {
                    throw new IllegalStateException("No conflict");
                }

                this.forcedPtr = this.processedPtr;
                lemmasPtr = clausePtr - EXTRA;

                while (true) {
                    buffer.clear();
                    clausePtr = lemmasPtr + EXTRA;
                    do {
                        ad = this.adlist.get(--checked);
                        d = ad & 1;
                        final int cPtr = ad >> 1;
                        if (d != 0 && this.DB.get(cPtr + 1) != 0) {
                            if (this.reason[Math.abs(this.DB.get(cPtr))] - 1 == ad >> 1) {
                                continue;
                            }
                            this.addWatch(cPtr, 0);
                            this.addWatch(cPtr, 1);
                        }
                    } while (d != 0);

                    this.time = this.DB.get(clausePtr - 1);

                    if (this.DB.get(clausePtr + 1) != 0) {
                        this.removeWatch(clausePtr, 0);
                        this.removeWatch(clausePtr, 1);
                    }

                    boolean gotoNextLemma = false;
                    if (ad == 0) {
                        gotoNextLemma = true;
                    }

                    if (!gotoNextLemma) {
                        while (this.DB.get(clausePtr) != 0) {
                            final int lit = this.DB.get(clausePtr++);
                            if (this.internalFalse[index(-lit)] != 0) {
                                flag = true;
                            }
                            if (this.internalFalse[index(lit)] == 0) {
                                buffer.push(lit);
                            }
                        }

                        if (flag && buffer.size() == 1) {
                            do {
                                this.internalFalse[index(this.falseStack[--this.forcedPtr])] = 0;
                            } while (this.falseStack[this.forcedPtr] != -buffer.get(0));
                            this.processedPtr = this.forcedPtr;
                            this.assignedPtr = this.forcedPtr;
                        }

                        if ((this.time & 1) != 0) {
                            int i;
                            for (i = 0; i < buffer.size(); ++i) {
                                this.assign(-buffer.get(i));
                                this.reason[Math.abs(buffer.get(i))] = 0;
                            }
                            if (this.propagate() == SAT) {
                                throw new IllegalStateException("Formula is SAT");
                            }
                        }
                    }

                    if (lemmasPtr + EXTRA == lastPtr) {
                        break;
                    }
                    while (this.DB.get(--lemmasPtr) != 0) {
                        // empty on purpose
                    }
                }
            }

            int marked;
            lemmasPtr = 0;
            while (lemmasPtr + EXTRA <= lastPtr) {
                if ((this.DB.get(lemmasPtr++) & 1) != 0) {
                    this.count++;
                }
                while (this.DB.get(lemmasPtr++) != 0) {
                    // empty on purpose
                }
            }
            lemmasPtr = 0;

            while (lemmasPtr + EXTRA <= lastPtr) {
                final LNGIntVector coreVec = new LNGIntVector();
                marked = this.DB.get(lemmasPtr++) & 1;
                while (this.DB.get(lemmasPtr) != 0) {
                    if (marked != 0) {
                        coreVec.push(this.DB.get(lemmasPtr));
                    }
                    lemmasPtr++;
                }
                if (marked != 0) {
                    this.core.push(coreVec);
                }
                lemmasPtr++;
            }

            this.count = 0;
            while (lemmasPtr + EXTRA <= endPtr) {
                this.time = this.DB.get(lemmasPtr);
                marked = this.DB.get(lemmasPtr++) & 1;
                if (marked != 0) {
                    this.count++;
                }
                while (this.DB.get(lemmasPtr) != 0) {
                    lemmasPtr++;
                }
                lemmasPtr++;
            }
            return this.core;
        }
    }

    /**
     * The result of an DRUP execution.
     */
    public static class DRUPResult {
        private boolean trivialUnsat;
        private LNGVector<LNGIntVector> unsatCore;

        /**
         * Returns {@code true} if the formula was trivially unsatisfiable.
         * @return {@code true} if the formula was trivially unsatisfiable
         */
        public boolean trivialUnsat() {
            return this.trivialUnsat;
        }

        /**
         * Returns the unsat core of the formula.
         * @return the unsat core of the formula
         */
        public LNGVector<LNGIntVector> unsatCore() {
            return this.unsatCore;
        }
    }
}

