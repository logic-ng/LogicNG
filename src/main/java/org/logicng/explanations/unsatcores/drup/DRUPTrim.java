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

/**************************************************************************************************
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
 **************************************************************************************************/

package org.logicng.explanations.unsatcores.drup;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the DRUP-trim tool to check satisfiability proofs and perform trimming.
 * @version 1.2
 * @since 1.2
 */
public final class DRUPTrim {

  private static int BIGINIT = 1000000;
  private static int UNSAT = 0;
  private static int SAT = 1;
  private static int EXTRA = 2;
  private static int MARK = 3;
  private static int ERROR = -1;

  public LNGVector<LNGIntVector> compute(final LNGVector<LNGIntVector> originalProblem, final LNGVector<LNGIntVector> proof) {
    Solver s = new Solver(originalProblem, proof);
    //    s.setTraceFile(traceFile);
    //    s.setLemmaFile(lemmaFile);
    boolean parseReturnValue = s.parse();
    if (!parseReturnValue) {
      System.out.print("c trivial UNSAT\ns VERIFIED\n");
      return new LNGVector<>();
    } else {
      System.out.print("s VERIFIED\n");
      return s.verify();
    }
  }

  private static int getHash(int[] _marks, int mark, final LNGIntVector input) {
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

  private static int index(int lit) {
    return lit > 0 ? lit * 2 : (-lit * 2) ^ 1;
  }

  private static class Solver {

    private LNGVector<LNGIntVector> originalProblem;
    private LNGVector<LNGIntVector> proof;
    private LNGVector<LNGIntVector> core;
    //    private BufferedWriter lemmaFile;
    //    private BufferedWriter traceFile;

    LNGIntVector DB; // Alle Klauseln und Lemma immer mit führender Klausel ID (gerade) und endender 0
    int nVars; // Anzahl Variablen
    int nClauses; // Anzahl Original Klauseln

    int[] falseStack; // Belegungen mit false
    int[] reason; // Reasons für die Belegungen
    int[] _false; // falsifizierte Literale
    int forcedPtr; // pointer in falseStack
    int processedPtr; // pointer in falseStack
    int assignedPtr; // pointer in falseStack

    LNGIntVector adlist;
    LNGIntVector[] _wlist; // Watchlists zu jedem Literal

    int mask;
    boolean delete;

    int count;
    int basePtr;
    int[] delStack;
    int delinfoPtr;
    int adlemmas;
    int lemmas; // wo beginnen die Lemmas ind er DB?
    int arcs;
    int time;

    private Solver(final LNGVector<LNGIntVector> originalProblem, final LNGVector<LNGIntVector> proof) {
      this.originalProblem = originalProblem;
      this.proof = proof;
      this.core = new LNGVector<>();
      this.delete = true;
    }

    //    private void setTraceFile(final File traceFile) throws IOException {
    //      this.traceFile = new BufferedWriter(new FileWriter(traceFile));
    //    }
    //
    //    private void setLemmaFile(final File lemmaFile) throws IOException {
    //      this.lemmaFile = new BufferedWriter(new FileWriter(lemmaFile));
    //    }

    private void assign(int a) {
      //      System.out.println("ASSIGN " + a);
      this._false[index(-a)] = 1;
      this.falseStack[this.assignedPtr++] = -a;
    }

    private void addWatch(int cPtr, int index) {
      int lit = this.DB.get(cPtr + index);
      //      System.out.println("addWatch=" + lit);
      this._wlist[index(lit)].push((cPtr << 1) + this.mask);
    }

    private void addWatchLit(int l, int m) {
      //      System.out.println("addWatchLiteral l=" + l + " m=" + m);
      this._wlist[index(l)].push(m);
    }

    private void removeWatch(int cPtr, int index) {
      int lit = this.DB.get(cPtr + index);
      //      System.out.println("removeWatch=" + lit);
      //      System.out.println("    lit=" + lit);
      final LNGIntVector watch = this._wlist[index(lit)];
      int watchPtr = 0;
      while (true) {
        int _cPtr = watch.get(watchPtr++) >> 1;
        //        System.out.println("    clause=" + this.DB.get(_cPtr));
        if (_cPtr == cPtr) {
          watch.set(watchPtr - 1, this._wlist[index(lit)].back());
          this._wlist[index(lit)].pop();
          return;
        }
      }
    }

    private void markWatch(int clausePtr, int index, int offset) {
      //      System.out.println("MARK WATCH clause=" + this.DB.get(clausePtr - 1) + " index=" + index + " offset=" + offset);
      final LNGIntVector watch = this._wlist[index(this.DB.get(clausePtr + index))];
      //      System.out.println("  markWatch=" + Arrays.toString(watch));
      int clause = this.DB.get(clausePtr - offset - 1);
      int watchPtr = 0;
      while (true) {
        int _clause = (this.DB.get((watch.get(watchPtr++) >> 1) - 1));
        if (_clause == clause) {
          watch.set(watchPtr - 1, watch.get(watchPtr - 1) | 1);
          //          System.out.println("  -> " + Arrays.toString(watch));
          return;
        }
      }
    }

    private void markClause(int clausePtr, int index) {
      //      System.out.println("markClause clause=" + this.DB.get(clausePtr - 1) + " index=" + index);
      this.arcs++;
      //      if (this.traceFile != null)
      //        this.traceFile.write((this.DB.get(clausePtr + index - 1) >> 1) + " ");
      if ((this.DB.get(clausePtr + index - 1) & 1) == 0) {
        this.DB.set(clausePtr + index - 1, this.DB.get(clausePtr + index - 1) | 1);
        //        if (this.lemmaFile != null) {
        //          this.delStack[this.delinfoPtr++] = this.time;
        //          this.delStack[this.delinfoPtr++] = clausePtr + index;
        //        }
        if (this.DB.get(clausePtr + 1 + index) == 0)
          return;
        this.markWatch(clausePtr, index, -index);
        this.markWatch(clausePtr, 1 + index, -index);
      }
      while (this.DB.get(clausePtr) != 0)
        this._false[index(this.DB.get(clausePtr++))] = MARK;
    }

    private void analyze(int clausePtr) {
      //      System.out.println("ANALYZE");
      this.markClause(clausePtr, 0);
      //      System.out.println(this.DB);
      while (this.assignedPtr > 0) {
        int lit = this.falseStack[--this.assignedPtr];
        //        System.out.println("  analyse lit=" + lit);
        if ((this._false[index(lit)] == MARK) && this.reason[Math.abs(lit)] != 0)
          this.markClause(this.reason[Math.abs(lit)], -1);
        this._false[index(lit)] = this.assignedPtr < this.forcedPtr ? 1 : 0;
      }
      //      if (this.traceFile != null)
      //        this.traceFile.write("0\n");
      this.processedPtr = this.forcedPtr;
      this.assignedPtr = this.forcedPtr;
    }

    private int propagate() {
      //      System.out.println("PROPAGATE");
      int[] start = new int[2];
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
          //          System.out.println("lit=" + lit);
          watch = this._wlist[index(lit)]; // Obtain the first watch pointer
          int watchPtr = lit == _lit ? _watchPtr : 0;

          //          System.out.print("watchers=");
          //          for (int l = watchPtr; l < this._used[index(lit)]; l++)
          //            System.out.print(watch[l] + " ");
          //          System.out.println();

          while (watchPtr < watch.size()) { // While there are watched clauses (watched by lit)
            //            System.out.println("  iteration");
            if ((watch.get(watchPtr) & 1) != check) {
              //              System.out.println("    case 1");
              watchPtr++;
              continue;
            }
            int clausePtr = watch.get(watchPtr) / 2; // Get the clause from DB
            //            System.out.println("    clause=" + this.DB.get(clausePtr - 1));
            if (this._false[index(-this.DB.get(clausePtr))] != 0 || this._false[index(-this.DB.get(clausePtr + 1))] != 0) {
              //              System.out.println("    case 2");
              watchPtr++;
              continue;
            }
            if (this.DB.get(clausePtr) == lit) {
              //              System.out.println("    case 3");
              this.DB.set(clausePtr, this.DB.get(clausePtr + 1)); // Ensure that the other watched literal is in front
            }
            boolean gotoNextClause = false;
            for (i = 2; this.DB.get(clausePtr + i) != 0; i++) { // Scan the non-watched literals
              //              System.out.println("    investigate lit=" + this.DB.get(clausePtr + i));
              if (this._false[index(this.DB.get(clausePtr + i))] == 0) { // When clause[j] is not false, it is either true or unset
                //                System.out.println("    case 4");
                this.DB.set(clausePtr + 1, this.DB.get(clausePtr + i));
                this.DB.set(clausePtr + i, lit); // Swap literals
                this.addWatchLit(this.DB.get(clausePtr + 1), watch.get(watchPtr)); // Add the watch to the list of clause[1]
                watch.set(watchPtr, this._wlist[index(lit)].back()); // Remove pointer
                this._wlist[index(lit)].pop();
                gotoNextClause = true;
                break;
              } // Goto the next watched clause
            }
            if (!gotoNextClause) {
              this.DB.set(clausePtr + 1, lit);
              watchPtr++; // Set lit at clause[1] and set next watch
              if (this._false[index(this.DB.get(clausePtr))] == 0) { // If the other watched literal is falsified,
                //                System.out.println("    case 5");
                this.assign(this.DB.get(clausePtr)); // A unit clause is found, and the reason is set
                this.reason[Math.abs(this.DB.get(clausePtr))] = clausePtr + 1;
                if (check == 0) {
                  //                  System.out.println("    case 6");
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
        if (check != 0)
          gotoFlipCheck = true;
      }
      this.processedPtr = this.assignedPtr;
      return SAT;
    }

    int matchClause(final LNGIntVector clauselist, int[] _marks, int mark, final LNGIntVector input) {
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
          int result = clauselist.get(i);
          clauselist.set(i, clauselist.back());
          return result;
        }
      }
      System.out.printf("c error: could not match deleted clause ");
      for (i = 0; i < input.size(); ++i)
        System.out.printf("%d ", input.get(i));
      System.out.printf("\ns MATCHING ERROR\n");
      return ERROR;
    }

    /**
     * Parses the input and returns {@code true} if further processing is required and {@code false} if the formula is
     * trivially UNSAT.
     * @return {@code true} if further processing is required and {@code false} if the formula is trivially UNSAT
     */
    private boolean parse() {
      this.nVars = 0;
      for (LNGIntVector vector : this.originalProblem)
        for (int i = 0; i < vector.size(); i++)
          if (vector.get(i) > this.nVars)
            this.nVars = vector.get(i);
      this.nClauses = originalProblem.size();

      boolean del = false;
      int nZeros = this.nClauses;
      LNGIntVector buffer = new LNGIntVector();

      this.DB = new LNGIntVector();

      this.count = 1;
      this.falseStack = new int[this.nVars + 1];
      this.reason = new int[this.nVars + 1];
      this._false = new int[2 * this.nVars + 3];

      this._wlist = new LNGIntVector[2 * this.nVars + 3];
      for (int i = 1; i <= this.nVars; ++i) {
        this._wlist[index(i)] = new LNGIntVector();
        this._wlist[index(-i)] = new LNGIntVector();
      }

      this.adlist = new LNGIntVector();

      int[] _marks = new int[2 * this.nVars + 3];
      int mark = 0;

      final Map<Integer, LNGIntVector> hashTable = new HashMap<>();
      LNGVector<LNGIntVector> currentVector = originalProblem;
      boolean fileSwitchFlag;
      int clauseNr = 0;
      while (true) {
        int lit = 0;
        fileSwitchFlag = nZeros <= 0;
        LNGIntVector clause = currentVector.get(clauseNr++);
        final List<Integer> toks = new ArrayList<>(clause.size() - 1);
        if (fileSwitchFlag && clause.get(0) == -1) {
          del = true;
        }
        for (int i = (fileSwitchFlag ? 1 : 0); i < clause.size(); i++)
          toks.add(clause.get(i));
        for (Integer l : toks)
          buffer.push(l);
        if (clauseNr >= currentVector.size() && !fileSwitchFlag) {
          fileSwitchFlag = true;
          clauseNr = 0;
          currentVector = proof;
        }
        if (clauseNr >= currentVector.size() && fileSwitchFlag)
          break;
        if (Math.abs(lit) > this.nVars)
          throw new IllegalStateException(String.format("Illegal literal %d due to max var %d", lit, this.nVars));
        int hash = getHash(_marks, ++mark, buffer);
        if (del) {
          if (this.delete) {
            int match = this.matchClause(hashTable.get(hash), _marks, mark, buffer);
            hashTable.get(hash).pop();
            this.adlist.push((match << 1) + 1);
          }
          del = false;
          buffer.clear();
          continue;
        }
        int clausePtr = this.DB.size() + 1;
        this.DB.push(2 * this.count++);
        for (int i = 0; i < buffer.size(); i++)
          this.DB.push(buffer.get(i));
        this.DB.push(0);

        LNGIntVector vec = hashTable.get(hash);
        if (vec == null) {
          vec = new LNGIntVector();
          hashTable.put(hash, vec);
        }
        vec.push(clausePtr);

        this.adlist.push(clausePtr << 1);

        if (nZeros == this.nClauses)
          this.basePtr = clausePtr;
        if (nZeros == 0) {
          this.lemmas = clausePtr;
          this.adlemmas = this.adlist.size() - 1;
        }
        if (nZeros > 0) {
          if (buffer.empty() || ((buffer.size() == 1) && this._false[index(this.DB.get(clausePtr))] != 0))
            return false;
          else if (buffer.size() == 1) {
            if (this._false[index(-this.DB.get(clausePtr))] == 0) {
              this.reason[Math.abs(this.DB.get(clausePtr))] = clausePtr + 1;
              this.assign(this.DB.get(clausePtr));
            }
          } else {
            this.addWatch(clausePtr, 0);
            this.addWatch(clausePtr, 1);
          }
        } else if (buffer.empty())
          break;
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
      int lastPtr = this.lemmas;
      int endPtr = this.lemmas;
      int checked = this.adlemmas;
      final LNGIntVector buffer = new LNGIntVector();

      this.time = this.DB.get(lemmasPtr - 1);

      //      if (this.lemmaFile != null) {
      //        this.delStack = new int[this.count * 2];
      //        this.delinfoPtr = 0;
      //      }

      //      if (this.traceFile != null)
      //        this.traceFile.write(this.count + " 0 ");

      boolean gotoPostProcess = false;
      if (this.processedPtr < this.assignedPtr)
        if (this.propagate() == UNSAT) {
          System.out.printf("c got UNSAT propagating in the input instance\n");
          gotoPostProcess = true;
        }
      this.forcedPtr = this.processedPtr;

      if (!gotoPostProcess) {
        boolean gotoVerification = false;
        while (!gotoVerification) {
          //          System.out.println("ITERATION");
          flag = false;
          buffer.clear();
          this.time = this.DB.get(lemmasPtr - 1);
          clausePtr = lemmasPtr;
          do {
            ad = this.adlist.get(checked++);
            //            System.out.println("  ad=" + ad);
            d = ad & 1;
            int cPtr = ad >> 1;
            //            System.out.println("  c[1]=" + this.DB.get(cPtr + 1));
            if (d != 0 && this.DB.get(cPtr + 1) != 0) {
              if (this.reason[Math.abs(this.DB.get(cPtr))] - 1 == ad >> 1)
                continue;
              this.removeWatch(cPtr, 0);
              this.removeWatch(cPtr, 1);
            }
          } while (d != 0);

          while (this.DB.get(lemmasPtr) != 0) {
            int lit = this.DB.get(lemmasPtr++);
            if (this._false[index(-lit)] != 0)
              flag = true;
            if (this._false[index(lit)] == 0) {
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

          if (flag)
            this.adlist.set(checked - 1, 0);
          if (flag)
            continue;   // Clause is already satisfied
          if (buffer.empty())
            throw new IllegalStateException("Conflict claimed, but not detected");

          if (buffer.size() == 1) {
            this.assign(buffer.get(0));
            this.reason[Math.abs(buffer.get(0))] = clausePtr + 1;
            //            System.out.println("reason " + Math.abs(buffer[0]) + " = " + (clausePtr + 1));
            this.forcedPtr = this.processedPtr;
            if (this.propagate() == UNSAT)
              gotoVerification = true;
          }

          if (lemmasPtr >= this.DB.size())
            break;
        }
        if (!gotoVerification)
          throw new IllegalStateException("No conflict");

        System.out.printf("c parsed formula and detected empty clause; start verification\n");

        this.forcedPtr = this.processedPtr;
        lemmasPtr = clausePtr - EXTRA;
        //        System.out.println("lemmaPtr=" + this.DB.get(lemmasPtr));

        while (true) {
          //          System.out.println("VERIFICATION ITERATION");
          buffer.clear();
          //          System.out.println(this.DB);
          clausePtr = lemmasPtr + EXTRA;
          //          System.out.println("  clausePtr=" + this.DB.get(clausePtr));

          do {
            ad = this.adlist.get(--checked);
            //            System.out.println("    ad=" + ad);
            d = ad & 1;
            int cPtr = ad >> 1;
            if (d != 0 && this.DB.get(cPtr + 1) != 0) {
              if (this.reason[Math.abs(this.DB.get(cPtr))] - 1 == ad >> 1)
                continue;
              this.addWatch(cPtr, 0);
              this.addWatch(cPtr, 1);
            }
          } while (d != 0);

          this.time = this.DB.get(clausePtr - 1);
          //          System.out.println("  time=" + this.time);

          if (this.DB.get(clausePtr + 1) != 0) {
            this.removeWatch(clausePtr, 0);
            this.removeWatch(clausePtr, 1);
          }

          boolean gotoNextLemma = false;
          if (ad == 0)
            gotoNextLemma = true;

          if (!gotoNextLemma) {
            while (this.DB.get(clausePtr) != 0) {
              int lit = this.DB.get(clausePtr++);
              //              System.out.println("  final analyze lit=" + lit);
              if (this._false[index(-lit)] != 0)
                flag = true;
              if (this._false[index(lit)] == 0)
                buffer.push(lit);
            }

            if (flag && buffer.size() == 1) {
              do {
                this._false[index(this.falseStack[--this.forcedPtr])] = 0;
              } while (this.falseStack[this.forcedPtr] != -buffer.get(0));
              this.processedPtr = this.forcedPtr;
              this.assignedPtr = this.forcedPtr;
            }

            if ((this.time & 1) != 0) {
              int i;
              //              if (this.traceFile != null) {
              //                this.traceFile.write((this.time >> 1) + " ");
              //                for (i = 0; i < buffer.size(); i++)
              //                  this.traceFile.write(buffer.get(i) + " ");
              //                this.traceFile.write("0 ");
              //              }
              for (i = 0; i < buffer.size(); ++i) {
                this.assign(-buffer.get(i));
                this.reason[Math.abs(buffer.get(i))] = 0;
              }
              if (this.propagate() == SAT)
                throw new IllegalStateException("Formula is SAT");
            }
          }

          if (lemmasPtr + EXTRA == lastPtr)
            break;
          while (this.DB.get(--lemmasPtr) != 0) ;
        }
      }

      int marked;
      int count = 0;
      lemmasPtr = 0;
      while (lemmasPtr + EXTRA <= lastPtr) {
        if ((this.DB.get(lemmasPtr++) & 1) != 0)
          count++;
        while (this.DB.get(lemmasPtr++) != 0) ;
      }
      System.out.printf("c %d of %d clauses in core\n", count, this.nClauses);

      lemmasPtr = 0;

      while (lemmasPtr + EXTRA <= lastPtr) {
        final LNGIntVector coreVec = new LNGIntVector();
        marked = this.DB.get(lemmasPtr++) & 1;
        while (this.DB.get(lemmasPtr) != 0) {
          if (marked != 0)
            coreVec.push(this.DB.get(lemmasPtr));
          lemmasPtr++;
        }
        if (marked != 0)
          this.core.push(coreVec);
        lemmasPtr++;
      }

      //      if (this.lemmaFile != null)
      //        this.delinfoPtr -= 2;
      int lcount = 0;
      count = 0;
      while (lemmasPtr + EXTRA <= endPtr) {
        lcount++;
        this.time = this.DB.get(lemmasPtr);
        marked = this.DB.get(lemmasPtr++) & 1;
        if (marked != 0)
          count++;
        while (this.DB.get(lemmasPtr) != 0) {
          //          if (marked != 0 && this.lemmaFile != null)
          //            this.lemmaFile.write(this.DB.get(lemmasPtr) + " ");
          lemmasPtr++;
        }
        lemmasPtr++;

        //        if (this.lemmaFile == null)
        //        continue;
        //        if (marked != 0)
        //          this.lemmaFile.write("0\n");
        //        while (this.delStack[this.delinfoPtr] == this.time) {
        //          clausePtr = this.delStack[this.delinfoPtr + 1];
        //          this.lemmaFile.write("d ");
        //          while (this.DB.get(clausePtr) != 0)
        //            this.lemmaFile.write(this.DB.get(clausePtr++) + " ");
        //          this.lemmaFile.write("0\n");
        //          this.delinfoPtr -= 2;
        //        }
      }
      System.out.printf("c %d of %d lemmas in core using %d resolution steps\n", count, lcount, this.arcs);

      //      if (this.traceFile != null) {
      //        lemmasPtr = 0;
      //        while (lemmasPtr + EXTRA <= lastPtr) {
      //          marked = this.DB.get(lemmasPtr++) & 1;
      //          if (marked != 0)
      //            this.traceFile.write((this.DB.get(lemmasPtr - 1) >> 1) + " ");
      //          while (this.DB.get(lemmasPtr) != 0) {
      //            if (marked != 0)
      //              this.traceFile.write(this.DB.get(lemmasPtr) + " ");
      //            lemmasPtr++;
      //          }
      //          if (marked != 0)
      //            this.traceFile.write("0 0\n");
      //          lemmasPtr++;
      //        }
      //        this.traceFile.close();
      //      }

      return this.core;
    }
  }
}

