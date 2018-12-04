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

package org.logicng.bdds.io;

import org.logicng.bdds.BDDFactory;
import org.logicng.bdds.datastructures.BDD;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

/**
 * A dot file writer for BDDs.  Writes the internal data structure of a BDD to a dot file.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDDotFileWriter {

  /**
   * Private constructor.
   */
  private BDDDotFileWriter() {
    // Intentionally left empty.
  }

  /**
   * Writes a given BDD as a dot file.
   * @param fileName the file name of the dot file to write
   * @param bdd      the BDD
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final String fileName, final BDD bdd) throws IOException {
    write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), bdd);
  }

  /**
   * Writes a given formula's internal data structure as a dot file.
   * @param file the file of the dot file to write
   * @param bdd  the BDD
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final File file, final BDD bdd) throws IOException {
    final StringBuilder sb = new StringBuilder(String.format("digraph G {%n"));
    if (!bdd.isContradiction()) {
      sb.append(String.format("  const_true [shape=box, label=\"$true\", style = bold, color = darkgreen];%n"));
    }
    if (!bdd.isTautology()) {
      sb.append(String.format("  const_false [shape=box, label=\"$false\", style = bold, color = red];%n"));
    }
    for (final BDDFactory.InternalBDDNode internalNode : bdd.internalNodes()) {
      sb.append(String.format("  id_%d [shape=ellipse, label=\"%s\"];%n", internalNode.nodenum(), internalNode.label()));
      sb.append(String.format("  id_%d -> %s [style = dotted, color = red];%n", internalNode.nodenum(), getNodeString(internalNode.low())));
      sb.append(String.format("  id_%d -> %s [color = darkgreen];%n", internalNode.nodenum(), getNodeString(internalNode.high())));
    }
    sb.append("}").append(System.lineSeparator());
    try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8))) {
      writer.append(sb);
      writer.flush();
    }
  }

  private static String getNodeString(final int i) {
    switch (i) {
      case 0:
        return "const_false";
      case 1:
        return "const_true";
      default:
        return "id_" + i;
    }
  }
}
