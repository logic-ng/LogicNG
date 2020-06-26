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

package org.logicng.graphs.io;

import static org.assertj.core.api.Assertions.assertThat;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.Condition;
import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.Test;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.GraphTest;
import org.logicng.graphs.io.conditions.ContainsCondition;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for the {@link GraphDotFileWriter}.
 * @version 2.0.0
 * @since 1.2
 */
public class GraphDotFileWriterTest {

    @Test
    public void testSmall() throws IOException {
        final Graph<String> g = new Graph<>();
        g.connect(g.node("A"), g.node("B"));
        g.node("C");
        testFiles("small", g);
    }

    @Test
    public void test20() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("30");
        for (long i = 0; i < 30; i++) {
            g.node(i);
        }
        testFiles("30", g);
    }

    @Test
    public void test50p1() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("50");
        g.node(51L);
        testFiles("50p1", g);
    }

    private <T> void testFiles(final String fileName, final Graph<T> g) throws IOException {
        GraphDotFileWriter.write("src/test/resources/graphs/io/temp/" + fileName + ".dot", g);
        final File expected = new File("src/test/resources/graphs/io/graphs-dot/" + fileName + ".dot");
        final File temp = new File("src/test/resources/graphs/io/temp/" + fileName + ".dot");
        assertFilesEqual(expected, temp);
    }

    private void assertFilesEqual(final File expected, final File actual) throws IOException {
        final SoftAssertions softly = new SoftAssertions();
        final BufferedReader expReader = new BufferedReader(new FileReader(expected));
        final BufferedReader actReader = new BufferedReader(new FileReader(actual));
        final List<String> expEdgeLines = new ArrayList<>();
        final List<String> actEdgeLines = new ArrayList<>();
        final List<String> expNodeLines = new ArrayList<>();
        final List<String> actNodeLines = new ArrayList<>();
        for (int lineNumber = 1; expReader.ready() && actReader.ready(); lineNumber++) {
            final String exp = expReader.readLine();
            final String act = actReader.readLine();
            if (exp.contains("{") || exp.contains("}")) {
                softly.assertThat(act).as("Line " + lineNumber + " not equal").isEqualTo(exp);
            } else {
                if (exp.contains("-")) {
                    expEdgeLines.add(exp);
                } else {
                    expNodeLines.add(exp);
                }
                if (act.contains("-")) {
                    actEdgeLines.add(act);
                } else {
                    actNodeLines.add(act);
                }
            }
        }
        assertThat(actNodeLines).allMatch(expNodeLines::contains);
        for (final String actEdge : actEdgeLines) {
            final String[] actEdgeNodes = actEdge.trim().split(" -- ");
            final Condition<? super List<? extends String>> left = new ContainsCondition(actEdge);
            final Condition<? super List<? extends String>> right = new ContainsCondition("  " + actEdgeNodes[1] + " -- " + actEdgeNodes[0]);
            softly.assertThat(expEdgeLines).has(Assertions.anyOf(left, right));
        }
        if (expReader.ready()) {
            softly.fail("Missing line(s) found, starting with \"" + expReader.readLine() + "\"");
        }
        if (actReader.ready()) {
            softly.fail("Additional line(s) found, starting with \"" + actReader.readLine() + "\"");
        }
        softly.assertAll();
    }
}
