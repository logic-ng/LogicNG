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

package org.logicng;

/**
 * A class which exposes the LogicNG library version as specified in the POM.
 * This works only if the compiled library as jar is included in your project.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class LogicNGVersion {

    /**
     * Private constructor.
     */
    private LogicNGVersion() {
        // Intentionally left empty.
    }

    /**
     * Returns the version string from the POM.
     * @return the version string from the POM
     */
    public static String version() {
        final Package mainPackage = LogicNGVersion.class.getPackage();
        return mainPackage.getImplementationVersion();
    }

    /**
     * Returns the major version or -1 if the version could not be parsed.
     * @return the major version
     */
    public static int major() {
        return major(version());
    }

    private static int major(final String version) {
        try {
            return Integer.parseInt(version.split("\\.")[0]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns the minor version or -1 if the version could not be parsed.
     * @return the minor version
     */
    public static int minor() {
        return minor(version());
    }

    private static int minor(final String version) {
        try {
            return Integer.parseInt(version.split("\\.")[1]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns the patch version or -1 if the version could not be parsed.
     * @return the patch version
     */
    public static int patch() {
        return patch(version());
    }

    private static int patch(final String version) {
        try {
            return Integer.parseInt(version.split("\\.")[2].split("-")[0]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns whether this is a snapshot version or not.
     * @return true if it is a snapshot version, false otherwise
     */
    public static boolean snapshot() {
        return snapshot(version());
    }

    private static boolean snapshot(final String version) {
        return version.contains("-SNAPSHOT");
    }
}
