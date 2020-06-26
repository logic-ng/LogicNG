package org.logicng;

/**
 * A class which exposes the LogicNG library version as specified in the POM.
 * This works only if the compiled library as jar is included in your project.
 * @version 2.0.0
 * @since 2.0.0
 */
public interface LogicNGVersion {

    /**
     * Returns the version string from the POM.
     * @return the version string from the POM
     */
    static String version() {
        final Package mainPackage = LogicNGVersion.class.getPackage();
        return mainPackage.getImplementationVersion();
    }

    /**
     * Returns the major version or -1 if the version could not be parsed.
     * @return the major version
     */
    static int major() {
        try {
            return Integer.parseInt(version().split("\\.")[0]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns the minor version or -1 if the version could not be parsed.
     * @return the minor version
     */
    static int minor() {
        try {
            return Integer.parseInt(version().split("\\.")[1]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns the patch version or -1 if the version could not be parsed.
     * @return the patch version
     */
    static int patch() {
        try {
            final String patchVersion = version().split("\\.")[2];
            return Integer.parseInt(patchVersion.split("-")[0]);
        } catch (final ArrayIndexOutOfBoundsException | NumberFormatException e) {
            return -1;
        }
    }

    /**
     * Returns whether this is a snapshot version or not.
     * @return true if it is a snapshot version, false otherwise
     */
    static boolean snapshot() {
        return version().contains("-SNAPSHOT");
    }
}
