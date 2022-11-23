package org.logicng.solvers.maxsat;

import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.solvers.MaxSATSolver;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Helper class for reading MaxSAT input files in the WDIMACS format.
 * <a href="http://www.maxhs.org/docs/wdimacs.html">...</a>
 * @version 2.4.0
 * @since 2.4.0
 */
public class MaxSATReader {

    /**
     * Read a WDIMACS file to a MaxSAT solver.
     * @param solver   the MaxSAT solver
     * @param fileName the file name to read
     * @throws IOException if something goes wrong
     */
    static void readCnfToSolver(final MaxSATSolver solver, final String fileName) throws IOException {
        final FormulaFactory f = solver.factory();
        final BufferedReader reader = new BufferedReader(new FileReader(fileName));
        boolean pureMaxSat = false;
        int hardWeight = -1;
        while (reader.ready()) {
            final String line = reader.readLine();
            if (line.startsWith("p wcnf")) {
                final String[] header = line.trim().split(" ", -1);
                if (header.length > 4) {
                    hardWeight = Integer.parseInt(header[4]);
                }
                break;
            } else if (line.startsWith("p cnf")) {
                pureMaxSat = true;
                break;
            }
        }
        String[] tokens;
        final List<Literal> literals = new ArrayList<>();
        while (reader.ready()) {
            tokens = reader.readLine().trim().split(" ");
            assert pureMaxSat ? tokens.length >= 2 : tokens.length >= 3;
            assert "0".equals(tokens[tokens.length - 1]);
            literals.clear();
            final int weight = Integer.parseInt(tokens[0]);
            for (int i = pureMaxSat ? 0 : 1; i < tokens.length - 1; i++) {
                if (!tokens[i].isEmpty()) {
                    final int parsedLit = Integer.parseInt(tokens[i]);
                    final String var = "v" + Math.abs(parsedLit);
                    literals.add(parsedLit > 0 ? f.literal(var, true) : f.literal(var, false));
                }
            }
            if (pureMaxSat) {
                solver.addSoftFormula(f.or(literals), 1);
            } else if (weight == hardWeight) {
                solver.addHardFormula(f.or(literals));
            } else {
                solver.addSoftFormula(f.or(literals), weight);
            }
        }
    }
}
