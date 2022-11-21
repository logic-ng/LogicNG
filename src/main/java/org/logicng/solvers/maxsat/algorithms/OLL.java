package org.logicng.solvers.maxsat.algorithms;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.maxsat.encodings.Encoder;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.LinkedHashSet;
import java.util.SortedMap;
import java.util.TreeMap;

public class OLL extends MaxSAT {
    private MiniSatStyleSolver solver;
    private final Encoder encoder;
    private final MaxSATConfig.IncrementalStrategy incrementalStrategy;

    //private LNGIntVector objFunction; // Literals to be used in the constraint that excludes models.
    //private LNGIntVector coeffs; // Coefficients of the literals that are used in the constraint that excludes models.
    private final SortedMap<Integer, Integer> coreMapping; // Mapping between the assumption literal and the respective soft clause.
    //private SortedMap<Integer, Integer> coreCardinality; // Mapping between the assumption literal and the respective soft clause.

    private final SortedMap<Integer, IntTriple> boundMapping; // lit -> <ID, bound, weight>

    private final LNGBooleanVector activeSoft; // Soft clauses that are currently in the MaxSAT formula.
    private final int minWeight;

    /**
     * Constructs a new solver with default values.
     */
    public OLL() {
        this(MaxSATConfig.builder().build());
    }

    /**
     * Constructs a new solver with a given configuration.
     * @param config the configuration
     */
    public OLL(final MaxSATConfig config) {
        super(config);
        this.solver = null;
        this.verbosity = config.verbosity;
        this.incrementalStrategy = config.incrementalStrategy;
        this.encoder = new Encoder(config.cardinalityEncoding);
        this.encoder.setPBEncoding(config.pbEncoding);
        this.coreMapping = new TreeMap<>();
        this.boundMapping = new TreeMap<>();
        this.activeSoft = new LNGBooleanVector();
        this.minWeight = 1;
    }

    @Override
    public MaxSATResult search() {
        if (this.encoder.cardEncoding() != MaxSATConfig.CardinalityEncoding.TOTALIZER) {
            throw new IllegalStateException("Error: Currently OLL only supports the totalizer encoding.");
        }
        if (this.problemType == ProblemType.UNWEIGHTED) {
            return unweighted();
        } else {
            //TODO
            return null;
        }
    }

    private MiniSatStyleSolver rebuildSolver() {
        final MiniSatStyleSolver s = newSATSolver();
        //TODO reserveSATVariables(S, maxsat_formula->nVars());
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }

        LNGIntVector clause;
        for (int i = 0; i < nSoft(); i++) {
            clause = new LNGIntVector(this.softClauses.get(i).clause());
            for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                clause.push(this.softClauses.get(i).relaxationVars().get(j));
            }
            s.addClause(clause, null);
        }

        return s;
    }

    private MaxSATResult unweighted() {
        this.nbInitialVariables = nVars();
        Tristate res;
        initRelaxation();
        this.solver = rebuildSolver();

        final LNGIntVector assumptions = new LNGIntVector();
        final LNGIntVector joinObjFunction = new LNGIntVector();
        final LNGIntVector encodingAssumptions = new LNGIntVector();
        this.encoder.setIncremental(MaxSATConfig.IncrementalStrategy.ITERATIVE);

        this.activeSoft.growTo(nSoft(), false);
        for (int i = 0; i < nSoft(); i++) {
            this.coreMapping.put(this.softClauses.get(i).assumptionVar(), i);
        }

        final LinkedHashSet<Integer> cardinalityAssumptions = new LinkedHashSet<>();
        final LNGVector<Encoder> softCardinality = new LNGVector<>();

        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler, assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final LNGBooleanVector model = this.solver.model();
                final int newCost = computeCostModel(model, Integer.MAX_VALUE);
                saveModel(model);

                this.ubCost = newCost;
                if (this.nbSatisfiable == 1) {
                    if (newCost == 0) {
                        return MaxSATResult.OPTIMUM;
                    }
                    for (int i = 0; i < nSoft(); i++) {
                        assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                    }
                } else {
                    assert this.lbCost == newCost;
                    return MaxSATResult.OPTIMUM;
                }
            } else {
                this.lbCost++;
                this.nbCores++;
                if (this.nbSatisfiable == 0) {
                    return MaxSATResult.UNSATISFIABLE;
                }
                if (this.lbCost == this.ubCost) {
                    assert this.nbSatisfiable > 0;
                    return MaxSATResult.OPTIMUM;
                }

                this.sumSizeCores += this.solver.conflict().size();

                final LNGIntVector softRelax = new LNGIntVector();
                final LNGIntVector cardinalityRelax = new LNGIntVector();
                for (int i = 0; i < this.solver.conflict().size(); i++) {
                    final int p = this.solver.conflict().get(i);
                    if (this.coreMapping.containsKey(p)) {
                        assert !this.activeSoft.get(this.coreMapping.get(p));
                        this.activeSoft.set(this.coreMapping.get(this.solver.conflict().get(i)), true);
                        assert p == this.softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).relaxationVars().get(0);
                        softRelax.push(p);
                    }

                    if (this.boundMapping.containsKey(p)) {
                        assert cardinalityAssumptions.contains(p);
                        cardinalityAssumptions.remove(p);
                        cardinalityRelax.push(p);

                        // this is a soft cardinality -- bound must be increased
                        final IntTriple softId = this.boundMapping.get(this.solver.conflict().get(i));
                        // // increase the bound
                        assert softId.id < softCardinality.size();
                        assert softCardinality.get(softId.id).hasCardEncoding();

                        joinObjFunction.clear();
                        encodingAssumptions.clear();
                        softCardinality.get(softId.id).incUpdateCardinality(this.solver, joinObjFunction, softCardinality.get(softId.id).lits(),
                                softId.bound + 1, encodingAssumptions);

                        // if the bound is the same as the number of literals then no restriction is applied
                        if (softId.bound + 1 < softCardinality.get(softId.id).outputs().size()) {
                            assert softCardinality.get(softId.id).outputs().size() > softId.bound + 1;
                            final int out = softCardinality.get(softId.id).outputs().get(softId.bound + 1);
                            this.boundMapping.put(out, new IntTriple(softId.id, softId.bound + 1, 1));
                            cardinalityAssumptions.add(out);
                        }
                    }
                }

                assert softRelax.size() + cardinalityRelax.size() > 0;
                if (softRelax.size() == 1 && cardinalityRelax.size() == 0) {
                    this.solver.addClause(softRelax.get(0), null);
                }

                if (softRelax.size() + cardinalityRelax.size() > 1) {
                    final LNGIntVector relaxHarden = new LNGIntVector(softRelax);
                    for (int i = 0; i < cardinalityRelax.size(); i++) {
                        relaxHarden.push(cardinalityRelax.get(i));
                    }
                    final Encoder e = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
                    e.setIncremental(MaxSATConfig.IncrementalStrategy.ITERATIVE);
                    e.buildCardinality(this.solver, relaxHarden, 1);
                    softCardinality.push(e);
                    assert e.outputs().size() > 1;

                    final int out = e.outputs().get(1);
                    this.boundMapping.put(out, new IntTriple(softCardinality.size() - 1, 1, 1));
                    cardinalityAssumptions.add(out);
                }

                // reset the assumptions
                assumptions.clear();
                for (int i = 0; i < nSoft(); i++) {
                    if (!this.activeSoft.get(i)) {
                        assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                    }
                }
                for (final Integer it : cardinalityAssumptions) {
                    assumptions.push(not(it));
                }
            }
        }
    }

    private void initRelaxation() {
        for (int i = 0; i < this.nbSoft; i++) {
            final int l = newLiteral(false);
            this.softClauses.get(i).relaxationVars().push(l);
            this.softClauses.get(i).setAssumptionVar(l);
        }
    }

    private static class IntTriple {
        private final int id;
        private final int bound;
        private final int weight;

        public IntTriple(final int id, final int bound, final int weight) {
            this.id = id;
            this.bound = bound;
            this.weight = weight;
        }
    }
}
