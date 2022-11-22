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
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * OLL Solver.
 * @version 2.4.0
 * @since 2.4.0
 */
public class OLL extends MaxSAT {
    private MiniSatStyleSolver solver;
    private final Encoder encoder;
    private final SortedMap<Integer, Integer> coreMapping; // Mapping between the assumption literal and the respective soft clause.
    private final SortedMap<Integer, IntTriple> boundMapping; // lit -> <ID, bound, weight>
    private final LNGBooleanVector activeSoft; // Soft clauses that are currently in the MaxSAT formula.
    private int minWeight;

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
        if (this.problemType == ProblemType.WEIGHTED) {
            return weighted();
        } else {
            return unweighted();
        }
    }

    private MiniSatStyleSolver rebuildSolver() {
        final MiniSatStyleSolver s = newSATSolver();
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

    private MaxSATResult weighted() {
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
        this.minWeight = this.currentWeight;

        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler, assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final LNGBooleanVector model = this.solver.model();
                final int newCost = computeCostModel(model, Integer.MAX_VALUE);
                if (newCost < this.ubCost || this.nbSatisfiable == 1) {
                    saveModel(model);
                    this.ubCost = newCost;
                }
                if (this.nbSatisfiable == 1) {
                    this.minWeight = findNextWeightDiversity(this.minWeight, cardinalityAssumptions);
                    for (int i = 0; i < nSoft(); i++) {
                        if (this.softClauses.get(i).weight() >= this.minWeight) {
                            assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                        }
                    }
                } else {
                    // compute min weight in soft
                    int notConsidered = 0;
                    for (int i = 0; i < nSoft(); i++) {
                        if (this.softClauses.get(i).weight() < this.minWeight) {
                            notConsidered++;
                        }
                    }
                    for (final Integer it : cardinalityAssumptions) {
                        final IntTriple softId = this.boundMapping.get(it);
                        assert softId != null;
                        if (softId.weight < this.minWeight) {
                            notConsidered++;
                        }
                    }
                    if (notConsidered != 0) {
                        this.minWeight = findNextWeightDiversity(this.minWeight, cardinalityAssumptions);
                        assumptions.clear();
                        for (int i = 0; i < nSoft(); i++) {
                            if (!this.activeSoft.get(i) && this.softClauses.get(i).weight() >= this.minWeight) {
                                assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                            }
                        }
                        for (final Integer it : cardinalityAssumptions) {
                            final IntTriple softId = this.boundMapping.get(it);
                            assert softId != null;
                            if (softId.weight >= this.minWeight) {
                                assumptions.push(not(it));
                            }
                        }
                    } else {
                        assert this.lbCost == newCost;
                        return MaxSATResult.OPTIMUM;
                    }
                }
            } else if (res == Tristate.FALSE) {
                // reduce the weighted to the unweighted case
                int minCore = Integer.MAX_VALUE;
                for (int i = 0; i < this.solver.conflict().size(); i++) {
                    final int p = this.solver.conflict().get(i);
                    if (this.coreMapping.containsKey(p)) {
                        assert !this.activeSoft.get(this.coreMapping.get(p));
                        if (this.softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).weight() < minCore) {
                            minCore = this.softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).weight();
                        }
                    }
                    if (this.boundMapping.containsKey(p)) {
                        final IntTriple softId = this.boundMapping.get(this.solver.conflict().get(i));
                        if (softId.weight < minCore) {
                            minCore = softId.weight;
                        }
                    }
                }
                this.lbCost += minCore;
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
                        if (this.softClauses.get(this.coreMapping.get(p)).weight() > minCore) {
                            assert !this.activeSoft.get(this.coreMapping.get(p));
                            // Split the clause
                            final int indexSoft = this.coreMapping.get(p);
                            assert this.softClauses.get(indexSoft).weight() - minCore > 0;

                            // Update the weight of the soft clause.
                            this.softClauses.get(indexSoft).setWeight(this.softClauses.get(indexSoft).weight() - minCore);
                            final LNGIntVector clause = new LNGIntVector(this.softClauses.get(indexSoft).clause());
                            final LNGIntVector vars = new LNGIntVector();

                            // Since cardinality constraints are added the variables are not in sync...
                            while (nVars() < this.solver.nVars()) {
                                newLiteral(false);
                            }
                            final int l = newLiteral(false);
                            vars.push(l);

                            // Add a new soft clause with the weight of the core.
                            addSoftClause(minCore, clause, vars);
                            this.activeSoft.push(true);

                            // Add information to the SAT solver
                            newSATVariable(this.solver);
                            clause.push(l);
                            this.solver.addClause(clause, null);
                            assert clause.size() - 1 == this.softClauses.get(indexSoft).clause().size();
                            assert this.softClauses.get(nSoft() - 1).relaxationVars().size() == 1;

                            // Create a new assumption literal.
                            this.softClauses.get(nSoft() - 1).setAssumptionVar(l);
                            assert this.softClauses.get(nSoft() - 1).assumptionVar() == this.softClauses.get(nSoft() - 1).relaxationVars().get(0);
                            this.coreMapping.put(l, nSoft() - 1); // Map the new soft clause to its assumption literal.
                            softRelax.push(l);
                            assert this.softClauses.get(this.coreMapping.get(l)).weight() == minCore;
                            assert this.activeSoft.size() == nSoft();
                        } else {
                            assert this.softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).weight() == minCore;
                            softRelax.push(p);
                            assert !this.activeSoft.get(this.coreMapping.get(p));
                            this.activeSoft.set(this.coreMapping.get(p), true);
                        }
                    }
                    if (this.boundMapping.containsKey(p)) {
                        assert cardinalityAssumptions.contains(p);
                        // this is a soft cardinality -- bound must be increased
                        final IntTriple softId = this.boundMapping.get(this.solver.conflict().get(i));

                        // increase the bound
                        assert softId.id < softCardinality.size();
                        assert softCardinality.get(softId.id).hasCardEncoding();
                        if (softId.weight == minCore) {
                            cardinalityAssumptions.remove(p);
                            cardinalityRelax.push(p);
                            joinObjFunction.clear();
                            encodingAssumptions.clear();
                            softCardinality.get(softId.id).incUpdateCardinality(this.solver, joinObjFunction,
                                    softCardinality.get(softId.id).lits(), softId.bound + 1, encodingAssumptions);

                            // if the bound is the same as the number of literals then no restriction is applied
                            if (softId.bound + 1 < softCardinality.get(softId.id).outputs().size()) {
                                assert softCardinality.get(softId.id).outputs().size() > softId.bound + 1;
                                final int out = softCardinality.get(softId.id).outputs().get(softId.bound + 1);
                                this.boundMapping.put(out, new IntTriple(softId.id, softId.bound + 1, minCore));
                                cardinalityAssumptions.add(out);
                            }
                        } else {
                            // Duplicate cardinality constraint
                            final Encoder e = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
                            e.setIncremental(MaxSATConfig.IncrementalStrategy.ITERATIVE);
                            e.buildCardinality(this.solver, softCardinality.get(softId.id).lits(), softId.bound);
                            assert e.outputs().size() > softId.bound;
                            final int out = e.outputs().get(softId.bound);
                            softCardinality.push(e);
                            this.boundMapping.put(out, new IntTriple(softCardinality.size() - 1, softId.bound, minCore));
                            cardinalityRelax.push(out);

                            // Update value of the previous cardinality constraint
                            assert softId.weight - minCore > 0;
                            this.boundMapping.put(p, new IntTriple(softId.id, softId.bound, softId.weight - minCore));

                            // Update bound as usual...
                            final IntTriple softCoreId = this.boundMapping.get(out);
                            joinObjFunction.clear();
                            encodingAssumptions.clear();
                            softCardinality.get(softCoreId.id).incUpdateCardinality(this.solver, joinObjFunction,
                                    softCardinality.get(softCoreId.id).lits(), softCoreId.bound + 1, encodingAssumptions);

                            // if the bound is the same as the number of literals then no restriction is applied
                            if (softCoreId.bound + 1 < softCardinality.get(softCoreId.id).outputs().size()) {
                                assert softCardinality.get(softCoreId.id).outputs().size() > softCoreId.bound + 1;
                                final int out2 = softCardinality.get(softCoreId.id).outputs().get(softCoreId.bound + 1);
                                this.boundMapping.put(out2, new IntTriple(softCoreId.id, softCoreId.bound + 1, minCore));
                                cardinalityAssumptions.add(out2);
                            }
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
                    this.boundMapping.put(out, new IntTriple(softCardinality.size() - 1, 1, minCore));
                    cardinalityAssumptions.add(out);
                }
                assumptions.clear();
                for (int i = 0; i < nSoft(); i++) {
                    if (!this.activeSoft.get(i) && this.softClauses.get(i).weight() >= this.minWeight) {
                        assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                    }
                }
                for (final Integer it : cardinalityAssumptions) {
                    final IntTriple softId = this.boundMapping.get(it);
                    assert softId != null;
                    if (softId.bound >= this.minWeight) {
                        assumptions.push(not(it));
                    }
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

    private int findNextWeightDiversity(final int weight, final Set<Integer> cardinalityAssumptions) {
        assert (this.nbSatisfiable > 0);
        int nextWeight = weight;
        int nbClauses;
        final LinkedHashSet<Integer> nbWeights = new LinkedHashSet<>();
        final double alpha = 1.25;
        boolean findNext = false;
        while (true) {
            if (this.nbSatisfiable > 1 || findNext) {
                nextWeight = findNextWeight(nextWeight, cardinalityAssumptions);
            }
            nbClauses = 0;
            nbWeights.clear();
            for (int i = 0; i < nSoft(); i++) {
                if (this.softClauses.get(i).weight() >= nextWeight) {
                    nbClauses++;
                    nbWeights.add(this.softClauses.get(i).weight());
                }
            }
            for (final Integer it : cardinalityAssumptions) {
                final IntTriple softId = this.boundMapping.get(it);
                assert softId != null;
                if (softId.weight >= nextWeight) {
                    nbClauses++;
                    nbWeights.add(softId.weight);
                }
            }
            if ((float) nbClauses / nbWeights.size() > alpha || nbClauses == nSoft() + cardinalityAssumptions.size()) {
                break;
            }
            if (this.nbSatisfiable == 1 && !findNext) {
                findNext = true;
            }
        }
        return nextWeight;
    }

    int findNextWeight(final int weight, final Set<Integer> cardinalityAssumptions) {
        int nextWeight = 1;
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() > nextWeight && this.softClauses.get(i).weight() < weight) {
                nextWeight = this.softClauses.get(i).weight();
            }
        }
        for (final Integer it : cardinalityAssumptions) {
            final IntTriple softId = this.boundMapping.get(it);
            assert softId != null;
            if (softId.weight > nextWeight && softId.weight < weight) {
                nextWeight = softId.weight;
            }
        }
        return nextWeight;
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
