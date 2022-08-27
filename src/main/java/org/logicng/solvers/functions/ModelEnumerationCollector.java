package org.logicng.solvers.functions;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Model;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;

import java.util.ArrayList;
import java.util.List;

public class ModelEnumerationCollector implements EnumerationCollector<List<Model>> {

    private final List<Model> committedModels = new ArrayList<>();
    private final List<Model> uncommittedModels = new ArrayList<>();

    @Override
    public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices, final AdvancedModelEnumerationHandler handler) {
        final Model model = solver.createModel(modelFromSolver, relevantAllIndices);
        this.uncommittedModels.add(model);
        return handler == null || handler.foundModel();
    }

    @Override
    public boolean commit(final AdvancedModelEnumerationHandler handler) {
        this.committedModels.addAll(this.uncommittedModels);
        this.uncommittedModels.clear();
        return handler == null || handler.commit();
    }

    @Override
    public boolean rollback(final AdvancedModelEnumerationHandler handler) {
        this.uncommittedModels.clear();
        return handler == null || handler.rollback();
    }

    @Override
    public List<Model> rollbackAndReturnModels(final AdvancedModelEnumerationHandler handler) {
        final List<Model> modelsToReturn = new ArrayList<>(this.uncommittedModels);
        rollback(handler);
        return modelsToReturn;
    }

    @Override
    public List<Model> getResult() {
        return this.committedModels;
    }
}
