package org.logicng.solvers.functions;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Model;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;

import java.util.List;

public interface EnumerationCollector<R> {

    boolean addModel(LNGBooleanVector modelFromSolver, MiniSat solver, LNGIntVector relevantAllIndices, AdvancedModelEnumerationHandler handler);

    boolean commit(AdvancedModelEnumerationHandler handler);

    boolean rollback(AdvancedModelEnumerationHandler handler);

    List<Model> rollbackAndReturnModels(final MiniSat solver, AdvancedModelEnumerationHandler handler);

    R getResult();
}
