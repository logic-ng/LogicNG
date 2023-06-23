package org.logicng.solvers.functions.modelenumeration;

import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.handlers.SATHandler;

public final class EnumerationCollectorTestHandler implements AdvancedModelEnumerationHandler {

    private int foundModels;
    private int commitCalls;
    private int rollbackCalls;

    @Override
    public void started() {
        AdvancedModelEnumerationHandler.super.started();
        this.foundModels = 0;
    }

    @Override
    public SATHandler satHandler() {
        return null;
    }

    @Override
    public boolean foundModels(int numberOfModels) {
        this.foundModels += numberOfModels;
        return true;
    }

    @Override
    public boolean commit() {
        ++this.commitCalls;
        return true;
    }

    @Override
    public boolean rollback() {
        ++this.rollbackCalls;
        return true;
    }

    public int getFoundModels() {
        return foundModels;
    }

    public int getCommitCalls() {
        return commitCalls;
    }

    public int getRollbackCalls() {
        return rollbackCalls;
    }
}