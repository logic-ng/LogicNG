package org.logicng.solvers.functions;

import java.util.ArrayList;
import java.util.List;

public class CartesianProduct {

    public <T> List<List<T>> product(final List<List<T>> lists) {
        final List<List<T>> product = new ArrayList<>();

        // We first create a list for each value of the first list
        product(product, new ArrayList<>(), lists);

        return product;
    }

    private <T> void product(final List<List<T>> result, final List<T> existingTupleToComplete, final List<List<T>> valuesToUse) {
        for (final T value : valuesToUse.get(0)) {
            final List<T> newExisting = new ArrayList<>(existingTupleToComplete);
            newExisting.add(value);

            // If only one column is left
            if (valuesToUse.size() == 1) {
                // We create a new list with the exiting tuple for each value with the value
                // added
                result.add(newExisting);
            } else {
                // If there are still several columns, we go into recursion for each value
                final List<List<T>> newValues = new ArrayList<>();
                // We build the next level of values
                for (int i = 1; i < valuesToUse.size(); i++) {
                    newValues.add(valuesToUse.get(i));
                }

                product(result, newExisting, newValues);
            }
        }
    }
}