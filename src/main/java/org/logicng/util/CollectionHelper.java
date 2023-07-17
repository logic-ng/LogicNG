// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.function.Supplier;

/**
 * A class which contains utility methods for {@link Collection} objects.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class CollectionHelper {

    /**
     * Private empty constructor. Class only contains static utility methods.
     */
    private CollectionHelper() {
        // Intentionally left empty
    }

    /**
     * Tests if the given collection is {@code null} or empty.
     * @param collection the collection, may be {@code null}
     * @param <T>        the type of the elements in the collection, may be
     *                   arbitrary
     * @return {@code true} if the collection is {@code null} or empty,
     *         otherwise {@code false}
     */
    public static <T> boolean nullOrEmpty(final Collection<T> collection) {
        return collection == null || collection.isEmpty();
    }

    /**
     * Null safe wrapper for collections. Returns an (unmodifiable!) empty list
     * if the given collection is {@code
     * null}, otherwise returns the given list unchanged.
     * @param collectionSupplier the collection, may be {@code null}
     * @param collectionFactory  the supplier for the collection
     * @param <T>                the type of the elements in the collection, may
     *                           be arbitrary
     * @param <C>                the type parameters of the collection
     * @return the collection itself, if it is not {@code null}, otherwise an
     *         unmodifiable empty list
     */
    public static <T, C extends Collection<T>> C nullSafe(final Supplier<C> collectionSupplier,
                                                          final Supplier<C> collectionFactory) {
        final C collection = collectionSupplier.get();
        return collection != null ? collection : collectionFactory.get();
    }

    /**
     * Null safe wrapper for collections. Returns an (unmodifiable!) empty list
     * if the given collection is {@code
     * null}, otherwise returns the given list unchanged.
     * @param collection the collection, may be {@code null}
     * @param <T>        the type of the elements in the collection, may be
     *                   arbitrary
     * @return the collection itself, if it is not {@code null}, otherwise an
     *         unmodifiable empty list
     */
    public static <T> Collection<T> nullSafe(final Collection<T> collection) {
        return collection != null ? collection : Collections.emptyList();
    }

    /**
     * Computes the intersection of the given collections. Each collection
     * treated in a null-safe manner, i.e. if a collection is {@code null} the
     * collection is considered to be an empty collection.
     * @param collections       the collections the intersection should be
     *                          computed for
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the intersection of the collections
     */
    public static <T, C extends Collection<T>> C intersection(final Collection<? extends Collection<T>> collections,
                                                              final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        boolean first = true;
        for (final Collection<T> collection : collections) {
            if (first) {
                result.addAll(nullSafe(collection));
                first = false;
            } else {
                result.retainAll(nullSafe(collection));
            }
        }
        return result;
    }

    /**
     * Computes the intersection of two collections. Each collection treated in
     * a null-safe manner, i.e. if a collection is {@code null} the collection
     * is considered to be an empty collection.
     * @param col1              the first collection
     * @param col2              the second collection
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the intersection of the two collections
     */
    public static <T, C extends Collection<T>> C intersection(final Collection<T> col1, final Collection<T> col2,
                                                              final Supplier<C> collectionFactory) {
        return intersection(Arrays.asList(col1, col2), collectionFactory);
    }

    /**
     * Computes the union of the given collections. Each collection is treated
     * in a null-safe manner, i.e. if a collection is {@code null} the
     * collection is considered to be an empty collection
     * @param collections       the collections the union should be computed for
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the union of the collections
     */
    public static <T, C extends Collection<T>> C union(final Collection<? extends Collection<T>> collections,
                                                       final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        for (final Collection<T> collection : collections) {
            result.addAll(nullSafe(collection));
        }
        return result;
    }

    /**
     * Computes the union of two collections. Each collection treated in a
     * null-safe manner, i.e. if a collection is {@code null} the collection is
     * considered to be an empty collection.
     * @param col1              the first collection
     * @param col2              the second collection
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the union of the two collections
     */
    public static <T, C extends Collection<T>> C union(final Collection<T> col1, final Collection<T> col2,
                                                       final Supplier<C> collectionFactory) {
        return union(Arrays.asList(col1, col2), collectionFactory);
    }

    /**
     * Computes the difference of the given two collections. The result contains
     * all elements from the first collection that are not contained within the
     * second collection. Each collection treated in a null-safe manner, i.e. if
     * a collection is {@code null} the collection is considered to be an empty
     * collection.
     * @param col1              the first collection
     * @param col2              the second collection
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the union of the two collections
     */
    public static <T, C extends Collection<T>> C difference(final Collection<T> col1, final Collection<T> col2,
                                                            final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        result.addAll(nullSafe(col1));
        result.removeAll(nullSafe(col2));
        return result;
    }

    /**
     * Builds a string representation of a collection using a delimiter.
     * @param collection the collection
     * @param delimiter  the delimiter
     * @param <T>        the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final Collection<T> collection, final String delimiter) {
        return mkString(collection, "", delimiter, "");
    }

    /**
     * Builds a string representation of a collection using a prefix, delimiter
     * and suffix.
     * @param collection the collection
     * @param prefix     the prefix
     * @param delimiter  the delimiter
     * @param suffix     the suffix
     * @param <T>        the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final Collection<T> collection, final String prefix, final String delimiter,
                                      final String suffix) {
        final StringBuilder sb = new StringBuilder(prefix);
        final Iterator<T> iterator = collection.iterator();
        while (iterator.hasNext()) {
            sb.append(iterator.next());
            if (iterator.hasNext()) {
                sb.append(delimiter);
            }
        }
        sb.append(suffix);
        return sb.toString();
    }

    /**
     * Builds a string representation of an array using a delimiter.
     * @param array     the array
     * @param delimiter the delimiter
     * @param <T>       the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final T[] array, final String delimiter) {
        return mkString(array, "", delimiter, "");
    }

    /**
     * Builds a string representation of an array using a prefix, delimiter and
     * suffix.
     * @param array     the array
     * @param prefix    the prefix
     * @param delimiter the delimiter
     * @param suffix    the suffix
     * @param <T>       the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final T[] array, final String prefix, final String delimiter,
                                      final String suffix) {
        final StringBuilder sb = new StringBuilder(prefix);
        for (int i = 0; i < array.length; i++) {
            sb.append(array[i]);
            if (i != array.length - 1) {
                sb.append(delimiter);
            }
        }
        sb.append(suffix);
        return sb.toString();
    }
}
