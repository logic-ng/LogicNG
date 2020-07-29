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

package org.logicng.util;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.function.Supplier;

/**
 * A class which contains utility methods for {@link Collection} objects.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class CollectionHelper {

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private CollectionHelper() {
        // Intentionally left empty
    }

    /**
     * Tests if the given collection is {@code null} or empty.
     * @param collection the collection, may be {@code null}
     * @param <T>        the type of the elements in the collection, may be arbitrary
     * @return {@code true} if the collection is {@code null} or empty, otherwise {@code false}
     */
    public static <T> boolean nullOrEmpty(final Collection<T> collection) {
        return collection == null || collection.isEmpty();
    }

    /**
     * Null safe wrapper for collections. Returns an (unmodifiable!) empty list if the given collection is {@code
     * null}, otherwise returns the given list unchanged.
     * @param collection the collection, may be {@code null}
     * @param <T>        the type of the elements in the collection, may be arbitrary
     * @return the collection itself, if it is not {@code null}, otherwise an unmodifiable empty list
     */
    public static <T> Collection<T> nullSafe(final Collection<T> collection) {
        return collection != null ? collection : Collections.emptyList();
    }

    /**
     * Computes the intersection of two collections.
     * @param col1              the first collection
     * @param col2              the second collection
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the intersection of the two collections
     */
    public static <T, C extends Collection<T>> C intersection(final Collection<T> col1, final Collection<T> col2, final Supplier<C> collectionFactory) {
        if (col1 == null || col2 == null) {
            return collectionFactory.get();
        }
        final C result = collectionFactory.get();
        result.addAll(col1);
        result.retainAll(col2);
        return result;
    }

    /**
     * Computes the union of two collections.
     * @param col1              the first collection
     * @param col2              the second collection
     * @param collectionFactory the supplier for the collection
     * @param <T>               the type parameters of the elements
     * @param <C>               the type parameters of the collection
     * @return the union of the two collections
     */
    public static <T, C extends Collection<T>> C union(final Collection<T> col1, final Collection<T> col2, final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        if (col1 != null) {
            result.addAll(col1);
        }
        if (col2 != null) {
            result.addAll(col2);
        }
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
     * Builds a string representation of a collection using a prefix, delimiter and suffix.
     * @param collection the collection
     * @param prefix     the prefix
     * @param delimiter  the delimiter
     * @param suffix     the suffix
     * @param <T>        the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final Collection<T> collection, final String prefix, final String delimiter, final String suffix) {
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
     * Builds a string representation of an array using a prefix, delimiter and suffix.
     * @param array     the array
     * @param prefix    the prefix
     * @param delimiter the delimiter
     * @param suffix    the suffix
     * @param <T>       the type parameter of the elements
     * @return the string representation of the collection
     */
    public static <T> String mkString(final T[] array, final String prefix, final String delimiter, final String suffix) {
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
