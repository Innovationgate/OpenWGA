/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.utils;

import java.util.Iterator;

/**
 * An iterator that is able to skip an arbitrary number of results without retrieving them (which may save resources)
 */
public interface SkippingIterator<T> extends Iterator<T> {
    
    /**
     * Will skip the given number of elements in the queue. The next call of the {@link #next()} method will return the element immediately after the last skipped one.
     * Must be called before any iterating methods ({@link #next()} {@link #hasNext()}) have been used. Otherwise an {@link IllegalStateException} is thrown. 
     * @param nrOfElements Number of elements to skip
     * @return The number of skipping iterations that could not be performed because the iterator reached the end. Should be 0 when the iterator was able to complete all operations without reaching the end.
     * @throws IllegalStateException when called after iterating methods have been used
     */
    public int skip(int nrOfElements);

}
