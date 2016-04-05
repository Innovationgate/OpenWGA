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

package de.innovationgate.webgate.api;

import java.util.List;

/**
 * Extension interface for database cores that provide sequence functionality
 */
public interface WGDatabaseCoreFeatureSequenceProvider extends WGDatabaseCore {
    
    /**
     * Initializes a sequence, if it either is not yet initialized or argument forceInit is true.
     * In the latter case an already existing sequence will be reset to the given start value.
     * @param name Name of the sequence
     * @param startValue The start value of the sequence, i.e. the first value to serve by the first {@link #incrementSequence(String)} call.
     * @param forceInit true if you want to re-initialize the sequence even if it already exists
     * @throws WGBackendException
     * @return true if the sequence was really initialized, false if not 
     */
    public boolean initSequence(String name, long startValue, boolean forceInit) throws WGAPIException;
    
    /**
     * Increments the sequence and returns a new unique value.
     * If the sequence is not yet initialized then it is automatically by this call and returns the default first value of 1
     * @param name Name of the sequence
     * @return A new unique value.
     */
    public long incrementSequence(String name) throws WGAPIException;
    
    /**
     * Returns if a sequence of that name is already initialized.
     * A sequence is initialized if already some stored sequence information exists and the sequence is most likely beyond its starting value.
     * @param name Name of the sequence.
     */
    public boolean isSequenceInitialized(String name) throws WGAPIException;
    
    /**
     * Returns the returned value of the sequence on the last {@link #incrementSequence(String)} call. 0 if it was not yet used.
     * @param name Name of the sequence
     * @throws WGAPIException
     */
    public long getLastSequenceIncrementValue(String name) throws WGAPIException;
    
    /**
     * Deletes a sequence, clearing all increment and initialisation data
     * @param name Name of the sequence
     * @throws WGAPIException
     */
    public void deleteSequence(String name) throws WGAPIException;
    
    /**
     * Returns names of sequences already initialized and/or in use
     * @throws WGAPIException
     */
    public List<String> getUsedSequenceNames() throws WGAPIException;

}
