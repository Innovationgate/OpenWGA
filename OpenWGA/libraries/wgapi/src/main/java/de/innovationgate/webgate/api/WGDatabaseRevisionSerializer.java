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


/**
 * A class that can be used to serialize/deserialize (and load) database revisions, returned by {@link WGDatabase#getRevision()} to string form.
 * @deprecated Since database revisions are always instances of {@link WGDatabaseRevision} now having their own serialisation/deserialisation methods
 */
public interface WGDatabaseRevisionSerializer {
    
    /**
     * Creates a serialized form of the given revision object, able to be persisted
     * @param revision
     * @throws Exception
     */
    public String toPersistentForm(Comparable<?> revision) throws Exception;
    
    /**
     * Recreates a revision object from a serialized form
     * @param revisionStr
     * @throws Exception
     */
    public Comparable<?> fromPersistentForm(String revisionStr) throws Exception;

}
