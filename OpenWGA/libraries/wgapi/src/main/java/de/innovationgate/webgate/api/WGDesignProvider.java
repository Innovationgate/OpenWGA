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
 * A provider of design document cores to be used with a WGDatabase if the design of that database comes from another
 * location. It can provide cores for all WGDesignDocument descendants except areas. It tells the WGDatabase which
 * designs it can provide via the providesType-Method.
 *
 */
public interface WGDesignProvider {

    /**
     * Retrieves all design objects of the given doc class.
     * @param type The doc class. See constants WGDocument.FDC_...
     * @return An unordered list of all design objects for this doc class. An empty list if there are no design objects of that class.
     */
    public abstract List getDesignObjects(int type) throws WGAPIException;

    /**
     * Retrieves a design object by it's doc class and unique name.
     * @param type The doc class of the design object. See Constants under WGDocument.FDC_...
     * @param name The unique name of the design
     * @param strMediaKey For media key specific design objects (e.g. WebTML-Modules), the media key of the design object to be retrieved
     * @return The retrieved design object.
     */
    public abstract WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException;

    /**
     * Determines if this design provider provides designs of this type
     * @param type The type. A constant WGDocument.TYPE_...
     * @return true if the type is provided
     */
    public abstract boolean providesType(int type);

    /**
     * Should determine if the given document core was served by this type of provider.
     * If not determinable this method should return false.
     * @param core The core to test
     * @return true, if the core was provided by this provider, false if not or not sure
     */
    public abstract boolean isProviderCore(WGDocumentCore core);

    /**
     * Displays a descriptive name of the type and purpose of this provider, to be displayed in error messages
     */
    public abstract String getName();

    /**
     * Creates the core for a new design document
     * @param type Type of document. Constant of WGDocument.TYPE_...
     * @param name Name of new design
     * @param mediaKey Mediakey of new design (for tml modules only. Otherwise provide null)
     */
    public abstract WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAuthorisationException, WGCreationException;

    /**
     * Adds a listener for design change events, issued by this design provider
     * If the design provider does not support this then {@link #isNotifying()} should return false
     * @param changeListener The listener to add
     */
    public abstract void addDesignChangeListener(WGDesignChangeListener changeListener);

    /**
     * Removes a listener for design change events 
     * @param changeListener The listener to remove
     */
    public abstract void removeDesignChangeListener(WGDesignChangeListener changeListener);

    /**
     * Prepares this object to be discarded, giving it the opportunity to clean up resources, references.
     */
    public abstract void dispose();

    /**
     * Determines if this design provider notifies it's design change listeners about design changes
     */
    public abstract boolean isNotifying();
    
    /**
     * Notifies the design provider of a new session, giving it the opportunity to connect to backends, do maintenance operations etc.
     * @param context
     */
    public void openSession(WGSessionContext context) throws WGBackendException;
    
    /**
     * Notifies the design provider of a closed session session, giving it the opportunity to disconnect from backends, do maintenance operations etc. 
     */
    public void closeSession() throws WGBackendException;
    
    
    /**
     * Provides a unique hashcode representing the design that is served by this provider for comparision purposes
     */
    public int designHashCode();
    
    /**
     * Returns if access to resources from this provider should be synchronized by WGAPI like regular backend accesses.
     * Disabling this may improve multithreaded access to this provider but also may mean more parallel traffic
     */
    public boolean isSynchronizeAccess();
    
    /**
     * Returns the supposed text encoding of text files in this design.
     * May be null if not determined text encoding is known.
     * If this design object is a combination of multiple designs it should return the encoding of the root design.
     */
    public String getFileEncoding();

}
