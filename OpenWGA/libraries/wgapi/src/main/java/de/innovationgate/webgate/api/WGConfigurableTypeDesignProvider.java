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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import de.innovationgate.utils.WGUtils;

/**
 * A base class for design providers with internal management of provided document types.
 * Method {@link #initProviderTypes(List)} should be called after the configuration for provided types has been received,
 * but before the provider is used.
 *
 */
public abstract class WGConfigurableTypeDesignProvider implements WGDesignProvider {
    
    protected void initProviderTypes(List<Integer> providerTypesList) {
        if (providerTypesList != null) {
            _providedTypes.addAll(providerTypesList);
        }
        else {
            _providedTypes.add(new Integer(WGDocument.TYPE_FILECONTAINER));
            _providedTypes.add(new Integer(WGDocument.TYPE_TML));
            _providedTypes.add(new Integer(WGDocument.TYPE_CSSJS));
        }
    }

    private Set<Integer> _providedTypes = new HashSet<Integer>();

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#getDesignObjects(int)
     */
    public abstract List getDesignObjects(int type) throws WGAPIException;

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#getDesignObject(int, java.lang.String, java.lang.String)
     */
    public abstract WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException;

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#providesType(int)
     */
    public boolean providesType(int type) {
        return _providedTypes .contains(new Integer(type));
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#isProviderCore(de.innovationgate.webgate.api.WGDocumentCore)
     */
    public abstract boolean isProviderCore(WGDocumentCore core);

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#getName()
     */
    public abstract String getName();

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#createDesignDocument(int, java.lang.String, java.lang.String)
     */
    public abstract WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAuthorisationException, WGCreationException;
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#addDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public abstract void addDesignChangeListener(WGDesignChangeListener changeListener);
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#removeDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public abstract void removeDesignChangeListener(WGDesignChangeListener changeListener);
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#dispose()
     */
    public abstract void dispose();
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.RealDesignProvider#isCacheable()
     */
    public abstract boolean isNotifying();

}
