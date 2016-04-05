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

package de.innovationgate.wga.server.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGAServerException;

/**
 * This object represents an OpenWGA domain, an entity which collects OpenWGA databases to use some shared functionality, most prominently the same authentication.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Domain {
    
    private WGA _wga;
    private String _name;

    protected Domain(WGA wga, String name) throws WGException {
        _wga = wga;
        _name = name;
        if (getCore() == null) {
            throw new WGAServerException("Domain " + name + " does not exist");
        }
    }


    /**
     * Returns the internally used domain core object
     * @throws WGException
     */
    public WGADomain getCore() throws WGException {
        return _wga.getCore().getDomains(_name);
    }
    
    
    /**
     * Returns a List of database keys of the OpenWGA applications in this domain, excluding data sources
     */
    public List<String> getAppKeys() throws WGException {
        
        List<String> keys = new ArrayList<String>();
        for (WGDatabase db :  _wga.getCore().getDatabasesForDomain(_name)) {
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                keys.add(db.getDbReference());
            }
        }
        return keys;
    }
    
    /**
     * Returns a List of database keys of all OpenWGA data sources of this domain, excluding full applications
     */
    public Collection<String> getDataSourceKeys() throws WGException {
        
        List<String> keys = new ArrayList<String>();
        for (WGDatabase db :  _wga.getCore().getDatabasesForDomain(_name)) {
            if (!db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                keys.add(db.getDbReference());
            }
        }
        return keys;
    }
    
    /**
     * Returns a List of database keys of all OpenWGA databases in this domain, including apps and data sources
     */
    public Collection<String> getDatabaseKeys() throws WGException {
        
        List<String> keys = new ArrayList<String>();
        for (WGDatabase db :  _wga.getCore().getDatabasesForDomain(_name)) {
            keys.add(db.getDbReference());
        }
        return keys;
    }
    
    /**
     * Returns an Auth object providing authentication services for this domain
     * @throws WGAServerException
     */
    public Auth auth() throws WGException {
        return new Auth(_wga, _name);
    }


    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_name == null) ? 0 : _name.hashCode());
        return result;
    }


    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Domain other = (Domain) obj;
        if (_name == null) {
            if (other._name != null)
                return false;
        }
        else if (!_name.equals(other._name))
            return false;
        return true;
    }


    /**
     * Returns the name of the domain
     */
    public String getName() throws WGException {
        return _name;
    }
    
    /**
     * Reopens all open sessions of all databases on the domain, using the current authentication
     */
    public void reopenSessions() throws WGException {
        for (String dbKey : getDatabaseKeys()) {
            Database db = _wga.database(dbKey);
            db.reopen();
        }
    }

}
