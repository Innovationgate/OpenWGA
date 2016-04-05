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

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

/**
 * A design context implementation for the server API, used when outside any WebTML/TMLScript environment
 */
public class DesignContext implements TMLDesignContext {
    
    private WGDatabase _db;
    private String _ref;
    private Map<String,Object> _localVars = new HashMap<String, Object>();
    
    protected DesignContext(WGDatabase db, String ref) {
        _db = db;
        _ref = ref;
    }

    @Override
    public BaseTagStatus getTag() {
        return null;
    }

    @Override
    public WGDatabase getDesignDB() {
        return _db;
    }

    @Override
    public DesignResourceReference getBaseReference() {
        return new DesignResourceReference(_db.getDbReference(), _ref).normalize();
    }

    @Override
    public Version getVersionCompliance() {

        Version complianceVersion = WGAVersion.VERSION;
        if (_db != null) {
            complianceVersion = _db.getComplianceVersion();
        }
        
        return complianceVersion;
        
    }

    @Override
    public void addWarning(TMLContext cx, String msg, boolean severe, Throwable cause) {
    }

    @Override
    public TMLDesignContext createContextDelegate(WGDatabase designdb, String baseReference) {
        return new DesignContext(designdb, baseReference);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_db == null) ? 0 : _db.hashCode());
        result = prime * result + ((_ref == null) ? 0 : _ref.hashCode());
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
        DesignContext other = (DesignContext) obj;
        if (_db == null) {
            if (other._db != null)
                return false;
        }
        else if (!_db.equals(other._db))
            return false;
        if (_ref == null) {
            if (other._ref != null)
                return false;
        }
        else if (!_ref.equals(other._ref))
            return false;
        return true;
    }

    @Override
    public TMLPortlet getPortlet() throws WGAPIException {
        return null;
    }

    @Override
    public TMLOption getOption(String name) {
        return null;
    }

    @Override
    public void setOption(String name, Object value, String scope) throws WGNotSupportedException {
        throw new WGNotSupportedException("Setting options not supported on this design context");
    }
    
    @Override
    public void setDownwardOption(String name, Object value, String scope) throws WGNotSupportedException {
        setOption(name, value, scope);
    }

    @Override
    public List<String> getOptionNames() {
        return Collections.emptyList();
    }

    @Override
    public String getMediaKey() throws WGException {
        return null;
    }

    @Override
    public void removeOption(String name) throws WGNotSupportedException {
        throw new WGNotSupportedException("Options not supported on this design context");
    }

    @Override
    public Version getMinimumWGAVersion() {
        if (_db != null) {
            return CSConfig.getMinimumWGAVersion((CSConfig) _db.getAttribute(WGACore.DBATTRIB_CSCONFIG));
        }
        else {
            return getVersionCompliance();
        }
    }
    
    @Override
    public void setLocalVarOnModule(String name, Object value) throws WGAPIException {
        setLocalVar(name, value);
    }

    @Override
    public void setLocalVar(String name, Object value) throws WGAPIException {
        _localVars.put(name, value);
    }

    @Override
    public Object retrieveLocalVar(String name) throws WGAPIException {
        if (_localVars.containsKey(name)) {
            return _localVars.get(name);
        }
        return new NullPlaceHolder();
    }

    @Override
    public Object removeLocalVar(String name) throws WGAPIException {
        if (_localVars.containsKey(name)) {
            return _localVars.remove(name);
        }
        else {
            return new NullPlaceHolder();
        }
    }

}
