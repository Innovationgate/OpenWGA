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

package de.innovationgate.wgpublisher.webtml.env;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.Warning;

public class IndependentDesignContext implements TMLDesignContext {
    
    private IndependentTMLScriptEnvironment _environment;
    private DesignResourceReference _baseReference;
    public IndependentDesignContext(IndependentTMLScriptEnvironment env, WGDatabase designDB, String baseDesignReference) {
        _environment = env;
        _baseReference = new DesignResourceReference((designDB != null ? designDB.getDbReference() : _environment.getMainDesignDB().getDbReference()), baseDesignReference).normalize();
    }

    public WGDatabase getDesignDB() {
        return _environment.fetchDB(_baseReference.getDesignApp());
    }

    public BaseTagStatus getTag() {
        return null;
    }

    public void addWarning(TMLContext cx, String msg, boolean severe, Throwable cause) {
        Warning warning = new Warning(null, (TMLContext) cx, msg, severe);
        _environment.getWarnings().add(warning);
        
        if (_environment.getCore().getWgaConfiguration().isWarningsOutputOnConsole()) {
            if (cause != null) {
                _environment.getCore().getLog().warn(warning.getConsoleText(), cause);
            }
            else {
                _environment.getCore().getLog().warn(warning.getConsoleText());
            }
        }
    }

    public boolean isTagAvailable() {
        return false;
    }

    public TMLDesignContext createContextDelegate(WGDatabase designdb, String baseReference) {
        return new IndependentDesignContext(_environment, designdb, baseReference);
    }

    public DesignResourceReference getBaseReference() {
        return _baseReference;
    }

    public Version getVersionCompliance() {

        Version complianceVersion = WGAVersion.VERSION;
        WGDatabase designDB = getDesignDB();
        if (designDB != null) {
            return designDB.getComplianceVersion();
        }
        
        return complianceVersion;
    }





    @Override
    public TMLPortlet getPortlet() throws WGAPIException {
        return null;
    }

    @Override
    public TMLOption getOption(String name) {
        return _environment.getOptions().get(name);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_baseReference == null) ? 0 : _baseReference.hashCode());
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
        IndependentDesignContext other = (IndependentDesignContext) obj;
        if (_baseReference == null) {
            if (other._baseReference != null)
                return false;
        }
        else if (!_baseReference.equals(other._baseReference))
            return false;
        return true;
    }

    @Override
    public void setOption(String name, Object value, String scope) {
        TMLOption option = new TMLOption(name, value, scope);
        _environment.getOptions().put(name, option);
    }
    
    @Override
    public void setDownwardOption(String name, Object value, String scope) {
        setOption(name, value, scope);
    }

    @Override
    public List<String> getOptionNames() {
        return new ArrayList<String>(_environment.getOptions().keySet());
    }

    @Override
    public String getMediaKey() throws WGException {
        if (_environment.getRequest() != null) {
            return WGA.get(_environment).call().getMediaKey();
        }
        else {
            return null;
        }
    }

    @Override
    public void removeOption(String name) throws WGException {
        _environment.getOptions().remove(name);            
    }
    
    @Override
    public Version getMinimumWGAVersion() {
        return CSConfig.getMinimumWGAVersion((CSConfig) getDesignDB().getAttribute(WGACore.DBATTRIB_CSCONFIG));
    }

    @Override
    public void setLocalVarOnModule(String name, Object value) throws WGAPIException {
        setLocalVar(name, value);
    }

    @Override
    public void setLocalVar(String name, Object value) throws WGAPIException {
        _environment.getPageVars().put(name, value);
    }

    @Override
    public Object retrieveLocalVar(String name) throws WGAPIException {
        if (_environment.getPageVars().containsKey(name)) {
            return _environment.getPageVars().get(name);
        }
        return new NullPlaceHolder();
    }

    @Override
    public Object removeLocalVar(String name) throws WGAPIException {
        if (_environment.getPageVars().containsKey(name)) {
            return _environment.getPageVars().remove(name);
        }
        else {
            return new NullPlaceHolder();
        }
    }
    
}