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

import java.util.List;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.Root;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

public class WebTMLDesignContext implements TMLDesignContext {
    
    private BaseTagStatus _tag;
    private WebTMLContextEnvironment _environment;
    private DesignResourceReference _baseReference;
    private TMLPortlet _portlet;
    
    public WebTMLDesignContext(WebTMLContextEnvironment env, BaseTagStatus tag, String dbKey, String baseReference) {
        _environment = env;
        _tag = tag;
        _baseReference = new DesignResourceReference((dbKey != null ? dbKey : null), baseReference).normalize();
    }

    public WGDatabase getDesignDB() {
        
        String dbKey = getBaseReference().getDesignApp();
        
        // In case of staticTML we return the main context db as design db
        if (dbKey != null && dbKey.equals(WGACore.STATICTML_DESIGNDBKEY)) {
            return _tag.tmlContext.getmaincontext().db();
        }
        
        
        return _environment.fetchDB(dbKey);
        
    }

    public BaseTagStatus getTag() {
        return _tag;
    }

    public void addWarning(TMLContext cx, String msg, boolean severe, Throwable cause) {
        _tag.addWarning(cx, msg, severe, cause);
    }



    public TMLDesignContext createContextDelegate(WGDatabase designdb, String baseReference) {
        return new WebTMLDesignContext(_environment, _tag, (designdb != null ? designdb.getDbReference() : null), baseReference);
    }

    public DesignResourceReference getBaseReference() {
        
        if (_baseReference.getDesignApp() == null) { // If no design db was given we retrieve it "in time" from the tag status (might not have been available yet on creation of this design context)
            return new DesignResourceReference(_tag.getDesignDBKey(), _baseReference.getResourceName());
        }
        
        return _baseReference;
    }

    public Version getVersionCompliance() {

        Version complianceVersion = WGAVersion.VERSION;
        
        WGDatabase db = getDesignDB();
        if (db != null) {
            return db.getComplianceVersion();
        }
        
        return complianceVersion;
    }

    @Override
    public TMLOption getOption(String name) {
        return _tag.getOptionObject(name);
    }

    @Override
    public TMLPortlet getPortlet() throws WGAPIException {
        if (_portlet == null) {
            TMLOption option = getOption(Base.OPTION_PORTLET_NAMESPACE);
            if (option == null) {
                return null;
            }
            String nameSpace = (String) option.getValue();
            _portlet = _environment.getMainContext().getportletbykey(nameSpace);
        }
        return _portlet;
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
        WebTMLDesignContext other = (WebTMLDesignContext) obj;
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
        BaseTagStatus tag = getTag();
        while (tag != null) {
            tag.setOption(name, value, scope);
            if (tag instanceof Root.Status) {
                break;
            }
            tag = tag.getParentTag();
        } 
    }
    
    @Override
    public void setDownwardOption(String name, Object value, String scope) {
        getTag().setOption(name, value, scope);
    }
    
    @Override
    public void removeOption(String name) throws WGException {
        BaseTagStatus tag = getTag();
        while (tag != null) {
            tag.removeOption(name);
            if (tag instanceof Root.Status) {
                break;
            }
            tag = tag.getParentTag();
        }             
    }

    @Override
    public List<String> getOptionNames() {
        return getTag().getOptionNames();
    }

    @Override
    public String getMediaKey() {
        return getTag().getTMLModuleMediaKey();
    }

    @Override
    public Version getMinimumWGAVersion() {
        return CSConfig.getMinimumWGAVersion((CSConfig) getDesignDB().getAttribute(WGACore.DBATTRIB_CSCONFIG));
    }

    @Override
    public void setLocalVarOnModule(String name, Object value) throws WGAPIException {

        BaseTagStatus tag = _tag;
        while (!(tag instanceof Root.Status)) {
            tag = tag.getParentTag();
        }
        tag.setLocalVar(name, value);
        
    }

    @Override
    public void setLocalVar(String name, Object value) throws WGAPIException {
        _tag.setLocalVar(name, value);        
    }

    @Override
    public Object retrieveLocalVar(String name) throws WGAPIException {

       BaseTagStatus tag = _tag;
       while (true) {
            if (tag.hasLocalVar(name)) {
                return tag.getLocalVar(name);
            }
            
            if (tag instanceof Root.Status) {
                break;
            }
            else {
                tag = tag.getParentTag();
            }
        }
       
        return new NullPlaceHolder();
        
    }
    
    @Override
    public Object removeLocalVar(String name) throws WGAPIException {

        BaseTagStatus tag = _tag;
       while (true) {
            if (tag.hasLocalVar(name)) {
                return tag.removeLocalVar(name);
            }
            
            if (tag instanceof Root.Status) {
                break;
            }
            else {
                tag = tag.getParentTag();
            }
        }
       
       return new NullPlaceHolder();
        
    }

    
}