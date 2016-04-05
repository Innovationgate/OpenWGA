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
package de.innovationgate.wgpublisher.webtml.utils;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.utils.WGUtils;


/**
 * serverside bean which represents an ajax action definition
 * used to render the correponding clientside javascript object in ajax action calls
 * like WGA.ajax.action(<actionDef>)
 */
public class AjaxActionDefinition {
    
    private String _actionLink;
    private String _divTagId;
    private boolean _graydiv;
    private boolean _keepParams = true;
    private Map<String,String> _additionalProps = new HashMap<String, String>(); 
    
    private String _tmlformSessionKey;
    private String _mode;

    private String _callbackFunction; // not used or rendered on serverside - just to complete the serverside definition

    public AjaxActionDefinition(String actionLink, String divTagId) {
        _actionLink = actionLink;
        _divTagId = divTagId;
        _graydiv = true;
    }
    
    public String toJavaScriptObject() {
        StringBuffer actionDef = new StringBuffer();
        actionDef.append("{");
        actionDef.append("action:'" + WGUtils.encodeJS(_actionLink) + "'");
        actionDef.append(", id:'" + WGUtils.encodeJS(_divTagId) + "'");        
        actionDef.append(", graydiv:" + Boolean.valueOf(_graydiv));
        actionDef.append(", keepParams: " + Boolean.valueOf(_keepParams));
        
        if (_mode != null) {
            actionDef.append(", mode:'" + WGUtils.encodeJS(_mode) + "'");            
        }
        if (_tmlformSessionKey != null) {
            actionDef.append(", tmlformSessionKey:'" + WGUtils.encodeJS(_tmlformSessionKey) + "'");            
        }
        
        for (Map.Entry<String,String> prop : _additionalProps.entrySet()) {
            actionDef.append(", " + prop.getKey() + ":" + prop.getValue());
        }
        
        actionDef.append("}");
        return actionDef.toString();
    }
    
    public String getActionLink() {
        return _actionLink;
    }

    public void setActionLink(String actionLink) {
        _actionLink = actionLink;
    }

    public boolean isGraydiv() {
        return _graydiv;
    }

    public void setGraydiv(boolean graydiv) {
        _graydiv = graydiv;
    }

    public String getMode() {
        return _mode;
    }

    public void setMode(String mode) {
        _mode = mode;
    }

    public String getDivTagId() {
        return _divTagId;
    }

    public void setDivTagId(String divTagId) {
        _divTagId = divTagId;
    }

    public String getTmlformSessionKey() {
        return _tmlformSessionKey;
    }

    public void setTmlformSessionKey(String tmlformSessionKey) {
        _tmlformSessionKey = tmlformSessionKey;
    }

    public boolean isKeepParams() {
        return _keepParams;
    }

    public void setKeepParams(boolean keepParams) {
        _keepParams = keepParams;
    }

    public Map<String, String> getAdditionalProps() {
        return _additionalProps;
    }
 


}
