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

package de.innovationgate.wgpublisher.webtml.portlet;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.gson.JsonObject;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.DescriptificationConfig;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.so.ScopeObjectContext;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class PortletScopeObjectContext implements ScopeObjectContext {

    private TMLPortlet _portlet;
    
    public static final String PORTLETITEM_CONFIGPROPNAMES = "$configPropertyNames";

    public PortletScopeObjectContext(TMLPortlet portlet) {
        _portlet = portlet;
    }
    
    @Override
    public JsonObject transformStateToExtract(JsonObject state, DesignResourceReference ref) throws WGException {
        return state;
        
    }

    @Override
    public JsonObject transformStateToInject(JsonObject state, DesignResourceReference ref) throws WGException {
        return state;
    }
    
    @Override
    public void beforeUsage(ScopeObject so) throws WGException {
        
        TMLScript tmlScript = WGA.get().tmlscript();
        Object obj = so.getObject();
        if (tmlScript.hasProperty(obj, TMLPortletState.PROP_CONFIG)) {
            @SuppressWarnings("unchecked")
            Map<Object,Object> config = (Map<Object,Object>) tmlScript.callMethod(obj, TMLPortletState.PROP_CONFIG);
            
            // Copy all items to the config object
            List<Object> configPropKeys =  _portlet.itemlist(PORTLETITEM_CONFIGPROPNAMES);
            for (Object configPropKey : configPropKeys) {
                Object value = _portlet.item(String.valueOf(configPropKey));
                value = ExpressionEngineFactory.getTMLScriptEngine().scriptify(value, config);
                config.put(configPropKey, value);
            }
            
            // Remove props no longer available as portlet item
            for (Object propName : config.keySet()) {
                if (!configPropKeys.contains(String.valueOf(propName))) {
                    config.remove(propName);
                }
            }
        }
        
    }
    
    @Override
    public void afterUsage(ScopeObject so) throws WGException {
        
        Object obj = so.getObject();
        RhinoExpressionEngine scriptEngine = ExpressionEngineFactory.getTMLScriptEngine();
        TMLScript tmlScript = WGA.get().tmlscript();
        if (scriptEngine.hasProperty(obj, TMLPortletState.PROP_CONFIG)) {
            @SuppressWarnings("unchecked")
            Map<Object,Object> config = (Map<Object, Object>) tmlScript.callMethod(obj, TMLPortletState.PROP_CONFIG);
            
            if (config != null) {
                
                // Copy all config properties to portlet items
                Set<Object> propKeys  = config.keySet();
                Set<String> currentConfigPropKeys = new HashSet<>();
                for (Object propKey : propKeys) {
                    Object propValue = config.get(propKey);
                    propValue = scriptEngine.descriptify(propValue, Object.class, new DescriptificationConfig().convertObjectsToJSON());
                    _portlet.setitem(String.valueOf(propKey), propValue);
                    currentConfigPropKeys.add(String.valueOf(propKey));
                }

                // Store key list, remove portlet items no longer available as config property
                List<Object> oldConfigPropkey = _portlet.itemlist(PORTLETITEM_CONFIGPROPNAMES);
                _portlet.setitem(PORTLETITEM_CONFIGPROPNAMES, currentConfigPropKeys);
                oldConfigPropkey.removeAll(currentConfigPropKeys);
                for (Object itemName : oldConfigPropkey) {
                    _portlet.removeitem(String.valueOf(itemName));
                }

            }
        }
        
    }

}
