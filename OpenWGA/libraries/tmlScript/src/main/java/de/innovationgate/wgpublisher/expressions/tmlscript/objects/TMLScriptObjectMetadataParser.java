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

package de.innovationgate.wgpublisher.expressions.tmlscript.objects;

import java.util.Arrays;
import java.util.List;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wgpublisher.events.ApplicationEventPath;
import de.innovationgate.wgpublisher.events.EventPath;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata.Method;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata.PortletEventListener;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.RhinoScope;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;

public class TMLScriptObjectMetadataParser {
    
    public TMLScriptObjectMetadata parseTMLScriptObjectMetaData(TMLAction objectDefinition, Function constructor) throws WGException, WGIllegalArgumentException {
        
        TMLScriptObjectMetadata md = new TMLScriptObjectMetadata();
        if (constructor == null) {
            return md;
        }
        
        // Parse properties
        Scriptable prototype = (Scriptable) ScriptableObject.getProperty(constructor, "prototype");        
        for (Object id : ScriptableObject.getPropertyIds(prototype)) {
            
            String propId = String.valueOf(id);
            Object prop = ScriptableObject.getProperty(prototype, propId);
            if (prop instanceof Function) {
                
                Method method = new Method(propId);
                md.getMethods().put(propId, method);
                
                
                // Direct Access
                try {
                    Object directAccess = ScriptableObject.getProperty((Function) prop, RhinoScope.PROP_DIRECTACCESS);
                    if (directAccess instanceof Boolean) {
                        method.setDirectAccess((Boolean) directAccess);
                    }
                }
                catch (EvaluatorException e) {
                    // Might get thrown from some object types (wrapped Java classes) if not found. Ignore.
                }
                
                // listenToAppEvents()
                try {
                    Object listenToAppEvents = ScriptableObject.getProperty((Function) prop, RhinoScope.PROP_LISTENTOAPPEVENTS);      
                    if (listenToAppEvents instanceof Object[]) {
                        
                        List<Object> events = Arrays.asList((Object[]) listenToAppEvents);
                        for (Object event : events) {
                            EventPath eventPath;
                            if (event instanceof String) {
                                eventPath = new ApplicationEventPath(objectDefinition.getModuleDatabase(), ApplicationEventPath.parseQualifiers(WGUtils.deserializeCollection((String) event, "/"))); 
                            }
                            else if (event instanceof List<?>) {
                                eventPath = new ApplicationEventPath(objectDefinition.getModuleDatabase(), ApplicationEventPath.parseQualifiers((List<?>) event));
                            }
                            else {
                                throw new WGIllegalArgumentException("Invalid type for defining event path: " + event.getClass().getName());
                            }
                            
                            md.getAppEventListeners().put(eventPath, propId);
                            
                        }
                        
                    }
                }
                catch (EvaluatorException e) {
                    // Might get thrown from some object types (wrapped Java classes) if not found. Ignore.
                }
                
                // listenToPortletEvents()
                try {
                    Object listenToPortletEvents = ScriptableObject.getProperty((Function) prop, RhinoScope.PROP_LISTENTOPORTLETEVENTS);      
                    if (listenToPortletEvents instanceof Object[]) {
                        
                        
                        
                        List<Object> arguments = Arrays.asList((Object[]) listenToPortletEvents);
                        if (arguments.size() > 0) {
    
                            Constants.AjaxMode ajaxMode = Constants.AjaxMode.NORMAL;
                            
                            // Optionally parse first parameter as AjaxMode
                            Object firstArgument = Context.jsToJava(arguments.get(0), Object.class);
                            if (firstArgument instanceof Constants.AjaxMode) {
                                ajaxMode = (Constants.AjaxMode) firstArgument;
                                arguments = arguments.subList(1, arguments.size());
                            }
                            
                            // Parse events
                            for (Object event : arguments) {
                                PortletEventListener portletEventListener = new PortletEventListener();
                                portletEventListener.setMethod(propId);
                                portletEventListener.setAjaxMode(ajaxMode.toString());
                                portletEventListener.setEventName((String) Context.jsToJava(event,String.class));
                                md.getPortletEventListeners().add(portletEventListener);
                            }
                        
                        }
                        
                    }
                }
                catch (EvaluatorException e) {
                    // Might get thrown from some object types (wrapped Java classes) if not found. Ignore.
                }
            }
            
        }
        
        return md;
        
    }

}
