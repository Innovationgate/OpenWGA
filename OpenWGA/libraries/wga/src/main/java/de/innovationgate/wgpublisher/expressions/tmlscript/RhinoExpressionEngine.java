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
package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.dom4j.Document;
import org.dom4j.DocumentException;

import com.google.gson.JsonElement;
import com.thoughtworks.xstream.converters.SingleValueConverter;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.CallMethodConfig;
import de.innovationgate.wga.server.api.DescriptificationConfig;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public interface RhinoExpressionEngine extends ExpressionEngine, SingleValueConverter {
	
	public static final String TL_ACTIONDEFINITION = "actionDefinition";
    public static final String TL_ACTIONLOCATOR = "actionLocator";
    public static final String TL_SCRIPTNAME = "scriptName";
    public static final String TL_ROOTSCOPE = "rootScope";
    public static final String TL_ORIGINALUSER = "originalUser";
    public static final String TL_WGACONTEXT = "wgaContext";
    
    /**
     *  Scriptlet resolve level, default is LEVEL_SYSTEM_MACROS
     */
    public static final String  SCRIPTLETOPTION_LEVEL = "level";
    public static final Integer LEVEL_SYSTEM_MACROS = new Integer(1);
    public static final Integer LEVEL_MACROS = new Integer(2);
    public static final Integer LEVEL_SCRIPTLETS = new Integer(3);
    
    /**
     * should scriptlet "imgurl" generate dataURLs? - Boolean(true/false) - default is false 
     */
    public static final String SCRIPTLETOPTION_IMAGEURL_AS_DATAURL = "imgurlAsDataURL";
    
    /**
     * Objects to inject into the runtime of custom scriptlets - Map<String,Object>
     */
    public static final String  SCRIPTLETOPTION_OBJECTS = "objects";
    
    public static final int DEFAULT_SCRIPTTIMEOUT = 0;
    public static final String PARAM_ACTIONDEFINITION = "$actiondefinition";
    public static final String PARAM_ACTIONLOCATOR = "$actionLocator";
    public static final String PARAM_SCRIPTNAME = "$scriptName";
    public static final String PARAM_SCRIPTTIMEOUT = "$tmlscripttimeout";
    
    
    public static final int TYPE_NOTMLSCRIPT = -1;
    public static final int TYPE_SCRIPTABLE = 1;
    public static final int TYPE_XMLOBJECT = 2;
    public static final int TYPE_XMLLIST = 3;
    public static final int TYPE_UNDEFINED = 4;
    public static final int TYPE_NAN = 5;
    
    public static final String SESSIONVAR_ENABLE_SCRIPTSTACKTRACE = "$enableScriptStackTrace";
    
    /**
     * Extdata fields for TMLScript modules that indicates the way how a TMLScript object should
     * be created from a TMLScript module.
     */
    public static final String EXTDATA_OBJECTSTRATEGY = RhinoExpressionEngine.class.getName() + ":ObjectStrategy";
    public static final String CONSTRUCTOR_PROPERTY = "__constructor__";
	
    public void init(WGACore core);
	public ExpressionResult evaluateExpression(String expression, TMLContext context, int type, Map<String,Object> objects);
	public String resolveScriptlets(Object input, TMLContext context, Map<String,Object> scriptletOptions) throws WGException;
    public long getScriptCacheCurrentSize();
    public int getScriptCacheMaxSize();
    public int determineTMLScriptType(Object obj);
    public List<?> convertXMLListToList(Object obj);
    public List<?> xpathTMLScriptBean(Object obj, String xpath);
    
    public String convertScriptableToJson(Object obj);
    public Object convertJsonToScriptable(String json);
    
    public <T> T descriptify(Object obj, Class<T> expectedType, DescriptificationConfig config) throws WGException;
    public Object scriptify(Object obj, Object scope) throws WGException;
    
    public Document convertNativeXMLtoDOM(Object xmlObj) throws DocumentException;
    public void debug();
    public void close();
    public void clearCache();
    public void enableDebugger();
    public void disableDebugger();
    public boolean isDebugEnabled();
    public Object createObject(WGA wga, TMLScript.ObjectType objectType, TMLAction action, FunctionArgumentSubstitutor substitutor, Map<String, Object> namedParams, List<Object> unnamedParams) throws WGException;
    public ExpressionResult callMethod(WGA wga, Object controller, String method, FunctionArgumentSubstitutor substitutor, Map<String,Object> namedParams, List<Object> unnamedParams, CallMethodConfig config) throws WGException;
    public TMLScriptGlobal createGlobal(String name, int type, Object ref) throws WGException;
    public Object provideGlobal(WGA wga, TMLScriptGlobal global) throws WGException;
    public Serializable serializeScriptable(TMLContext context, Object scriptable) throws WGException;
    public JsonElement extractProperty(Object controller, String prop);
    public void injectProperty(Object controller, String prop, JsonElement state);
    public boolean hasProperty(Object controller, String prop);
    public TMLScriptObjectMetadata getTmlscriptObjectMetadata(WGA wga, Design design) throws WGException;
    public Class<?> getScriptableType();
    public Object getUndefined();
    public boolean scriptableEquals(Object o1, Object o2);
    

}
