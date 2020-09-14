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

import java.io.IOException;
import java.io.StringWriter;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringEscapeUtils;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WebTMLFunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.so.ManagedObject;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext.ListVarContainer;

public class ItemExpression {
    
    private static final Pattern PATTERN_STRING_LITERAL = Pattern.compile("^'(.*)'$");
    
    private static final Pattern PATTERN_NUMBER_LITERAL = Pattern.compile("^(-??[0-9]+.??[0-9]*)$");
    
    private static final DecimalFormat DECIMALFORMAT_NUMBER_LITERAL = new DecimalFormat("0.#", DecimalFormatSymbols.getInstance(Locale.ENGLISH)); 

    
    private String _fullItemName;
    private String _itemName;
    private String _itemCall = null;
    private boolean _enhancedExpressions;
    private TMLContext _context;
    private List<Runnable> _afterRetrieval = new ArrayList<Runnable>();

    public ItemExpression(TMLContext context, String itemName) {
        _context = context;
        _fullItemName = itemName;
        _itemName = itemName;
        _enhancedExpressions = context.isEnhancedItemExpressions();
        
        if (_enhancedExpressions && _fullItemName != null) {
            int dotPos = _fullItemName.indexOf(".");
            if (dotPos != -1) {
                _itemName = _fullItemName.substring(0, dotPos);
                _itemCall = _fullItemName.substring(dotPos + 1);
            }
        }
        
    }
    
    public Object retrieve(Map<String,Object> params, List<Object> unnamedParams, Map<String,Object> objects) throws WGException {
        
        Object itemValue = retrieveItemValue();
        
        if (_itemCall != null && itemValue != null) {
            itemValue = performItemCall(itemValue, params, unnamedParams, objects);
        }
        
        for (Runnable cleanup : _afterRetrieval) {
            cleanup.run();
        }
        
        return itemValue;
        
    }

    private Object performItemCall(Object object, Map<String, Object> params, List<Object> unnamedParams, Map<String,Object> systemInjections) throws WGException {
        
        WGA wga = WGA.get(_context);
        Map<String,Object> allNamedParams = new HashMap<String, Object>();
        if (systemInjections != null) {
            for (Map.Entry<String,Object> entry : systemInjections.entrySet()) {
                allNamedParams.put("$" + entry.getKey(), entry.getValue());
            }
        }
        if (params != null) {
            allNamedParams.putAll(params);
        }
        
        if(object instanceof ListVarContainer)
        	object = ((ListVarContainer) object).getList();
        
        Object value = wga.tmlscript().callMethod(object, _itemCall, allNamedParams, unnamedParams);
        
        if (value instanceof List<?>) {
            @SuppressWarnings("unchecked")
            List<Object> valueList = (List<Object>) value;
            value = new ListVarContainer(valueList);
        }
        
        return value;
        
    }

    private Object retrieveItemValue() throws WGException {

        if (_itemName == null) {
            return null;
        }
        
        if (_enhancedExpressions) {
            
            // String Literal (Match against full itemname bc. the decimal point, if present, has been misinterpreted as call chain. Also clear call information on match) 
            Matcher stringLiteralMatcher = PATTERN_STRING_LITERAL.matcher(_fullItemName);
            if (stringLiteralMatcher.matches()) {
                try {
                    String stringContent = stringLiteralMatcher.group(1);
                    StringWriter out = new StringWriter();
                    StringEscapeUtils.unescapeJavaScript(out, stringContent);
                    _itemCall = null;
                    return out.toString();
                }
                catch (IOException e) {
                    _context.addwarning("Exception interpreting string literal in item expression: " + _fullItemName, true);
                    
                }
            }
            
            // Number literal (Match against full itemname bc. the decimal point, if present, has been misinterpreted as call chain. Also clear call information on match)
            Matcher numberLiteralMatcher = PATTERN_NUMBER_LITERAL.matcher(_fullItemName);
            if (numberLiteralMatcher.matches()) {
                try {
                    String numberContent = numberLiteralMatcher.group(1);
                    _itemCall = null;
                    return DECIMALFORMAT_NUMBER_LITERAL.parse(numberContent);
                }
                catch (ParseException e) {
                    _context.addwarning("Parse error interpreting number literal in item expression: " + _fullItemName + ": " + e.getMessage(), true);
                }
            }
            
            // Boolean literal
            if (Boolean.TRUE.toString().equals(_itemName)) {
                return true;
            }
            if (Boolean.FALSE.toString().equals(_itemName)) {
                return false;
            }
            
            // Null literal
            if ("null".equals(_itemName)) {
                return null;
            }
            
            // Undefined literal
            if ("undefined".equals(_itemName)) {
                return ExpressionEngineFactory.getTMLScriptEngine().getUndefined();
            }
            
            if (_itemName.startsWith("$")) {
                
                // Controllers
                if (_itemName.equals("$pc")) {
                    Object pc = retrievePortletController();
                    if (pc != null) {
                        return pc;
                    }
                }
                
                if (_itemName.equals("$mc")) {
                    Object mc = retrieveModuleController();
                    if (mc != null) {
                        return mc;
                    }
                }
                
                // WebTML substitution variables
                Object substitutedValue = new WebTMLFunctionArgumentSubstitutor(WGA.get(_context)).getArgumentValue(_itemName);
                if (substitutedValue != null) {
                    if (substitutedValue instanceof List<?>) {
                        @SuppressWarnings("unchecked")
                        List<Object> substitutedValueList = (List<Object>) substitutedValue;
                        substitutedValue = new ListVarContainer(substitutedValueList);
                    }
                    return substitutedValue;
                }
            }
        
        }
        
        // WebTML variables
        Object value = _context.retrieveVar(_itemName);
        if (!(value instanceof NullPlaceHolder)) {
            return value;
        }

        // Mapped item values
        Object result = _context.getMappedItemValue(_itemName.toLowerCase());
        if (result != null) {
            return result;
        }

        // Content items
        WGDocument doc = _context.getdocument();
        if (doc.hasItem(_itemName)) {
            return doc.getItemValue(_itemName);
        }
                
        // Symbolizes that no value could be found
        return new NullPlaceHolder();
    
        
    }

    private Object retrieveModuleController() throws WGException {
        final ManagedObject controller = _context.fetchModuleController();
        if (controller != null) {
            controller.beforeUsage();
            _afterRetrieval.add(new Runnable() {
                @Override
                public void run() {
                    try {
                        controller.afterUsage();
                    }
                    catch (WGException e) {
                        _context.getlog().error("Exception in module controller cleanup", e);
                    }
                }
                
            });
            return controller.getObject();
        }
        else {
            return null;
        }
    }

    private Object retrievePortletController() throws WGException {
        WGA wga = WGA.get(_context);
        final TMLPortlet portlet = _context.getportlet();
        if (portlet == null) {
            return null;
        }
        
        final ScopeObject controller = portlet.getState().fetchController(wga);
        if (controller == null) {
            return null;
        }

        controller.beforeUsage();
        _afterRetrieval.add(new Runnable() {
            @Override
            public void run() {
                
                try {
                    controller.afterUsage();
                }
                catch (WGException e) {
                    _context.getlog().error("Exception cleaning up from portlet controller", e);
                }
            }
            
        });
        
        return controller.getObject();
    }
    
}
