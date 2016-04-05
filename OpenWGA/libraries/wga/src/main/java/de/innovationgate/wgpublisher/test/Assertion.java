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
package de.innovationgate.wgpublisher.test;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import de.innovationgate.utils.WGUtils;

/**
 * represents an assertion, used by TestCore to store assertions which are tested
 * in different assert... methods.
 *
 */
public class Assertion {
    
    public static final String TYPE_ASSERT_TRUE = "assertTrue";
    public static final String TYPE_ASSERT_EQUALS = "assertEquals";
    public static final String TYPE_ASSERT_NOT_EQUALS = "assertNotEquals";
    public static final String TYPE_ASSERT_LARGER = "assertLarger";
    public static final String TYPE_ASSERT_JAVA = "assertJava";
    
    private String _type;
    private Date _time;
    private String _title;
    private String _category;    
    private boolean _result = false;
    private boolean _executed = false;
    private String _expression;
    private String _contextPath;
    private boolean _expressionError = false;
    private String _expressionErrorMsg;
    private String _tmlModule = null;
    
    private String _valueObj1;
    private String _valueObj2;
    
    private transient Object _rawValueObj1;
    private transient Object _rawValueObj2;
    
    private boolean _preregistered;
    private String _id;
    
    private static final DateFormat DATEFORMAT = new SimpleDateFormat("dd.MM.yyyy - HH:mm:ss.SSS z");
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(DATEFORMAT.format(_time) + " | ");
        buf.append("TYPE: " + _type + " | ");
        buf.append("PREREGISTERED: " + _preregistered + " | ");
        buf.append("RESULT: " + _result + " | ");
        buf.append("CATEGORY: " + _category + " | ");
        buf.append("TITLE: " + _title + " | ");                
        buf.append("CONTEXTPATH: " + _contextPath + " | ");
        if (_type.equals(TYPE_ASSERT_TRUE)) {
            buf.append("EXPRESSION_ERROR: " + _expressionError + " | ");
            buf.append("EXPRESSION_ERROR_MSG: " + _expressionErrorMsg + " | ");        
            buf.append("EXPRESSION: " + _expression);
        } else if (_type.equals(TYPE_ASSERT_EQUALS)) {
            buf.append("VALUE1: " + _valueObj1 + " | ");
            buf.append("VALUE2: " + _valueObj2 + " | ");
        }
        
        return buf.toString();
    }
    
    public Assertion(String type, String title, String category) {
        this(type, title, category, null);
    }
    
    public Assertion(String type, String title, String category, String id) {
        _type = type;
        _title = title;
        _category = category;
        _id = id;
    }
    
    
    /**
     * no-arg constructor for XStream on JRockIt VM
     *
     */
    @SuppressWarnings("unused")
    private Assertion() {        
    }
    
    public String getCategory() {
        return _category;
    }
    
    public String getShortCategory() {
        
        int pointPos = _category.lastIndexOf(".");
        if (pointPos != -1) {
            return _category.substring(pointPos + 1);
        }
        else {
            return _category;
        }
        
    }
    public boolean isResult() {
        return _result;
    }
    public void setResult(boolean result) {
        _result = result;
    }
    public String getTitle() {
        return _title;
    }
    public String getExpression() {
        return _expression;
    }

    public void setExpression(String expression) {
        _expression = expression;
    }

    public boolean isExpressionError() {
        return _expressionError;
    }

    public void setExpressionError(boolean expressionError) {
        _expressionError = expressionError;
    }

    public String getExpressionErrorMsg() {
        return _expressionErrorMsg;
    }

    public void setExpressionErrorMsg(String expressionErrorMsg) {
        _expressionErrorMsg = expressionErrorMsg;
    }

    public Date getTime() {
        return _time;
    }

    public void setTime(Date time) {
        _time = time;
    }

    public String getContextPath() {
        return _contextPath;
    }

    public void setContextPath(String contextPath) {
        _contextPath = contextPath;
    }

    public String getType() {
        return _type;
    }

    public String getValueObj1() {
        return _valueObj1;
    }

    public void setValueObj1(Object obj) {
        if (obj != null) {
            _rawValueObj1 = obj;
            _valueObj1 = stringify(obj);
        }
    }

    private String stringify(Object obj) {
        
        if (obj instanceof List<?>) {
            return "[" + WGUtils.serializeCollection((List<?>) obj, " , ") + "]";
        }
        else if (obj instanceof Date) {
            return DATEFORMAT.format(obj);
        }
        
        else { 
            return obj.toString();
        }
    }

    public String getValueObj2() {
        return _valueObj2;
    }

    public void setValueObj2(Object obj) {
        if (obj != null) {
            _rawValueObj2 = obj;
            _valueObj2 = stringify(obj);
        }
    }

    public boolean isPreregistered() {
        return _preregistered;
    }

    public void setPreregistered(boolean preregistered) {
        _preregistered = preregistered;
    }

    public String getTmlModule() {
        return _tmlModule;
    }

    public void setTmlModule(String tmlModule) {
        _tmlModule = tmlModule;
    }

    public boolean isExecuted() {
        return _executed;
    }

    public void setExecuted(boolean executed) {
        _executed = executed;
    }

    public String getId() {
        return _id;
    }

    public Object getRawValueObj1() {
        return _rawValueObj1;
    }

    public Object getRawValueObj2() {
        return _rawValueObj2;
    }
    
    public String getFailureMessage() {
        
        if (getType().equals(TYPE_ASSERT_EQUALS)) {
            return "Must be equal, but:\n" + getValueObj1() + "\n!=\n" + getValueObj2();
        }
        if (getType().equals(TYPE_ASSERT_NOT_EQUALS)) {
            return "Must not be equal, but:\n" + getValueObj1() + "\n==\n" + getValueObj2();
        }
        if (getType().equals(TYPE_ASSERT_TRUE)) {
            return "Expression must be true, but is false:\n" + getExpression();
        }
        if (getType().equals(TYPE_ASSERT_LARGER)) {
            return "Must be larger, but:\n" + getValueObj1() + "\n<=\n" + getValueObj2(); 
        }
        if (getType().equals(TYPE_ASSERT_JAVA)) {
            return "Java Assertion was false:\n" + getExpression();
        }
        
        return "No failure information";
        
        
    }
    
    public String getTestClass() {
        if (_tmlModule != null) {
            
            String pkg;
            String clz;
            
            int lastColonPos = _tmlModule.lastIndexOf(":");
            if (lastColonPos != -1) {
                 pkg = _tmlModule.substring(0, lastColonPos) + ".";
                 clz = _tmlModule.substring(lastColonPos + 1);
            }
            else {
                pkg = "";
                clz = _tmlModule;
            }
            if (clz.length() >= 2) {
                clz = Character.toUpperCase(clz.charAt(0)) + clz.substring(1);
            }
            return TestCore.TESTCLASS_BASE_PACKAGE + WGUtils.strReplace(pkg, ":", ".", true) + clz;
        }
        else {
            return TestCore.TESTCLASS_BASE_PACKAGE + "Global"; 
        }
    }
    
    
    
}
