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
package de.innovationgate.wgpublisher.webtml;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.ResultIterator;
import de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Evaluate extends Base {
		
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String language = null;
	private String type = null;
    private String expression = null;
    private String _timeout;
    
    public static class Status extends BaseTagStatus implements ResultSetTagStatus {
        
        protected String language;
        protected String type;
        
        public String getResultLanguage() {
            return MULTILANGUAGE_RESULT;
        }
        
        public void addResultSet(WGResultSet resultSet, String language) {}
        
        public ResultIterator getResultIterator() throws WGBackendException, WGAPIException {
            if (result instanceof List) {
                return new ResultIterator((List) result);
            }
            else if (result instanceof WGResultSet) {
                WGResultSet resultSet = (WGResultSet) result;
                return new ResultIterator(resultSet.getResultIterator(), resultSet);
            }
            else {
                return null;
            }
        }
        
        public int results() {
            if (result instanceof List) {
                return ((List) result).size();
            }
            else {
                return 0;
            }
        }
        
        @Override
        public void initAttributeDelegates(Base tag) {

            Evaluate evTag = (Evaluate) tag;
            this.type = evTag.getType();
            this.language = evTag.getLanguage();
            
            super.initAttributeDelegates(tag);
        }
        
    }
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }

	/**
	 * @throws WGAPIException 
	 * @see TMLTag#doStartTag()
	 */
	public void tmlEndTag() throws TMLException, WGAPIException {
	
	        Status status = (Status) getStatus();
			de.innovationgate.webgate.api.WGContent content = this.getTMLContext().content();
			ExpressionEngine engine;
			engine = ExpressionEngineFactory.getEngine(status.language);
			if (engine == null) {
				this.addWarning("Unknown expression type: " + status.language, true);
				return;
			}

			int exprType = (status.type.equals("script") ? ExpressionEngine.TYPE_SCRIPT : ExpressionEngine.TYPE_EXPRESSION);
			String expr = this.getResultString(false);
            
            Map additionalObjects = new HashMap();
            String timeoutStr = getTimeout();
            if (timeoutStr != null) {
                try {
                    additionalObjects.put(RhinoExpressionEngine.PARAM_SCRIPTTIMEOUT, Integer.valueOf(timeoutStr));
                }
                catch (NumberFormatException e) {
                  addWarning("Unable to set timeout value because it cannot be parsed as a number", false);
                }
            }
            
            String tmlScriptDebug = getPageContext().getRequest().getParameter("tmlscriptDebug");
            if (tmlScriptDebug != null && tmlScriptDebug.equals(getId())) {
                additionalObjects.put("$tmlscriptDebug", new Boolean(true));
            }
            
            additionalObjects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, getTagDescription());
			ExpressionResult result = engine.evaluateExpression(expr, this.getChildTagContext(), exprType, additionalObjects);
			
			if (result.isError()) {
			    addExpressionWarning(expr, result);
			}
			this.setResult(result.getResult());

	}




    /**
	 * Gets the type
	 * @return Returns a String
	 */
	public String getLanguage() {
		return this.getTagAttributeValue("language", language, this.getDefaultExpressionLanguage());
	}
	
	public String getXplanguage() {
		return this.getLanguage();
	}
	/**
	 * Sets the type
	 * @param type The type to set
	 */
	public void setLanguage(String type) {
		this.language = type;
	}
	
	public void setXplanguage(String type) {
		this.setLanguage(type);
	}

	/**
	 * Returns the type.
	 * @return String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", type, "script");
	}

	/**
	 * Sets the type.
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

    /**
     * @return Returns the timeout.
     */
    public String getTimeout() {
        return this.getTagAttributeValue("timeout", _timeout, null);
    }




    /**
     * @param timeout The timeout to set.
     */
    public void setTimeout(String timeout) {
        _timeout = timeout;
    }









    public String getExpression() {
        return getTagAttributeValue("expression", expression, null);
    }




    public void setExpression(String expression) {
        this.expression = expression;
    }




    public void tmlStartTag() throws TMLException, WGException {
       
        Status status = (Status) getStatus();
        
        String expr = getExpression();
        if (expr != null) {
            status.type = "expression";
            setResult(expr);
            setEvalBody(false);
        }
        super.tmlStartTag();
        
    }




    

}

