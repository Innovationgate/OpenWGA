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

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wgpublisher.DeployerException;
import de.innovationgate.wgpublisher.WGPDeployer;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.Root.Status;
import de.innovationgate.wgpublisher.webtml.utils.AjaxInfo;
import de.innovationgate.wgpublisher.webtml.utils.RootTagReceptor;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Option extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public class Status extends BaseTagStatus implements RootTagReceptor {

        private Root.Status rootTag;
        private long _startTime;

        @Override
        public void setRootTagStatus(de.innovationgate.wgpublisher.webtml.Root.Status root) {
            rootTag = root;
        }

        @Override
        public TMLContext getChildTagContext() {
            return childTMLContext;
        }

        @Override
        public boolean isPortletInclude() {
            return false;
        }

        @Override
        public AjaxInfo getPortletAJAXInfo() {
            return null;
        }

        public Root.Status getRootTagStatus() {
            return rootTag;
        }

        @Override
        public void setStartTime(long currentTimeMillis) {
            _startTime = currentTimeMillis;
        }

        @Override
        public long getStartTime() {
            return _startTime;
        }
        
        @Override
        public Map<String, Object> getLocalVarsToInherit() {
            return Collections.emptyMap();
        }
        
    }
	
	private String _name;
	private String _mode;
    protected String _scope;
    private String _expression;
    private String _item;
    
    public String getItem() {
        return getTagAttributeValue("item", _item, null);
    }

    public void setItem(String item) {
        this._item = item;
    }

    private String _default;
    private String _defaultexpression;
    private String _defaultitem;
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
    
	/**
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {
		
		String tagResult = this.getResultString();
		
		boolean setMode = false;
		String theMode = getMode();
		
		// If mode-Att is set, determine mode by it
		if (theMode != null) {
		    if (theMode.equals("set") || theMode.equals("write")) {
		        setMode = true;
		    }
		}
		
		// Else determine by the presence of tag content
		else {
		    setMode = !tagResult.equals("") || getExpression() != null || getItem() != null;
		}
		
		// Get mode 
		if (!setMode) {
			Object value = getOption(this.getName());
			if (value == null) {
			    // Try to find a default value
				String defaultValue = getDefault();
                String defaultExpression = getDefaultexpression();
                String defaultItem = getDefaultitem();
                if (defaultValue == null && defaultExpression == null && defaultItem == null) {
                    return;
                }
				
				if (defaultValue != null) {
				    value = defaultValue;
				}
				else if (defaultExpression != null) {
				    value = calculateOptionByExpression(defaultExpression);
				}
				else if (defaultItem != null) {
				    value = getTMLContext().item(defaultItem);
				}
			}
			
			if (value instanceof Inline.Pointer) {
			    performInlineInclude((Inline.Pointer) value);
			}
			else {
			    this.setResult(value);
			}
		}
		
		// Set mode
		else {
            this.setResultOutput(false);
            
            Object optionValue = retrieveOptionValue(tagResult);
            
			BaseTagStatus parent = getStatus().getParentTag();
			if (parent == null) {
				this.addWarning("Parent tag is no option receptor", true);
				return;
			}
			
            try {
                parent.setOption(this.getName(), optionValue, getScope());
            }
            catch (IllegalArgumentException e) {
                addWarning(e.getMessage(), true); 
            }
            
            BaseTagStatus preferredReceiver = getStatus().getAncestorTag(PreferredOptionReceiverTag.class, false);
            if (preferredReceiver != null) {
                try {
                    preferredReceiver.setOption(this.getName(), optionValue, getScope());
                }
                catch (IllegalArgumentException e) {
                    addWarning(e.getMessage(), true); 
                }
            }
            
		}
	}

    protected Object retrieveOptionValue(Object tagResult) throws WGException {
        String expr = getExpression();
        if (expr != null) {
            tagResult = calculateOptionByExpression(expr);
        }
        
        String item = getItem();
        if (item != null) {
            return getTMLContext().item(item);
        }
        
        return tagResult;
    }

    private Object calculateOptionByExpression(String expr) throws TMLException {
        Object result = null;
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        ExpressionResult exprResult = engine.evaluateExpression(expr, getTMLContext(), RhinoExpressionEngine.TYPE_EXPRESSION, null);
        if (!exprResult.isError()) {
            result = exprResult.getResult();
            return result;
        }
        else {
            throw new TMLException("Error evaluation option expression: " + exprResult.getException().getClass() + exprResult.getException().getMessage(), true);
        }
        
    }

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", _name, "");
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this._name = name;
	}

    /**
     * @return Returns the mode.
     */
    public String getMode() {
        return getTagAttributeValue("mode", _mode, null);
    }
    /**
     * @param mode The mode to set.
     */
    public void setMode(String mode) {
        this._mode = mode;
    }

    public String getScope() {
        return getTagAttributeValue("scope", _scope, null);
    }

    public void setScope(String scope) {
        this._scope = scope;
    }

    public String getExpression() {
        return getTagAttributeValue("expression", _expression, null);
    }

    public void setExpression(String expression) {
        this._expression = expression;
    }

    public String getDefault() {
        return getTagAttributeValue("default", _default, null);
    }

    public void setDefault(String default1) {
        _default = default1;
    }

    public String getDefaultexpression() {
        return getTagAttributeValue("defaultexpression", _defaultexpression, null);
    }

    public void setDefaultexpression(String defaultexpression) {
        _defaultexpression = defaultexpression;
    }

    public String getDefaultitem() {
        return getTagAttributeValue("defaultitem", _defaultitem, null);
    }

    public void setDefaultitem(String defaultitem) {
        _defaultitem = defaultitem;
    }
}

