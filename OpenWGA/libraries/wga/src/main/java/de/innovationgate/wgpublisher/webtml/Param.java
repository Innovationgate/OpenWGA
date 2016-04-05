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

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Param extends Base {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    public String getItem() {
        return getTagAttributeValue("item", item, null);
    }
    public void setItem(String item) {
        this.item = item;
    }
    public static final String TYPE_URL = "url";
	public static final String TYPE_VAR = "var";
    private String name;
	private String expression;
	private String item;
	private String type;

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {

		TMLParameterReceiver query = (TMLParameterReceiver) getStatus().getAncestorTag(TMLParameterReceiver.class);
		if (query == null) {
			this.addWarning("No ancestor tag that is a WebTML parameter receiver", true);
			return;
		}
		
		String expr = getExpression();
		String item = getItem();
		Object result;
        if (expr != null) {
            RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
            ExpressionResult exprResult = engine.evaluateExpression(expr, getTMLContext(), RhinoExpressionEngine.TYPE_EXPRESSION, null);
            if (!exprResult.isError()) {
                result = exprResult.getResult();
            }
            else {
                addWarning("Error evaluation param expression: " + exprResult.getException().getClass() + exprResult.getException().getMessage(), true);
                return;
            }
        }
        else if (item != null) {
            result = getTMLContext().item(item);
        }
        else {
            result = getResultString();
        }
		
		query.addParam(this.getName(), result, getType());
		clearResult();

	}
    public String getExpression() {
        return getTagAttributeValue("expression", expression, null);
    }
    public void setExpression(String expression) {
        this.expression = expression;
    }
    public String getType() {
        return getTagAttributeValue("type", type, TYPE_VAR);
    }
    public void setType(String type) {
        this.type = type;
    }

}

