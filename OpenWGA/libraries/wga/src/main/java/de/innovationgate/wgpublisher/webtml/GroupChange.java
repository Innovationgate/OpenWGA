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

import javax.servlet.jsp.PageContext;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class GroupChange extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    // Attributes
	private String expression;
	private String expressionlanguage;
	
	
	public static class Status extends BaseTagStatus {
	    
	    @Override
	    public Object getTagInfo(String name) throws WGAPIException {

	        if (name.equals("currentvalue")) {
	            return tmlContext.getEnvironment().getPageContext().getAttribute(GroupChange.class.getName() + ":PreviousResult_" + this.id, PageContext.REQUEST_SCOPE);
	        }
	        
	        return super.getTagInfo(name);
	    }
	    
	}
	
	@Override
	protected BaseTagStatus createTagStatus() {
	    return new Status();
	}
	

	/**
	 * Gets the expression
	 * @return Returns a String
	 */
	public String getExpression() {
		return this.getTagAttributeValue("expression", expression, null);
	}
	/**
	 * Sets the expression
	 * @param expression The expression to set
	 */
	public void setExpression(String expression) {
		this.expression = expression;
	}

	/**
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException {

		if (this.getExpression() == null) {
			this.addWarning("No expression given");
			return;
		}
		
		ExpressionEngine engine = ExpressionEngineFactory.getEngine(this.getExpressionlanguage());
		ExpressionResult result = engine.evaluateExpression(this.getExpression(), this.getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, null);
		if (result.isError()) {
			this.addWarning("Error in group change expression evaluation: " + result.getException().getMessage() + " (Expression: " + result.getException().getExpression() + ")", true);
			return;
		}
		
		Object previousResult = this.pageContext.getAttribute(this.getClass().getName() + ":PreviousResult_" + this.getId(), PageContext.REQUEST_SCOPE);
        if (previousResult == null && result.getResult() == null) {
            this.setEvalBody(false);
        }
        else if (previousResult != null && result.getResult() != null && result.getResult().equals(previousResult)) {
			this.setEvalBody(false);
		}
		else {
			this.pageContext.setAttribute(this.getClass().getName() + ":PreviousResult_" + this.getId(), result.getResult(), PageContext.REQUEST_SCOPE);
		}
		

	}

	/**
	 * Gets the expressionlanguage
	 * @return Returns a String
	 */
	public String getExpressionlanguage() {
		return this.getTagAttributeValue("expressionlanguage", expressionlanguage, this.getDefaultExpressionLanguage());
	}
	
	public String getXplanguage() {
		return this.getExpressionlanguage();
	}
	/**
	 * Sets the expressionlanguage
	 * @param expressionlanguage The expressionlanguage to set
	 */
	public void setExpressionlanguage(String expressionlanguage) {
		this.expressionlanguage = expressionlanguage;
	}
	
	public void setXplanguage(String xpl) {
		this.setExpressionlanguage(xpl);
	}

}

