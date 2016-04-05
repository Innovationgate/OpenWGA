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

import java.text.Collator;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.webtml.Base;


public class CompareExpressionComparator extends de.innovationgate.utils.ObjectComparator {
	
	private Base tag;
	private ExpressionEngine engine;
	private String expression;
	private String valueInjectionName = null;
    private List<String> valueBaseNames = new ArrayList<String>();
	
	public CompareExpressionComparator(Base tag, ExpressionEngine engine, String expression, Collator collator, List<String> valueBaseNames) {
        super(collator);
		this.tag = tag;
		this.engine = engine;
		this.expression =expression;
		this.valueBaseNames = valueBaseNames;
	}

	/**
	 * @see Comparator#compare(Object, Object)
	 */
	public int compare(Object arg0, Object arg1) {
		
	    try {
            TMLContext context = getValueContext(arg0);
            ExpressionEngine engine0 = this.getEngine(context.content());
            for (String name: valueBaseNames) {
                context.setvar(name + "1", getValueObject(arg0));
                context.setvar(name + "2", getValueObject(arg1));
            }
            
            ExpressionResult result = engine0.evaluateExpression(this.expression, context, ExpressionEngine.TYPE_EXPRESSION, null);
            
            if (result.isError()) {
            	this.tag.addWarning("Error in sort comparision evaluation: " + result.getException().getMessage() + " (Expression: [" + result.getException().getExpression() + "])");
            	return 0;
            }
            
            Object resultObj = result.getResult();
            if (resultObj instanceof Number) {
                return ((Number) resultObj).intValue();
            }
            else if (resultObj == null)  {
                this.tag.addWarning("Error in sort comparison evaluation: The evaluation returned null which cannot be used for comparison");
                return 0;
            }
            else {
                this.tag.addWarning("Error in sort comparison evaluation: The evaluation returned a non-number result: " + resultObj + "(" + resultObj.getClass().getName() + ") which cannot be used for comparison");
                return 0;
            }
            
            
        }
        catch (WGAPIException e) {
            tag.getTMLContext().getlog().error("Exception executing comparison expressions", e);
            return 0;
        }

	}
	
	private Object getValueObject(Object arg0) {

	    if (arg0 instanceof WGDocument) {
	        return tag.getTMLContextForDocument((WGDocument) arg0);
	    }
	    else {
	        return arg0;
	    }
	    
    }

    private ExpressionEngine getEngine(WGContent content) {
		
		if (this.engine != null) {
			return this.engine;
		}
		else {
			return ExpressionEngineFactory.getEngine(tag.getDefaultExpressionLanguage());
		}
	}

    public String getValueInjectionName() {
        return valueInjectionName;
    }

    public void setValueInjectionName(String valueInjectionName) {
        this.valueInjectionName = valueInjectionName;
    }
    
    private TMLContext getValueContext(Object value) {
        if ((value instanceof WGContent)) {
            return tag.getTMLContextForDocument((WGContent) value);
        }
        else {
            return tag.getTMLContext();
        }
    }

}

