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
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.webtml.Base;


public class ExpressionComparator extends de.innovationgate.utils.ObjectComparator {
	
	private Base tag;
	private ExpressionEngine engine;
	private String expression;
	private List<String> valueInjectionNames = new ArrayList<String>();
	
	public ExpressionComparator(Base tag, ExpressionEngine engine, String expression, Collator collator) {
        super(collator);
		this.tag = tag;
		this.engine = engine;
		this.expression =expression;
	}

	/**
	 * @see Comparator#compare(Object, Object)
	 */
	public int compare(Object arg0, Object arg1) {
		
	    try {
            TMLContext context0 = getValueContext(arg0);
            ExpressionEngine engine0 = this.getEngine(context0.content());
            Map<String,Object> additionalObjects0 = new HashMap<String,Object>();
            for (String name : this.valueInjectionNames) {
                context0.setvar(name, arg0);
            }
            ExpressionResult result0 = engine0.evaluateExpression(this.expression, context0, ExpressionEngine.TYPE_EXPRESSION, additionalObjects0);
            
            if (result0.isError()) {
            	this.tag.addWarning("Error in sort expression evaluation: " + result0.getException().getMessage() + " (Expression: [" + result0.getException().getExpression() + "])");
            	return -1;
            }
            
            TMLContext context1 = getValueContext(arg1);
            ExpressionEngine engine1 = this.getEngine(context1.content());
            Map<String,Object> additionalObjects1 = new HashMap<String,Object>();
            for (String name : this.valueInjectionNames) {
                context1.setvar(name, arg1);
            }
            ExpressionResult result1 = engine1.evaluateExpression(this.expression, context1, ExpressionEngine.TYPE_EXPRESSION, additionalObjects1);
            if (result1.isError()) {
            	this.tag.addWarning("Error in sort expression evaluation: " + result1.getException().getMessage() + " (Expression: [" + result1.getException().getExpression() + "])");
            	return 1;
            }
            return super.compare(result0.getResult(), result1.getResult());
        }
        catch (WGAPIException e) {
            tag.getTMLContext().getlog().error("Exception executing comparison expressions", e);
            return 0;
        }

	}

    private TMLContext getValueContext(Object value) {
		if ((value instanceof WGContent)) {
		    return tag.getTMLContextForDocument((WGContent) value);
		}
		else {
		    return tag.getTMLContext();
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

    public List<String> getValueInjectionNames() {
        return valueInjectionNames;
    }

}

