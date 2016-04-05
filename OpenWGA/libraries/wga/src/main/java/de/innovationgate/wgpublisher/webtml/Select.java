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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Select extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String testAll;
	private String _switch;
	private String _item;
	
	public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
	    private List<Base> resultTags = new ArrayList<Base>();
	    private boolean bTestAll;
	    protected boolean bTestOn;
	    protected boolean bHasResultTag;
	    Object _switchValue = null;
	    String _switchItem = null;
	    
	    public void addResultTag(Base tag) {
	        resultTags.add(tag);
	        bHasResultTag = true;
	        if (bTestAll == false) {
	            bTestOn = false;
	        }
	    }
	    
	    @Override
	    public boolean isDirectOutputCapable() {
	        return true;
	    }
	}
	
	@Override
	protected BaseTagStatus createTagStatus() {
	    return new Status();
	}
	
	@Override
	public Status getStatus() {
	    return (Status) super.getStatus();
	}
	

	/**
	 * Gets the testAll
	 * @return Returns a String
	 */
	public String getTestall() {
		return this.getTagAttributeValue("testall", testAll, "false");
	}
	/**
	 * Sets the testAll
	 * @param testAll The testAll to set
	 */
	public void setTestall(String testAll) {
		this.testAll = testAll;
	}



	/**
	 * @throws WGAPIException 
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException, WGAPIException {

	    Status status = (Status) getStatus();
		status.bTestAll = this.stringToBoolean(this.getTestall());
		status.bTestOn = true;
		status.bHasResultTag = false;
		
		String switchExpression = getSwitch();
		if (switchExpression != null) {

            ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
            Map<String,Object> objects = new HashMap<>();
            objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "Switch statement on " + getTagDescription());
            ExpressionResult result = engine.evaluateExpression(switchExpression, this.getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, objects);
            if (result.isError()) {
                addExpressionWarning(switchExpression,result);
            }
            else {
                status._switchValue = result.getResult();
            }

		}
		
		String itemName = getItem();
		if (itemName != null) {
		    status._switchItem = itemName;
		}

	}
    public String getSwitch() {
        return getTagAttributeValue("switch", _switch, null);
    }
    public void setSwitch(String switch1) {
        _switch = switch1;
    }
    public Object getSwitchValue() {
        Status status = (Status) getStatus();
        return status._switchValue;
    }

    public String getItem() {
        return getTagAttributeValue("item", _item, null);
    }

    public void setItem(String item) {
        _item = item;
    }
}

