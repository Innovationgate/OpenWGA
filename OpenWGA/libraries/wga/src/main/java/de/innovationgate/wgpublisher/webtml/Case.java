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
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.utils.BooleanItemExpression;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Case extends ConditionBase {
	
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String value;
    private String equals;
    
    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
        private Select.Status parentTag;

        @Override
        public boolean isDirectOutputCapable() {
            return true;
        }
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
	

	/**
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException {

	    Status status = (Status) getStatus();
		status.keepResult = true;

		status.parentTag = (Select.Status) getStatus().getParentTag(Select.class);
		if (status.parentTag != null && status.parentTag.bTestOn == false) {
			this.setEvalBody(false);
			this.setWriteVars(false);
			return;
		}
		
		if (this.testCondition() == true) {

			if (status.parentTag != null) {
				status.parentTag.addResultTag(this);
			}
		}
		
		else {
			this.setEvalBody(false);
			this.setWriteVars(false);
		}
	}


    public String getValue() {
        return getTagAttributeValue("value", value, null);
    }


    public void setValue(String value) {
        this.value = value;
    }


    protected boolean innerTestCondition() {
        
        
        
        
        try {
            
            // Generic condition
            boolean result = super.innerTestCondition();
            if (result == false) {
                return false;
            }
        
            // Conditions only valid for tml:case: value and equals
            String attValue = getValue();
            if (attValue != null) {
                
                Status status = (Status) getStatus();
                if (status.parentTag == null) {
                    addWarning("You cannot use attribute 'value' on tml:case without having tml:select as a parent", true);
                    return false;
                }
                
                Object switchValue = status.parentTag._switchValue;
                if (switchValue == null) {
                	return false;
                } else if (!compareSwitchValue(switchValue, attValue)) {
                    return false;
                }
            	
            }
            
            String attEquals = getEquals();
            if (attEquals != null) {
                
                Status status = (Status) getStatus();
                if (status.parentTag == null || status.parentTag._switchItem == null) {
                    addWarning("You cannot use attribute 'equals' on tml:case without having tml:select with attribute 'item' as a parent", true);
                    return false;
                }
                
                BooleanItemExpression bie = new BooleanItemExpression(status.parentTag._switchItem, attEquals);
                if (!bie.isTrue(getTMLContext())) {
                    return false;
                }
                
            }
        
        }
        catch (Exception exc) {
            log.error("Error evaluating expression", exc);
            return false;
        }
        catch (Error err) {
            log.error("Error evaluating expression", err);
            return false;
        }

        
        
        return true;
    }


    private boolean compareSwitchValue(Object switchValue, String attValue) {
        
        // Two modes: Compare numeric if switchValue is a number, else compare string representations
        if (switchValue instanceof Number) {
        	Double numericSwitchValue = new Double(((Number)switchValue).doubleValue());
            Double numericAttValue = Double.valueOf(attValue);
            return numericSwitchValue.equals(numericAttValue);
        }
        else {
            return String.valueOf(switchValue).equals(attValue);
        }
        
        
    }


    public String getEquals() {
        return getTagAttributeValue("equals", equals, null);
    }


    public void setEquals(String equals) {
        this.equals = equals;
    }
}

