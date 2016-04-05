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


public class ElseIf extends ConditionBase {
	
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
    	private If.Status parentTag;
    	private boolean conditionTrue;
    	
    	@Override
    	public boolean isDirectOutputCapable() {
    	    return true;
    	}
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }

	/**
	 * @see TMLTag#doStartTag()
	 */
	public void tmlStartTag()  {

	    Status status = (Status) getStatus();
	    
		status.conditionTrue = false;

		status.parentTag = (If.Status) getStatus().getAncestorTag(If.class);
		if (status.parentTag == null) {
			this.addWarning("Elseif-Tag without If tag as parent", true);
			return;
		}
		
		if (status.parentTag.anyConditionTrue) {
			this.setEvalBody(false);
			this.setWriteVars(false);
			return;
		}
		
		boolean result  = this.testCondition();
		if (result == true) {
		    status.conditionTrue = true;
		}
		else {
		    status.conditionTrue = false;
		}
		
		this.setEvalBody(status.conditionTrue);
		this.setWriteVars(status.conditionTrue);
	}
	
	public void tmlEndTag()  {
	
	    Status status = (Status) getStatus();
		if (status.conditionTrue == true) {
		    If.Status ifStatus = status.parentTag;
		    ifStatus.anyConditionTrue = true;
		    ifStatus.resultTag = status;
		}
				
	}
}

