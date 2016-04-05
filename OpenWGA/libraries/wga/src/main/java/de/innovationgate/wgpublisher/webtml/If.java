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

import net.sf.ehcache.writer.writebehind.operations.WriteOperation;


    public class If extends ConditionBase {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
    	protected String result = null;
    	protected boolean mainConditionTrue = false;
    	protected boolean anyConditionTrue = false;
    	protected BaseTagStatus resultTag = null;
    	
    	@Override
    	public boolean isDirectOutputCapable() {
    	    return true;
    	}
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    public Status getStatus() {
        return (Status) super.getStatus();
    }

	public void tmlStartTag() {
		
	    Status status = (Status) getStatus();
		boolean result = this.testCondition();
		if (result == true) {
		    status.resultTag = status;
		    status.mainConditionTrue = true;
		    status.anyConditionTrue = true;
		}
		else {
		    status.resultTag = null;
		    status.mainConditionTrue = false;
			status.anyConditionTrue = false;			
		}
	}


}

