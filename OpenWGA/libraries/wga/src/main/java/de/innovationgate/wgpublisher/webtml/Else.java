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

public class Else extends Base {
	
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
        private If.Status parentTag;
        
        @Override
        public boolean isDirectOutputCapable() {
            return true;
        }
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }

	public void tmlStartTag() {
		
	    Status status = (Status) getStatus();

		status.parentTag = (If.Status) getStatus().getAncestorTag(If.class);
		if (status.parentTag == null) {
			this.addWarning("Then-Tag without If tag as parent", true);
			return;
		}
		if (status.parentTag.anyConditionTrue) {
			this.setEvalBody(false);
			this.setWriteVars(false);
			return;
		}
	}
	
	public void tmlEndTag() {
		
	    Status status = (Status) getStatus();
	    If.Status ifStatus = status.parentTag;
		if (ifStatus.anyConditionTrue == false) {
		    ifStatus.anyConditionTrue = true;
		    ifStatus.resultTag = status;
		}
	}
		
	 

}
