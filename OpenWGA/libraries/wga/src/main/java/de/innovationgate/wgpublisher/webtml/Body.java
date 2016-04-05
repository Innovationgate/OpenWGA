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

import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Body extends Base {
	
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class BodyStatus extends BaseTagStatus {
    	private Element.Status element;
    	private de.innovationgate.wgpublisher.webtml.utils.ElementImplContext context;
    }
    
    @Override
    public BodyStatus createTagStatus() {
        return new BodyStatus();
    }

	/**
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException {
	    BodyStatus status = (BodyStatus) getStatus();
		getStatus().keepResult = true;
		status.element = (Element.Status) status.getParentTag(Element.class);
		status.element.bodyTag = this;

		if (status.element == null) {
			this.addWarning("tml:option can only be used inside tml:element");
			return;
		}
		status.element.maybeCallBegin();
		status.context = status.element.elementImplContext;
		if (status.element.callBeforeBody() == false) {
			this.setEvalBody(false);
		}
	}

	/**
	 * @see Base#tmlAfterBody()
	 */
	public void tmlAfterBody() throws TMLException {
	    BodyStatus status = (BodyStatus) getStatus();
		status.element.maybeCallBegin();
		status.context.appendResult(this.getResultString());
		this.clearResult();
		if (status.element.callAfterBody() == false) {
			this.setEvalBody(false);
		}
		else {
			this.setEvalBody(true);
		}
	}

	/**
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws TMLException {
		this.setResultOutput(false);
	}

}

