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
package de.innovationgate.webgate.api.workflow;

import de.innovationgate.webgate.api.WGContent;

/**
 * Represents a workflow event, e.g. sending a workflow mail, and contains associated information.
 */
public class WGWorkflowEvent {
	
	private Object _mailBody;


	/**
	 * A workflow mail will be sent.
	 */
	public static final int TYPE_WORKFLOWMAIL = 1;
	

	private WGContent _content;

	private int _type;

	/**
	 * Constructor. Not no be used outside WGAPI
	 * @param type
	 * @param content
	 * @param mailBody
	 */
	public WGWorkflowEvent(int type, WGContent content, Object mailBody) {
		_type = type;
		_content = content;
		_mailBody = mailBody;
	}


	/**
	 * The content, for which the event was triggered
	 */
	public WGContent getContent() {
		return _content;
	}

	/**
	 * The type of workflow event. A constant of WGWorkflowEvent.TYPE_...
	 */
	public int getType() {
		return _type;
	}

	/**
	 * An object representing the text body of a workflow mail
	 */
	public Object getMailBody() {
		return _mailBody;
	}

}
