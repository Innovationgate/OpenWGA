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

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Level extends Base {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String from;
	private String to;
	
	public void tmlStartTag() throws TMLException, WGAPIException {
		
		de.innovationgate.webgate.api.WGContentNavigator navigator = new de.innovationgate.webgate.api.WGContentNavigator(null, new WebTMLLanguageChooser(getTMLContext().db(), getTMLContext()));
		int level = navigator.getContentLevel(this.getTMLContext().content()).intValue();
		int minLevel = 0;
		int maxLevel = 0;
		
		try {
			minLevel = stringToInteger(this.getFrom(), 1);
			maxLevel = (this.getTo().equals("*") ? Integer.MAX_VALUE : stringToInteger(getTo(), Integer.MAX_VALUE));
		}
		catch (NumberFormatException exc) {
			this.addWarning("Attribute to and/or from in tag level not numeric: " + exc.toString(), true);
		}
		
		if (level < minLevel || level > maxLevel) {
			this.setEvalBody(false);
		}

	} 

	/**
	 * Gets the from
	 * @return Returns a String
	 */
	public String getFrom() {
		return this.getTagAttributeValue("from", from, "1");
	}
	/**
	 * Sets the from
	 * @param from The from to set
	 */
	public void setFrom(String from) {
		this.from = from;
	}

	/**
	 * Gets the to
	 * @return Returns a String
	 */
	public String getTo() {
		return this.getTagAttributeValue("to", to, "*");
	}
	/**
	 * Sets the to
	 * @param to The to to set
	 */
	public void setTo(String to) {
		this.to = to;
	}

}

