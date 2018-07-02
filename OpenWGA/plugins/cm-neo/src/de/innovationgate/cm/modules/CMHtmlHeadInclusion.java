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

package de.innovationgate.cm.modules;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.model.BrowsingSecurity;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.HTMLHeadInclusion;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class CMHtmlHeadInclusion implements HTMLHeadInclusion {

    public static final Object CM_APP_ID = "cm-neo";

	public CharSequence processInclusion(TMLContext context) {

    	// Determine, which (if any) document can be edited in this request
    	// then inject CSS-Inclusion

    	if(context.getEnvironment().getPageContext().getSession().getAttribute("CM.appid")!=CM_APP_ID)
    		return null;
    	
		try {
			WGACore wgacore = WGA.get().getCore();
			if(wgacore.getDispatcher().getBrowsingSecurity(context.content().getDatabase())>BrowsingSecurity.NO_AUTHORING
					&& context.isbrowserinterface()){
	        	StringBuffer result = new StringBuffer();
				result.append("\n<link rel=\"stylesheet\" type=\"text/css\" href=\"" + 
						WGA.get().design("plugin-cm-neo").resolve("bi-style-injection:styles").scriptURL("css") +
						"\">");
				return result.toString();
			}
				
		} catch (WGException e) {}

        return null;
    }

}
