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
import java.util.List;

import javax.servlet.jsp.PageContext;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.Warning;

public class Warnings extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String autohide;

	/**
	 * @throws WGAPIException 
	 * @see Base#tmlEndTag(TMLContext)
	 */
	public void tmlEndTag() throws TMLException, WGAPIException {
		
		if (!getCore().getWgaConfiguration().isWarningsEnabled()) {
			return;
		}
		
		String warningsPageOutput = getCore().getWgaConfiguration().getWarningsOutputViaTML();
		if (warningsPageOutput.equals(Constants.WARNINGS_TML_OFF)) {
			return;
		}
		
		List<Warning> warnings = getTMLContext().getEnvironment().getWarnings();
		
		if (this.stringToBoolean(this.getAutohide()) == true && warnings.size() == 0) {
			return;
		}
		
		if (warningsPageOutput.equals(Constants.WARNINGS_TML_AS_COMMENT)) {
		    outputAsComment(warnings);
		}
		else {
		    outputAsHTML(warnings);
		}
		this.clearWarnings();
		
	}

    private void outputAsComment(List<Warning> warnings) throws WGAPIException {
        
        this.appendResult("\n<!-- ##### WebTML Warnings ##### \n\n");
        
        if (warnings.size() > 0) {
            this.appendResult(warnings.size() + " warning(s) detected\n");
        }
        else {
            this.appendResult("     No warnings detected\n");
        }

        java.util.Iterator warningList = warnings.iterator();
        Warning warning;
        while (warningList.hasNext()) {
            warning = (Warning) warningList.next();
            this.appendResult("      - " + WGUtils.strReplace(warning.getConsoleText(), "-->", "-- >", true) + "\n");
        }
        
        this.appendResult("\n     ##### WebTML Warnings ##### -->");
        
    }

    private void outputAsHTML(java.util.List<Warning> warnings) throws WGAPIException {
        this.appendResult("<table width=\"100%\" border=\"1\" cellpadding=\"3\" class=\"wga-warning\">");
		this.appendResult("<tr><td colspan=\"8\"><h4>TML Warnings </h4>");
		
		if (warnings.size() > 0) {
			this.appendResult("<b>" + warnings.size() + " warning(s) detected</b></td></tr>");
			this.appendResult("<tr><td></td><td><b>Layout</b></td><td><b>Line Nr.</b></td><td><b>Tag Type</b></td><td><b>Tag ID</b></td><td><b>DB</b></td><td><b>Context</b></td><td><b>Message</b></td></tr>");
		}
		else {
			this.appendResult("No warnings detected</td></tr>");
		}

		java.util.Iterator warningList = warnings.iterator();
		Warning warning;
		while (warningList.hasNext()) {
			warning = (Warning) warningList.next();
			this.appendResult("<tr>");
			this.appendResult("<td align=\"center\"><img src=\"" + this.getWGPPath() + "/static/images/warning_" + (warning.isSevere() ? "severe" : "info") + ".jpg\"/></td>");
			this.appendResult("<td>" + warning.getResource() + "</td>");
			this.appendResult("<td>" + warning.getSourceLine()+ "</td>");
			this.appendResult("<td>" + warning.getTagType() + "</td>");
			this.appendResult("<td>" + warning.getTagId() + "</td>");
				this.appendResult("<td>" + warning.getDatabase() + "</td>");
			this.appendResult("<td>" + warning.getContextWGKey() + "</td>");
			this.appendResult("<td>" + WGUtils.encodeHTML(warning.getMessage()) + "</td>");
			this.appendResult("</tr>");
		}
		
		this.appendResult("</table>");
    }

	/**
	 * Returns the autohide.
	 * @return String
	 */
	public String getAutohide() {
		return this.getTagAttributeValue("autohide", autohide, "false");
	}

	/**
	 * Sets the autohide.
	 * @param autohide The autohide to set
	 */
	public void setAutohide(String autohide) {
		this.autohide = autohide;
	}

}

