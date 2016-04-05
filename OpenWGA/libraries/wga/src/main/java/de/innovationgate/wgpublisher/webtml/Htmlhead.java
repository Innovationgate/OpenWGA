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

import de.innovationgate.webgate.api.WGException;

public class Htmlhead extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _scripts = null;
    private String _metaoutput = null;
	
	public void tmlEndTag() throws WGException {
		buildHTMLHead(stringToBoolean(getMetaoutput()), getScripts());
	}

    public String getScripts() {
		return this.getTagAttributeValue("scripts", _scripts, null);
	}

	public void setScripts(String scripts) {
		this._scripts = scripts;
	}

    /**
     * @return Returns the metaoutput.
     */
    public String getMetaoutput() {
        String defaultValue = (String) getOption("metaoutput");
        if (defaultValue == null) {
            defaultValue = "true";
        }
        return getTagAttributeValue("metaoutput", _metaoutput, defaultValue);
    }

    /**
     * @param metaoutput The metaoutput to set.
     */
    public void setMetaoutput(String metaoutput) {
        _metaoutput = metaoutput;
    }

}
