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
package de.innovationgate.wgpublisher.webtml.utils;

import java.util.Collections;

import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;

public class HTMLXMLEncodingFormatter implements ObjectFormatter {

	private String _encoding;
    private Version _version;
	
	public HTMLXMLEncodingFormatter(String encoding, Version version) {
		_encoding = encoding;
		_version = version;	
	}

	/**
	 * @see de.innovationgate.utils.ObjectFormatter#format(Object)
	 */
	public String format(Object obj) {

		if (_encoding.equalsIgnoreCase("html")) {
		    if (_version.isAtLeast(6, 2)) {
		        return WGUtils.encodeHTML(String.valueOf(obj), true, WGUtils.LINEFEEDS_CONVERT_TO_BR, Collections.singleton((int) Character.valueOf('\'')));
		    }
		    else {
		        return WGUtils.encodeHTML(String.valueOf(obj));
		    }
		}
		else {
			return WGUtils.encodeXML(String.valueOf(obj));
		}

	}

}
