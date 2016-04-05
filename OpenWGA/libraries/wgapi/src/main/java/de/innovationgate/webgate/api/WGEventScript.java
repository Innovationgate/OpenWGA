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
package de.innovationgate.webgate.api;

/**
 * Represents the data of an event script that can be executed on a WGA event.
 */
public class WGEventScript {

	private String _type;
	private String _code;
	
	/**
	 * Constructor taking script type and code separately
	 * @param type The script type (i.e. language)
	 * @param code The script code
	 */
	public WGEventScript(String type, String code) {
		_type = type;
		_code = code;
	}
	
	/**
	 * Constructor taking the script code as parameter
	 * @param eventString The script type and code in format: scripttype/scriptcode
	 */
	public WGEventScript(String eventString) {
		int slashPos = eventString.indexOf("/");
		if( slashPos != -1){
			_type = eventString.substring(0, slashPos);
			_code = eventString.substring(slashPos + 1);
		}
		else{
			_type = "";
			_code = "";
		}
	}
	
	

	/**
	 * Returns the script code
	 */
	public String getCode() {
		return _code;
	}

	/**
	 * Returns the script type (i.e. language)
	 */
	public String getType() {
		return _type;
	}

	/**
	 * Sets the script code
	 * @param string
	 */
	public void setCode(String string) {
		_code = string;
	}

	/**
	 * Sets the script type (i.e. language)
	 * @param string
	 */
	public void setType(String string) {
		_type = string;
	}

	/**
	 * Returns the one-string-representation of this script in format: scripttype/scriptcode
	 */
	public String toString() {
		return getType() + "/" + getCode();
	}

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_code == null) ? 0 : _code.hashCode());
        result = prime * result + ((_type == null) ? 0 : _type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WGEventScript other = (WGEventScript) obj;
        if (_code == null) {
            if (other._code != null)
                return false;
        }
        else if (!_code.equals(other._code))
            return false;
        if (_type == null) {
            if (other._type != null)
                return false;
        }
        else if (!_type.equals(other._type))
            return false;
        return true;
    }

}
