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
 * but W    ITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.wgpublisher.events;

public class EventPathEntry {
    
    public static final String ENTRYKEY_TYPE = "eventtype";
    public static final String ENTRYKEY_PAGEKEY = "pagekey";
    public static final String ENTRYKEY_LANGUAGE = "language";
    public static final String ENTRYKEY_VERSION = "version";
    public static final String ENTRYKEY_CONTENTTYPE = "contenttype";
    public static final String ENTRYKEY_DB = "db";
    public static final String ENTRYKEY_EVENT = "event";
    public static final String ENTRYKEY_QUALIFIER = "q";

	private String _key;
	private String _value;
	
	public EventPathEntry(String name, String value) {
		_key = name;
		_value = value;
	}
	
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append(_key).append("=").append(_value);
		return buffer.toString();
	}
	
	public EventPathEntry toWildcardVersion() {
		return new EventPathEntry(_key, "*");
	}


	/**
	 * @return
	 */
	public String getKey() {
		return _key;
	}

	/**
	 * @return
	 */
	public String getValue() {
		return _value;
	}
	
	public boolean isWildcard() {
		return _value.equals("*");
	}

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_key == null) ? 0 : _key.hashCode());
        result = prime * result + ((_value == null) ? 0 : _value.hashCode());
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
        EventPathEntry other = (EventPathEntry) obj;
        if (_key == null) {
            if (other._key != null)
                return false;
        }
        else if (!_key.equals(other._key))
            return false;
        if (_value == null) {
            if (other._value != null)
                return false;
        }
        else if (!_value.equals(other._value))
            return false;
        return true;
    }


}
