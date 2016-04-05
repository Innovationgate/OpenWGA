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

package de.innovationgate.wga.server.api;

/**
 * Represents an HTTP cookie
 */
public class Cookie {

    private String _name;
    private String _path;
    private String _value;
    private String _domain;
    private int _maxAge = -1;
    private boolean _secure = false;
    private boolean _fromClient = false;

    public Cookie(javax.servlet.http.Cookie c) {
        _name = c.getName();
        _domain = c.getDomain();
        _path = c.getPath();
        _value = c.getValue();
        _maxAge = c.getMaxAge();
        _secure = c.getSecure();
        _fromClient = true;
    }
    
    protected Cookie() {
    }

    public String getName() {
        return _name;
    }

    public String getDomain() {
        return _domain;
    }

    public String getPath() {
        return _path;
    }

    public String getValue() {
        return _value;
    }

    public int getMaxAge() {
        return _maxAge;
    }

    public boolean isSecure() {
        return _secure;
    }
    
    public boolean isPersistent() {
        return _maxAge != -1;        
    }

    public void setName(String name) {
        _name = name;
    }

    public void setDomain(String domain) {
        _domain = domain;
    }

    public void setPath(String path) {
        _path = path;
    }

    public void setValue(String value) {
        _value = value;
    }

    public void setMaxAge(int maxAge) {
        _maxAge = maxAge;
    }

    public void setSecure(boolean secure) {
        _secure = secure;
    }
    
    protected javax.servlet.http.Cookie toJavaCookie() {
        
        javax.servlet.http.Cookie c = new javax.servlet.http.Cookie(_name, _value);
        c.setDomain(_domain);
        c.setPath(_path);
        c.setMaxAge(_maxAge);
        c.setSecure(_secure);
        return c;
        
    }

    public boolean isFromClient() {
        return _fromClient;
    }

}
