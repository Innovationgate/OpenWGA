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

import java.io.UnsupportedEncodingException;
import java.util.Map;

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGCookie;

/**
 * Represents an HTTP cookie
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Cookie {

	public static String BASE64 = "base64";
	public static String HEX = "hex";
	
    private String _name;
    private String _path;
    private String _value;
    private String _domain;
    private int _maxAge = -1;
    private boolean _secure = false;
    private boolean _fromClient = false;
    private String _sameSite = "Strict";

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

    public String getSameSite(){
    	return _sameSite;
    }
    public Cookie setSameSite(String value){
    	_sameSite = value;
    	return this;
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

    public String getValue(String decode) throws UnsupportedEncodingException, DecoderException {
    	if(decode.equalsIgnoreCase(HEX))
    		return new String(Hex.decodeHex(_value.toCharArray()), "UTF-8");
    	else if(decode.equalsIgnoreCase(BASE64))
    		return WGA.Base64.decode(_value);
    	else throw(new UnsupportedEncodingException());
    }
    
    public Cookie setValue(String value) {
        _value = value;
        return this;
    }

    public Cookie setValue(String value, String encode) throws UnsupportedEncodingException {
    	if(encode.equalsIgnoreCase(HEX))
    		_value = Hex.encodeHexString(value.getBytes("UTF-8"));
    	else if(encode.equalsIgnoreCase(BASE64))
    		_value = WGA.Base64.encode(value);
    	else throw(new UnsupportedEncodingException());
        return this;
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

    public Cookie setName(String name) {
        _name = name;
        return this;
    }

    public Cookie setDomain(String domain) {
        _domain = domain;
        return this;
    }

    public Cookie setPath(String path) {
        _path = path.isEmpty() ? "/" : path;
        return this;
    }

    public Cookie setMaxAge(int maxAge) {
        _maxAge = maxAge;
        return this;
    }

    public Cookie setSecure(boolean secure) {
        _secure = secure;
        return this;
    }
    
    protected javax.servlet.http.Cookie toJavaCookie() {
        
        javax.servlet.http.Cookie c = new javax.servlet.http.Cookie(_name, _value);
        if (_domain != null) {
            c.setDomain(_domain);
        }
        c.setPath(_path);
        c.setMaxAge(_maxAge);
        c.setSecure(_secure);
        return c;
        
    }

    public boolean isFromClient() {
        return _fromClient;
    }

    public void write() throws WGException{
    	WGA wga = WGA.get();
        WGCookie wgcookie = new WGCookie(getName(), getValue());
        wgcookie.setPath(getPath());
        wgcookie.setDomain(getDomain());
        wgcookie.setSameSite(getSameSite());
        wgcookie.setSecure(isSecure());
        wgcookie.setMaxAge(getMaxAge());        
        wgcookie.addCookieHeader(wga.getResponse());
        @SuppressWarnings("unchecked")
		Map<String,Cookie> cookies = (Map<String,Cookie>) wga.getRequest().getAttribute(WGACore.ATTRIB_COOKIES);
        cookies.put(getName(), this);
    }
    
}
