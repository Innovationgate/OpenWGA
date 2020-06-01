package de.innovationgate.wgpublisher;
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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.hibernate.id.IdentityGenerator.GetGeneratedKeysDelegate;

import de.innovationgate.utils.WGUtils;

public class WGCookie extends Cookie {

    private boolean _httpOnly = true;
    private String _sameSite = "Strict";
    
    public static final DateFormat EXPIRES_FORMAT = new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss z");
    static {
        EXPIRES_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }
    
    public WGCookie(String name, String value) {
        super(name, value);
    }

    public boolean isHttpOnly() {
        return _httpOnly;
    }

    public void setHttpOnly(boolean httpOnly) {
        _httpOnly = httpOnly;
    }
    
    public String getSameSite(){
    	return _sameSite;
    }
    public void setSameSite(String value){
    	_sameSite = value;
    }
    
    public String buildSetCookieHeader() {
        List<String> parts = new ArrayList<String>();        
        parts.add(getName()+"="+getValue());
        if (getPath() != null) {
            parts.add("Path="+getPath());
        }
        if (getDomain() != null) {
            parts.add("Domain=" + getDomain());
        }        
        if (getSecure()) {
            parts.add("Secure");
        }
        if (getMaxAge() != -1) {
            parts.add("Max-Age=" + getMaxAge());
            if (getMaxAge() > 0) {
                parts.add("Expires=" + EXPIRES_FORMAT.format(new Date(System.currentTimeMillis() +
                        getMaxAge()*1000L)));
            }
        }
        if (getComment() != null) {
            parts.add("Comment=" + getComment());
        }
        if (isHttpOnly()) {
            parts.add("HttpOnly");
        }
        parts.add("SameSite=" + getSameSite()); 
        
        return WGUtils.serializeCollection(parts, "; ");
    }

    public void addCookieHeader(HttpServletResponse response) {
        response.addHeader("Set-Cookie", buildSetCookieHeader());        
    }

    public static WGCookie from(Cookie cookie) {
        WGCookie wgCookie = new WGCookie(cookie.getName(), cookie.getValue());
        wgCookie.setComment(cookie.getComment());
        
        if (cookie.getDomain() != null) {
            wgCookie.setDomain(cookie.getDomain());
        }
        
        wgCookie.setSecure(cookie.getSecure());
        wgCookie.setPath(cookie.getPath());
        wgCookie.setMaxAge(cookie.getMaxAge());
        wgCookie.setVersion(cookie.getVersion());
        return wgCookie;
    }
    

}
