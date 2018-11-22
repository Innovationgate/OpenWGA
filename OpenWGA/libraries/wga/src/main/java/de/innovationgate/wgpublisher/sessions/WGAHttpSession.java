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

package de.innovationgate.wgpublisher.sessions;

import java.io.Serializable;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import javax.servlet.http.HttpSessionContext;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.wgpublisher.WGACore;

@SuppressWarnings("deprecation")
public class WGAHttpSession implements HttpSession, de.innovationgate.wgpublisher.sessions.api.WGAHttpSession, Serializable {

    private static final long serialVersionUID = 2L;
    private Map<String,Object> _attributes = new ConcurrentHashMap<String, Object>();
    
    private transient ServletContext _context = null;

    private int _maxInactiveInterval = 30 * 60;
    private long _creationTime;
    private String _id;


    private long _lastAccessTime;
    private boolean _new = true;
    private boolean _invalidated = false;
    
    
    public WGAHttpSession(ServletContext context, String id) {
        _id = id;
        _context = context;
        _creationTime = System.currentTimeMillis();
        access();
    }

    public Object getAttribute(String key) {
        checkValidity();
        access();
        return _attributes.get(key);
    }

    public Enumeration<String> getAttributeNames() {
        checkValidity();
        access();
        return Collections.enumeration(_attributes.keySet());
    }

    public void access() {
        _lastAccessTime = System.currentTimeMillis();
    }

    public long getCreationTime() {
        checkValidity();
        return _creationTime;
    }

    public String getId() {
        return _id;
    }
    
    public void setId(String id) {
        _id = id;
    }


    public long getLastAccessedTime() {
        checkValidity();
        return _lastAccessTime;
    }

    public int getMaxInactiveInterval() {
        return _maxInactiveInterval ;
    }

    public ServletContext getServletContext() {
        access();
        return _context;
    }

    @Deprecated
    public HttpSessionContext getSessionContext() {
        access();
        return null;
    }

    public Object getValue(String key) {
        return getAttribute(key);
    }

    public String[] getValueNames() {
        checkValidity();
        access();
        return _attributes.keySet().toArray(new String[0]);
    }

    @Override
    public void invalidate() {    
        checkValidity();
        _invalidated = true;
        Iterator<String> keys = _attributes.keySet().iterator();
        while (keys.hasNext()) {
            String key = keys.next();
            Object value = _attributes.get(key);
            if (value instanceof HttpSessionBindingListener) {
                fireUnboundEvent(key, value);
            }
        }
    }

    public boolean isNew() {
        checkValidity();
        return _new ;
    }
    
    public void setNew(boolean isNew) {
        _new = isNew;
    }

    public void putValue(String key, Object value) {
        setAttribute(key, value);
    }

    public void removeAttribute(String key) {
        checkValidity();
        access();
        Object att = _attributes.remove(key);
        if (att != null) {
            fireUnboundEvent(key, att);
        }
    }

    private void fireUnboundEvent(String key, Object att) {
        try {
            if (att instanceof HttpSessionBindingListener) {
                HttpSessionBindingEvent event = new HttpSessionBindingEvent(this, key, att);
                ((HttpSessionBindingListener)att).valueUnbound(event);
            }
        } catch (Throwable e) {
            WGACore.retrieve(getServletContext()).getLog().error("Failed to dispatch HttpSessionBindingEvent for attribute '" + att + "'." , e);
        }
    }

    public void removeValue(String key) {
        removeAttribute(key);               
    }

    public void setAttribute(String key, Object value) {
        checkValidity();
        access();
        if(value!=null)
        	_attributes.put(key, value);
        else WGACore.retrieve(getServletContext()).getLog().warn("WGAHttpSession: ignored null value for key '" + key + "'");
        if (value != null && value instanceof HttpSessionBindingListener) {
            try {
                HttpSessionBindingEvent event = new HttpSessionBindingEvent(this, key, value);
                ((HttpSessionBindingListener)value).valueBound(event);
            } catch (Throwable e) {
                WGACore.retrieve(getServletContext()).getLog().error("Failed to dispatch HttpSessionBindingEvent for attribute '" + value + "'." , e);
            }
        }
    }

    private void checkValidity() {
        if (_invalidated) {
            throw new IllegalStateException("Session '" + _id + "' has been invalidated.");
        }        
    }

    protected boolean isInvalidated() {
        return _invalidated;
    }

    public void setMaxInactiveInterval(int interval) {
        _maxInactiveInterval = interval;
    }       
    
    
    public void setContext(ServletContext context) {
        _context = context;
    }

    public void pushData(WGAHttpSession wgaHttpSession) {
        Enumeration<String> attNames = getAttributeNames();
        while (attNames.hasMoreElements()) {
            String attName = attNames.nextElement();
            Object value = getAttribute(attName);
            if (value != null && value instanceof Serializable) {                
                wgaHttpSession.setAttribute(attName, XStreamUtils.clone(value));
            } else if (value == null) {
                wgaHttpSession.setAttribute(attName, null);
            }
        }        
    }

    @Override
    public void touch() {
        access();
    }

    @Override
    public HttpSession getJavaSession() {
        return this;
    }


}
