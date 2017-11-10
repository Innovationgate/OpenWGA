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
package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletRequest;

import org.dom4j.Node;

import de.innovationgate.ext.org.mozilla.javascript.ConsString;
import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.WrapFactory;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.Server;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.events.ApplicationEvent;
import de.innovationgate.wgpublisher.events.ContentTypeEvent;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

import de.innovationgate.wgpublisher.filter.WGAFilter.RequestWrapper;

public class RhinoWrapFactory extends WrapFactory {
    
    static interface WrapMethod<T> {
        public Scriptable wrap(Object obj, Scriptable scope);
    }
    
    private Map<Class<?>,WrapMethod<? extends Object>> _wrapMethods = new HashMap<Class<?>, RhinoWrapFactory.WrapMethod<?>>();
    
    public RhinoWrapFactory() {

        setJavaPrimitiveWrap(false);
        _wrapMethods.put(TMLContext.class, new WrapMethod<TMLContext>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new ContextWrapper(scope, (TMLContext) obj);
            }
            
        });
        
        _wrapMethods.put(TMLUserProfile.class, new WrapMethod<TMLUserProfile>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new UserProfileWrapper(scope, (TMLUserProfile) obj);
            }
            
        });
        
        _wrapMethods.put(TMLForm.class, new WrapMethod<TMLForm>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new FormWrapper(scope, (TMLForm) obj);
            }
            
        });

        _wrapMethods.put(RequestWrapper.class, new WrapMethod<RequestWrapper>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new ServletRequestWrapper(scope, (RequestWrapper) obj);
            }
            
        });

        _wrapMethods.put(HashMap.class, new WrapMethod<HashMap>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new MapWrapper(scope, (HashMap) obj);
            }
            
        });

        
        _wrapMethods.put(TMLPortlet.class, new WrapMethod<TMLPortlet>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new PortletWrapper(scope, (TMLPortlet) obj);
            }
            
        });
        
        _wrapMethods.put(ContentTypeEvent.class, new WrapMethod<ContentTypeEvent>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new EventWrapper(scope, (ContentTypeEvent) obj);
            }
            
        });
        
        _wrapMethods.put(App.class, new WrapMethod<App>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new AppWrapper(scope, (App) obj);
            }
            
        });
        
        _wrapMethods.put(Server.class, new WrapMethod<Server>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new ServerWrapper(scope, (Server) obj);
            }
            
        });
        
        _wrapMethods.put(ApplicationEvent.class, new WrapMethod<ApplicationEvent>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new ApplicationEventWrapper(scope, (ApplicationEvent) obj);
            }
            
        });
        
        _wrapMethods.put(TMLPageImpl.class, new WrapMethod<TMLPage>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new TMLPageWrapper(scope, (TMLPage) obj);
            }
            
        });
        
        _wrapMethods.put(Node.class, new WrapMethod<Node>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new NodeWrapper(scope, (Node) obj);
            }
            
        });
        
        _wrapMethods.put(Design.class, new WrapMethod<Design>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                try {
                    return new de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Design((Design) obj, scope);
                }
                catch (Exception e) {
                    // Cannot happen with these parameters
                    return null;
                }
            }
            
        });
        
        _wrapMethods.put(Date.class, new WrapMethod<Date>() {

            @Override
            public Scriptable wrap(Object obj, Scriptable scope) {
                return new DateWrapper(scope, (Date) obj);
            }
            
        });
     

    }
    

	public Object wrap(Context cx, Scriptable scope, java.lang.Object obj, Class<?> staticType) {
	    
	    // Custom wrapping
	    if (obj != null) {
    	    Class<?> lookupClass = (obj instanceof Date ? Date.class : obj.getClass());
    	    WrapMethod<? extends Object> method = _wrapMethods.get(lookupClass);
    	    if (method != null) {
    	        return method.wrap(obj, scope);
    	    }
	    }
	    
	    // Special case: ConsString is actually (like) a primitive, but will nevertheless be wrapped
	    // by the default wrap functionality. We return it unaltered here.
	    if (obj instanceof ConsString) {
	        return obj;
	    }

	    return super.wrap(cx, scope, obj, staticType);
		
	}
    
    /**
     * Converts Scriptable.NOT_FOUND to null.
     * Worksaround a behaviour change in Rhino 1.6R7 where all methods beginning with "set|get|is" of an object
     * would be treated as potential property getters/setters unregarding their parameter count.
     * If such a non-property method was found by NativeJavaObject, it would strip of the prefix and register
     * it as property. In such a case NativeJavaObject.has(name) would return true, but NativeJavaObject.get(name)
     * returns Scriptable.NOT_FOUND, resulting an ReferenceError. This method helps in removing these unwanted NOT_FOUND.
     * @param obj
     * @return
     */
    public static Object notFoundToNull(Object obj) {
        
        if (obj == Scriptable.NOT_FOUND) {
            return null;
        }
        else {
            return obj;
        }
        
    }
}
