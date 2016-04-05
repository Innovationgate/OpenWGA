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
package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.Permissions;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.apache.commons.collections.iterators.IteratorEnumeration;

public class IsolatedJARLoader extends URLClassLoader {
	
	private ClassLoader _fallBackLoader = null;

	public IsolatedJARLoader(URL[] urlsToJars, ClassLoader fallbackLoader) {
		super(urlsToJars, null);
		_fallBackLoader = fallbackLoader;
	}


	/* (Kein Javadoc)
	 * @see java.lang.ClassLoader#loadClass(java.lang.String, boolean)
	 */
	public synchronized Class loadClass(String name, boolean resolve) throws ClassNotFoundException {
		
        Class clazz = findLoadedClass(name);

        if (clazz == null) {
    		try {
    			clazz = super.loadClass(name, false);
    		}
    		catch (ClassNotFoundException e) {
    		}
        }
		
        if (clazz == null) {
            clazz = _fallBackLoader.loadClass(name);
        }
        
        if (clazz != null && resolve) {
            resolveClass(clazz);
        }
        return clazz;
	}


    @Override
    public URL findResource(String name) {
        
        URL res = super.findResource(name);

        if (res == null) {
            res = _fallBackLoader.getResource(name);
        }
        
        return res;
        
    }


    @Override
    public Enumeration<URL> findResources(String s) throws IOException {
        
        List<URL> urls = new ArrayList<URL>();
        urls.addAll(Collections.list(super.findResources(s)));
        urls.addAll(Collections.list(_fallBackLoader.getResources(s)));
        return new IteratorEnumeration(urls.iterator());
        
    }


    @Override
    protected PermissionCollection getPermissions(CodeSource arg0) {
        Permissions permissions = new Permissions();
        permissions.add(new AllPermission());
        return permissions;
    }

}
