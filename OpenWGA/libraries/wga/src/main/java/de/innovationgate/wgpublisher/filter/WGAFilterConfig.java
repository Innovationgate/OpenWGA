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
package de.innovationgate.wgpublisher.filter;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;

import de.innovationgate.wga.config.FilterMapping;


public class WGAFilterConfig implements FilterConfig {
	
	private String _filterName;
	private String _filterClassName;
	
	private List<String> _urlpatterns = new ArrayList<String>();
	private Hashtable<String,String> _parameters = new Hashtable<String,String>();
	
	
	private ServletContext _servletContext;
	
	private boolean _enabled;

	public static WGAFilterConfig createFromMapping(FilterMapping mapping) {
		WGAFilterConfig config = new WGAFilterConfig();
		
		config._filterName = mapping.getName();		
		config._filterClassName = mapping.getImplClassName();
		
		if (config._filterName == null || config._filterClassName == null) {
			return null;
		}
		
		config._urlpatterns = new ArrayList<String>(mapping.getUrlPatterns());
		config._parameters = new Hashtable<String,String>(mapping.getInitParameters());
		config._enabled = mapping.isEnabled();
		return config;
	}

	public WGAFilterConfig() {
	}

	public String getFilterName() {
		return _filterName;
	}

	public String getInitParameter(String name) {
		return (String) _parameters.get(name);
	}

	public Enumeration<String> getInitParameterNames() {
		return _parameters.keys();
	}

	public ServletContext getServletContext() {
		return _servletContext;
	}

	public void setServletContext(ServletContext servletContext) {
		_servletContext = servletContext;
	}

	public String getFilterClassName() {
		return _filterClassName;
	}

	public List<String> getUrlpatterns() {
		return _urlpatterns;
	}

	public boolean isEnabled() {
		return _enabled;
	}

	public void setEnabled(boolean enabled) {
		_enabled = enabled;
	}


}
