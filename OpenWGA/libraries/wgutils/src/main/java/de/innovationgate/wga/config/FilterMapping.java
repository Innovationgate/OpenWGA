/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;


/**
 * Declaration of an OpenWGA servlet filter
 */
@Root(strict=false)
public class FilterMapping extends IdentifiableConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String name;
	
	@Element
	@NotNull
	private String implClassName;
	
	@Attribute
	@NotNull
	private boolean enabled;
	
	@ElementList(required=false)
	@NotNull
	private List<String> urlPatterns = new ArrayList<String>();

	@ElementMap(required=false)
	@NotNull
	private Map<String, String> initParameters = new HashMap<String, String>();
	
	public FilterMapping() {
	    super();
		setEnabled(true);
	};
	
	public FilterMapping(String name, String implClassName) {
		super();
		setName(name);
		setImplClassName(implClassName);
		setEnabled(true);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getImplClassName() {
		return implClassName;
	}

	public void setImplClassName(String implClassName) {
		this.implClassName = implClassName;
	}

	public List<String> getUrlPatterns() {
		return urlPatterns;
	}

	public Map<String, String> getInitParameters() {
		return initParameters;
	}
	
	
	public void setUrlPatterns(List<String> urlPatterns) {
		if (urlPatterns == null) {
			this.urlPatterns = new ArrayList<String>();
		} else {
			this.urlPatterns = urlPatterns;
		}
	}

	public void setInitParameters(Map<String, String> initParameters) {
		if (initParameters == null) {
			this.initParameters = new HashMap<String, String>();
		} else {
			this.initParameters = initParameters;
		}
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}


}
