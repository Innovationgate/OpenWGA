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

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementMap;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;

/**
 * Access log configuration
 */
public class AccessLog extends ConfigBean {

    private static final long serialVersionUID = 1L;

    public static final String OPTION_PATH = "path";

	@Attribute(required=false)
	private boolean enabled = true;
	
	@Attribute(required=false)
	private boolean logKeysOnly = false;
	
	@Element
	@NotNull
	private String dbServer;
	
	@Element
	@NotNull
	private String implClassName;
	
	/*
	@Element
	@NotNull
	private String path;*/
	
    @ElementMap(entry="option", key="name", attribute=true, required=false)
    @NotNull
    @Options
	private Map<String,String> options = new LinkedHashMap<String,String>();


	public AccessLog() {		
	}
	
	public AccessLog(String implClassName, String path) {
		setImplClassName(implClassName);
		getOptions().put(OPTION_PATH, path);
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public boolean isLogKeysOnly() {
		return logKeysOnly;
	}

	public void setLogKeysOnly(boolean logKeysOnly) {
		this.logKeysOnly = logKeysOnly;
	}

	public String getImplClassName() {
		return implClassName;
	}

	public void setImplClassName(String implClassName) {
		this.implClassName = implClassName;
	}

	public Map<String, String> getOptions() {
		return options;
	}
	
	
	public void setOptions(Map<String, String> options) {
		if (options == null) {
			this.options = new HashMap<String, String>();
		} else {
			this.options = options;
		}
	}

    

	public String getDbServer() {
		return dbServer;
	}

	public void setDbServer(String dbServer) {
		this.dbServer = dbServer;
	}

    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = new ArrayList<ModuleDefinition>();
        
        if (property.getName().equals("options")) {
            list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.AccessLoggerModuleType", getImplClassName()));
        }
        
        return list;
        
    }

}
