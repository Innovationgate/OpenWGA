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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

import de.innovationgate.utils.Base64;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;

/**
 * A database server configuration
 */
@Root(strict=false)
public class DatabaseServer extends IdentifiableConfigBean {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String OPTION_PATH = "Path";
    public static final String OPTION_MASTERLOGIN_USER = "MasterLoginUser";
    public static final String OPTION_MASTERLOGIN_PASSWORD = "MasterLoginPassword";
    public static final String OPTION_PORT = "Port";
    
    public static final String OPTION_SHAREDPOOL = "SharedPool";
    public static final String OPTION_SHAREDPOOL_MAX_CONNECTIONS = "SharedPool.MaxConnections";
    public static final String OPTION_SHAREDPOOL_MAX_IDLE_CONNECTIONS = "SharedPool.MaxIdleConnections";
    public static final String OPTION_SHAREDPOOL_MIN_IDLE_CONNECTIONS = "SharedPool.MinIdleConnections";
    public static final String OPTION_SHAREDPOOL_MAX_CONNECTION_LIFETIME = "SharedPool.MaxConnectionLifetime";
    public static final String OPTION_SHAREDPOOL_MAX_WAIT = "SharedPool.MaxWait";
    public static final String OPTION_SHAREDPOOL_LEGACY_DBCP_MONITORING = "SharedPool.LegacyDBCPMonitoring";
	public static final String OPTION_SHAREDPOOL_REMOVE_ABANDONED_TIMEOUT = "SharedPool.RemoveAbandonedTimeout";
	public static final String OPTION_SHAREDPOOL_MIN_EVICTABLE_IDLE_TIME_MILLIS = "SharedPool.minEvictableIdleTimeMillis";
	
	@Element
	@NotNull
	private String implClassName;
	
    @Attribute(required=false)
    private boolean enabled = true;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String title;
	
    @ElementMap(entry="option", key="name", attribute=true, required=false)
    @NotNull
    @Options
	private Map<String,String> options = new LinkedHashMap<String,String>();


	public DatabaseServer() {		
	    super();
	}
	
	public DatabaseServer(String implClassName) {
	    super();
		setImplClassName(implClassName);
	}
	
	public String getImplClassName() {
		return implClassName;
	}

	public void setImplClassName(String implClassName) {
		this.implClassName = implClassName;
	}

	public Map<String,String> getOptions() {
		return options;
	}
	
	public void setOptions(Map<String, String> options) {
		if (options == null) {
			this.options = new HashMap<String, String>();
		} else {
			this.options = options;
		}
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
    
    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = new ArrayList<ModuleDefinition>();
        
        if (property.getName().equals("options")) {
            list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.DatabaseServerModuleType", getImplClassName()));
        }
        
        return list;
        
    }


}
