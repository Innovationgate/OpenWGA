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
import org.simpleframework.xml.Root;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * base configuration element for data sources, OpenWGA applications and personalisation databases
 *
 */
@Root(strict=false)
public abstract class Database extends IdentifiableConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String OPTION_PATH = "Path";
	public static final String OPTION_JNDI_PATH = "JNDIPath";
	
	public static final String OPTIONFLAG_PATH_OPTION = "PathOption";
	
    @Attribute(required=false)
	private boolean enabled = true;
	
	@Attribute(required=false)
	private boolean lazyConnecting = false;
	
	@Element
	@NotNull
	private String dbServer;
	
	@Element
	@NotNull
	private String implClassName;
	
	@ElementMap(entry="option", key="name", attribute=true, required=false)
	@NotNull
	@Options
	private Map<String, String> databaseOptions = new LinkedHashMap<String,String>();
	
	public Database() {
	    super();
	}
	
	public Database(String dbServer, String implClassName) {
	    super();
		setDbServer(dbServer);
		setImplClassName(implClassName);
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public boolean isLazyConnecting() {
		return lazyConnecting;
	}

	public void setLazyConnecting(boolean lazyConnecting) {
		this.lazyConnecting = lazyConnecting;
	}

	public String getDbServer() {
		return dbServer;
	}

	public void setDbServer(String dbServer) {
		this.dbServer = dbServer;
	}

	public String getImplClassName() {
		return implClassName;
	}

	public void setImplClassName(String implClassName) {
		this.implClassName = implClassName;
	}

	public Map<String, String> getDatabaseOptions() {
		return databaseOptions;
	}

	public void setDatabaseOptions(Map<String, String> databaseOptions) {
		if (databaseOptions == null) {
			this.databaseOptions = new HashMap<String, String>();
		} else {
			this.databaseOptions = databaseOptions;
		}
	}
	
    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = new ArrayList<ModuleDefinition>();
        
        if (property.getName().equals("databaseOptions")) {
            if (this instanceof ContentStore) {
                list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.ContentStoreModuleType", getImplClassName()));
            }
            else if (this instanceof ContentDatabase) {
                list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.ContentDatabaseModuleType", getImplClassName()));
            }
            else if (this instanceof PersonalisationDatabase) {
                list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType", getImplClassName()));
            }
        }
        
        return list;
        
    }


}
