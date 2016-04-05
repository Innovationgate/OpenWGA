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
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.ElementMap;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * A content database (i.e. "data source") configuration
 */
public class ContentDatabase extends Database {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String key;
	
	@Element
	@NotNull
	private String domain;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String title;
		
	@Element(required=false)
	private boolean clientRestrictionsEnabled = false;
	
	@ElementList(required=false)
	@NotNull
	private List<ClientRestriction> clientRestrictions = new ArrayList<ClientRestriction>();
	
	@ElementList(required=false)
	@NotNull
	private List<FieldMapping> fieldMappings = new ArrayList<FieldMapping>();

    @ElementMap(entry = "option", key = "name", attribute = true, required=false)
    @NotNull
    @Options
    private Map<String, String> publisherOptions = new LinkedHashMap<String,String>();
	
	public ContentDatabase() {
		super();
	}
	
	public ContentDatabase(String dbServer, String domain, String implClassName, String key) {
		super(dbServer, implClassName);
		setDomain(domain);
		setKey(key);
	}


	public String getKey() {
		return key;
	}


	public void setKey(String key) {
		this.key = key.toLowerCase();
	}


	public String getDomain() {
		return domain;
	}


	public void setDomain(String domain) {
		this.domain = domain.toLowerCase();
	}


	public boolean isClientRestrictionsEnabled() {
		return clientRestrictionsEnabled;
	}


	public void setClientRestrictionsEnabled(boolean clientRestrictionsEnabled) {
		this.clientRestrictionsEnabled = clientRestrictionsEnabled;
	}


	public List<ClientRestriction> getClientRestrictions() {
		return clientRestrictions;
	}


	public List<FieldMapping> getFieldMappings() {
		return fieldMappings;
	}


	public String getTitle() {
		return title;
	}


	public void setTitle(String title) {
		this.title = title;
	}

    public Map<String, String> getPublisherOptions() {
    	return publisherOptions;
    }

    public void setPublisherOptions(Map<String, String> publisherOptions) {
    	if (publisherOptions == null) {
    		this.publisherOptions = new HashMap<String, String>();
    	} else {
    		this.publisherOptions = publisherOptions;
    	}
    }
    
    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = new ArrayList<ModuleDefinition>();
        
        if (property.getName().equals("publisherOptions") && getClass().getName().equals(ContentDatabase.class.getName())) {
            list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.ContentDatabasePublisherOptionsModuleType", "de.innovationgate.wgpublisher.modules.poptions.ContentDatabasePublisherOptionsCollector"));
        }
        
        return list;
        
    }

}
