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
import java.util.List;

import org.simpleframework.xml.Element;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * An OpenWGA application configuration (misnamed)
 */
@Root(strict=false)
public class ContentStore extends ContentDatabase {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Element(required=false)
	private Design design;
	
    @Element(required=false)
    private Design overlay;
	
	@Element(required=false)
	private String defaultLanguage;
	
	@Element(required=false)
	@NotNull
	private LuceneIndexConfiguration luceneIndexConfiguration = new LuceneIndexConfiguration();

	public ContentStore() {
		super();
	}

	public ContentStore(String dbServer, String domain, String implClassName, String key, String defaultLanguage) {
		super(dbServer, domain, implClassName, key);
		setDefaultLanguage(defaultLanguage);
	}		

	public Design getDesign() {
		return design;
	}

	public void setDesign(Design design) {
		this.design = design;
	}

	public String getDefaultLanguage() {
		return defaultLanguage;
	}

	public void setDefaultLanguage(String defaultLanguage) {
		this.defaultLanguage = defaultLanguage;
	}

	public LuceneIndexConfiguration getLuceneIndexConfiguration() {
		return luceneIndexConfiguration;
	}

    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = super.getOptionDefinitions(registry, property, config);
        
        if (property.getName().equals("publisherOptions")) {
            list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.ContentDatabasePublisherOptionsModuleType", "de.innovationgate.wgpublisher.modules.poptions.ContentStorePublisherOptionsCollector"));
        }
        
        return list;
        
    }

    public Design getOverlay() {
        return overlay;
    }

    public void setOverlay(Design overlay) {
        this.overlay = overlay;
    }


}
