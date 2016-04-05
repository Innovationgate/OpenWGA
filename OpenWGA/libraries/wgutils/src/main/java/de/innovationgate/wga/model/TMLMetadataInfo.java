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
package de.innovationgate.wga.model;

import de.innovationgate.utils.WGUtils;

/**
 * beans representing information for TMLMetadata
 *
 */
public class TMLMetadataInfo extends DesignMetadataInfo {
	
	private static final String METANAME_CATEGORY = "category";

    public static final String XSTREAM_ALIAS = "TMLMetaData";

    private static final String METANAME_DIRECTACCESS = "directaccess";

    private static final String METANAME_CACHEABLE = "cacheable";

    private static final String METANAME_CATEGORIES = "categories";

	private boolean directAccess = true;
	private boolean cacheable = false;
	private String category = "";
	
	public boolean isDirectAccess() {
		return directAccess;
	}
	
	public void setDirectAccess(boolean directAccess) {
		this.directAccess = directAccess;
	}
	
	public boolean isCacheable() {
		return cacheable;
	}
	public void setCacheable(boolean cacheable) {
		this.cacheable = cacheable;
	}
	
	public String getCategory() {
		return category;
	}
	
	
	public void setCategory(String category) {
		this.category = category;
	}
	
	@Override
	public void setMetadataFromHeader(String property, String value) {
        
        if (property.equalsIgnoreCase(METANAME_DIRECTACCESS)) {
            setDirectAccess(WGUtils.stringToBoolean(value));
        }
        else if (property.equalsIgnoreCase(METANAME_CACHEABLE)) {
            setCacheable(WGUtils.stringToBoolean(value));
        }
        else if (property.equalsIgnoreCase(METANAME_CATEGORIES) || property.equalsIgnoreCase(METANAME_CATEGORY)) {
            setCategory(value);
        }
        else {
            super.setMetadataFromHeader(property, value);
        }
        
    }
	
	
	
}
