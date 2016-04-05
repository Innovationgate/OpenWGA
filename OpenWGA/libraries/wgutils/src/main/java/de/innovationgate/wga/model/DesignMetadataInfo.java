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



/**
 * bean representing information for DesignMetadata
 *
 */
public class DesignMetadataInfo {
    
    private static final String METANAME_DESCRIPTION = "description";

	private String description = "";

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	
	public void setMetadataFromHeader(String property, String value) {
        
        if (property.equalsIgnoreCase(METANAME_DESCRIPTION)) {
            setDescription(value);
        }
        
    }
	
    public void processDesignHeader(String line) throws IllegalArgumentException {
        
        if (line.startsWith("## ")) {
            // Comment header - Do nuffin'
        }
        else if (line.startsWith("##MDSET ")) {
            String instruction = line.substring(8);
            int equalPos = instruction.indexOf("=");
            if (equalPos == -1) {
                throw new IllegalArgumentException("Invalid design file header: " + line);
            }
            
            try {
                String property = instruction.substring(0, equalPos).trim();
                String value = instruction.substring(equalPos + 1).trim();
                setMetadataFromHeader(property, value);
            }
            catch (RuntimeException e) {
                throw new IllegalArgumentException("Unable to set metadata by file header: " + line, e);
            }
        }
        else {
            throw new IllegalArgumentException("Unknown design file header: " + line);
        }
        
        
    }
	
	
}
