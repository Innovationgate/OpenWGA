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
/**
 * 
 */
package de.innovationgate.wga.model;


import java.util.LinkedHashMap;
import java.util.Map;

import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;

public class VersionCompliance extends KeyValueBean<String, String> {
	
	public VersionCompliance(String compliance, String title) {
		super(compliance, title);
	}
	
	public static final String VERSIONCOMPLIANCE_DEFAULT = CSConfig.VERSIONCOMPLIANCE_WGA76;
    public static final String VERSIONCOMPLIANCE_MAX = CSConfig.VERSIONCOMPLIANCE_WGA76;
    
    public static final Map<String, VersionCompliance> VERSIONCOMPLIANCES = new LinkedHashMap<String, VersionCompliance>();
    static {
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA3, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA3, "WGA 3.x"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA4, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA4, "WGA 4.0"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA41, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA41, "WGA 4.1"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA50, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA50, "WGA 5.0"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA51, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA51, "WGA 5.1"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA52, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA52, "WGA 5.2"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA53, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA53, "WGA 5.3"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA54, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA54, "WGA 5.4"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA55, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA55, "WGA 5.5"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA60, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA60, "WGA 6.0"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA61, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA61, "WGA 6.1"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA62, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA62, "WGA 6.2"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA63, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA63, "WGA 6.3"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA70, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA70, "WGA 7.0"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA71, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA71, "WGA 7.1"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA72, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA72, "WGA 7.2"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA73, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA73, "WGA 7.3"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA74, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA74, "WGA 7.4"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA75, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA75, "WGA 7.5"));
        VERSIONCOMPLIANCES.put(CSConfig.VERSIONCOMPLIANCE_WGA76, new VersionCompliance(CSConfig.VERSIONCOMPLIANCE_WGA76, "WGA 7.6"));

    }
    
    public static VersionCompliance get(String complianceString) {
    	VersionCompliance v = VERSIONCOMPLIANCES.get(complianceString);
    	if(v!=null)
    		return v;
    	else return VERSIONCOMPLIANCES.get(VERSIONCOMPLIANCE_MAX);
    }
     
    public static VersionCompliance get(Version wgaVersion) {
        if (wgaVersion.isAtLeast(4, 1)) {
            return VERSIONCOMPLIANCES.get("wga" + wgaVersion.getMajorVersion() + "." + wgaVersion.getMinorVersion());
        } else if (wgaVersion.isAtLeast(4, 0)) {
            return VERSIONCOMPLIANCES.get(CSConfig.VERSIONCOMPLIANCE_WGA4);
        } else {
            return VERSIONCOMPLIANCES.get(CSConfig.VERSIONCOMPLIANCE_WGA3);
        }
    }
    
	public Version toWGAVersion() {
		return toWGAVersion(this);
	}
	
	public static Version toWGAVersion(VersionCompliance versionCompliance) {
        String currentNumericPart = versionCompliance.getComplianceString().substring(3);
        
        String majorPart = currentNumericPart;
        String minorPart = "0";
        int pointIdx = currentNumericPart.indexOf(".");
        if (pointIdx != -1) {
            majorPart = currentNumericPart.substring(0, pointIdx);
            minorPart = currentNumericPart.substring(pointIdx + 1);
        }
        int majorVersion = Integer.parseInt(majorPart);
        int minorVersion = Integer.parseInt(minorPart);
        return new Version(majorVersion, minorVersion, 0);
	}
	
	public String getComplianceString() {
	    return getKey();
	}
	
	public String getTitle() {
	    return getValue();
	}
}
