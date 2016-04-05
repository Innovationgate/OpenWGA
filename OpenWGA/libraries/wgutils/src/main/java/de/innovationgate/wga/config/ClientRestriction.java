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
import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Root;

import de.innovationgate.utils.WGUtils;

/**
 * A client restriction rule
 */
@Root(strict=false)
public class ClientRestriction extends ConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String type;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String hostIP;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String network;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String netmask;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String startIP;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String endIP;
	
    /**
     * Restriction including a single host
     */
    public static final String TYPE_HOST = "host";

    /**
     * Restriction including a network plus netmask
     */
    public static final String TYPE_NETWORK = "network";
    
    /**
     * Restriction including a network defined by CIDR Prefix
     */
    public static final String TYPE_NETWORK_CIDR_PREFIX = "network_cidr_prefix";

    /**
     * Restriction including an IP range
     */
    public static final String TYPE_RANGE = "range";
	
	public ClientRestriction() {		
	}
	
	protected ClientRestriction(String type) {
	}

	public String getNetmask() {
		return netmask;
	}
	public void setNetmask(String netmask) {
		this.netmask = netmask;
	}

	public String getHostIP() {
		return hostIP;
	}

	public void setHostIP(String hostIP) {
		this.hostIP = hostIP;
	}

	public String getNetwork() {
		return network;
	}

	public void setNetwork(String network) {
		this.network = network;
	}

	public String getStartIP() {
		return startIP;
	}

	public void setStartIP(String startIP) {
		this.startIP = startIP;
	}

	public String getEndIP() {
		return endIP;
	}

	public void setEndIP(String endIP) {
		this.endIP = endIP;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
	
	public boolean isIP6() {
	    
	    if (type.equals(TYPE_HOST)) {
	        return hostIP.contains(":");
	    }
	    else if (type.equals(TYPE_NETWORK) || type.equals(TYPE_NETWORK_CIDR_PREFIX)) {
	        return network.contains(":");
	    }
	    else if (type.equals(TYPE_RANGE)) {
	        return startIP.contains(":");
	    }
	    return false;
	    
	}
	
	@Override
	public String toString() {

	    List<String> data = new ArrayList<>();
	    data.add("Type: " + getType());
	    if (!WGUtils.isEmpty(getHostIP())) {
	        data.add("Host: " + getHostIP());
	    }
	    if (!WGUtils.isEmpty(getStartIP())) {
            data.add("Start: " + getStartIP());
        }
	    if (!WGUtils.isEmpty(getEndIP())) {
            data.add("End: " + getEndIP());
        }
	    if (!WGUtils.isEmpty(getNetwork())) {
            data.add("Network: " + getNetwork());
        }
	    if (!WGUtils.isEmpty(getNetmask())) {
            data.add("Netmask: " + getNetmask());
        }
	    return WGUtils.serializeCollection(data, ", ");
	}

}
