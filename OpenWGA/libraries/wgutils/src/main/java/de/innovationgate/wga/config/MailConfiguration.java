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

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

/**
 * Configuration of a global SMTP mail server
 */
@Root(strict=false)
public class MailConfiguration extends ConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(required=false)
	@NormalizeEmptyValue
	private String server;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String user;
	
	@Attribute(required=false)
	@NormalizeEmptyValue
	private String password;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String toAddress;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String fromAddress;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String fromName;

	@Attribute(required=false)
	private String encryption;

	@Element(required=false)
	private boolean enableAdminNotifications = false;	
	
    @ElementMap(entry="option", key="name", attribute=true, required=false)
    @NotNull
    @Options
	private Map<String,String> options = new LinkedHashMap<String,String>();

	public MailConfiguration() {	
	}

	public String getServer() {
		return server;
	}

	public void setServer(String server) {
		this.server = server;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String user) {
		this.user = user;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getFromAddress() {
		return fromAddress;
	}

	public void setFromAddress(String fromAddress) {
		this.fromAddress = fromAddress;
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

	
	public boolean isConfigured() {
		return getServer() != null && !getServer().trim().equals("");
	}

	public String getToAddress() {
		return toAddress;
	}

	public void setToAddress(String toAddress) {
		this.toAddress = toAddress;
	}

	public boolean isEnableAdminNotifications() {
		return enableAdminNotifications;
	}

	public void setEnableAdminNotifications(boolean enableAdminNotifications) {
		this.enableAdminNotifications = enableAdminNotifications;
	}

    public String getFromName() {
        return fromName;
    }

    public void setFromName(String fromName) {
        this.fromName = fromName;
    }
    
    public void setEncryption(String value){
    	this.encryption=value;
    }
    public String getEncryption(){
    	return this.encryption;
    }
}
