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

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.Root;


/**
 * A domain configuration
 */
@Root(strict=false)
public class Domain extends IdentifiableConfigBean {
	
    private static final long serialVersionUID = 1L;

    @Attribute
	@NotNull
	private String name;
	
	@Element(required=false)
	@NormalizeEmptyValue
	private String defaultManager;
	
	@Element(required=false)
	private int maximumLoginAttempts = 5;

	@Element(required=false)
	private String pwdSelfserviceURL;

	@Element(required=false)
	private AuthenticationSource authenticationSource;

	@Element(required=false)
	private PersonalisationDatabase personalisation;

	public Domain() {
	    super();
	}
	
	/**
	 * Constructor to create nonpersistent domains for specific names (for plugin domains for example)
	 * Not to be used for WGA configuration file 
	 * @param uid
	 */
	public Domain(String uid) {
	    super();
	    setUid(uid);
	}

	public String getPwdSelfserviceURL(){
		return pwdSelfserviceURL;
	}

	public void setPwdSelfserviceURL(String url){
		pwdSelfserviceURL= url;
	}

	public String getDefaultManager() {
		return defaultManager;
	}

	public void setDefaultManager(String defaultManager) {
		this.defaultManager = defaultManager;
	}

	public int getMaximumLoginAttempts() {
		return maximumLoginAttempts;
	}

	public void setMaximumLoginAttempts(int maximumLoginAttempts) {
		this.maximumLoginAttempts = maximumLoginAttempts;
	}

	public AuthenticationSource getAuthenticationSource() {
		return authenticationSource;
	}
	
	public void createAuthenticationSource(String implClassName) {
		authenticationSource = new AuthenticationSource(implClassName);
	}

	public void removeAuthenticationSource() {
		authenticationSource = null;
	}
	
	public PersonalisationDatabase getPersonalisation() {
		return personalisation;
	}

	public void setPersonalisation(PersonalisationDatabase personalisation) {
		this.personalisation = personalisation;
	}
	
	public void createFileBasedAuthentication(String xmlAuthFile) {
		createAuthenticationSource("de.innovationgate.webgate.api.auth.FileAuthenticationModule");
		getAuthenticationSource().getOptions().put("auth.file", xmlAuthFile);
	}
	
	public void setAuthenticationSource(AuthenticationSource authenticationSource) {
		this.authenticationSource = authenticationSource;
	}

	public String toString() {
		StringBuffer result = new StringBuffer();
		result.append(getName());
		return result.toString();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name.toLowerCase();
	}
}
