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
import org.simpleframework.xml.Root;

import de.innovationgate.utils.security.HashedPassword;
import de.innovationgate.utils.security.HashingException;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * An administrator login
 * for wga
 */
@Root(strict=false)
public class Administrator extends IdentifiableConfigBean {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(name="username")
	@NotNull
	private String _username;
	
	@Attribute(name="password")
	@NotNull
	private String _password;
	
	@Attribute(name="encoding",required=false)
	@NotNull
	private String _encoding = ENCODING_HASH;
	
	public static final String ENCODING_HASH = "hash";
	public static final String ENCODING_NONE = "none";
	
	public Administrator() {
	    super();
	}
	
	public Administrator(String username, String password) {
		this(username, password, HashedPassword.FALLBACK_SCHEME);
	}
	
	public Administrator(String username, String password, String scheme) {
	    super();
		setUsername(username);
		setEncoding(scheme);
		setPassword(password);
	}	

	public String getUsername() {
		return _username;
	}

	public void setUsername(String username) {
		this._username = username;
	}

	public String getPassword() {
		return _password;
	}

	public void setPassword(String password) {
		this._password = password;
	}
	
	public boolean isPasswordCorrect(String password, ModuleRegistry reg) throws HashingException {
	    
		if (password == null) {
		    return false;
		}
	
		// No encoding at all
		if (getEncoding().equalsIgnoreCase(ENCODING_NONE)) {
			return password.equals(getPassword());
		}
		
		// Check password based on hashing scheme
		if (getEncoding().equalsIgnoreCase(ENCODING_HASH)) {
		      HashedPassword hashedPwd = new HashedPassword(getPassword());
		      return hashedPwd.check(password, reg);
		}
		
		return false;

	}

	public String getEncoding() {
		return _encoding;
	}

	public void setEncoding(String encoding) {
		this._encoding = encoding;
	}
}
