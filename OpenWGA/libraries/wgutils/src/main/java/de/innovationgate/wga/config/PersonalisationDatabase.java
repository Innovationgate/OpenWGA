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
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;

/**
 * Configuration of a domain-wide personalisation database
 */
@Root(strict=false)
public class PersonalisationDatabase extends Database {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String MODE_AUTO = "auto";
	public static final String MODE_LOGIN = "login";
	
	
	public static final String STATISTICS_OFF = "off";
	public static final String STATISTICS_ON_SESSION = "session";
	public static final String STATISTICS_ON_HIT = "hit";

	public PersonalisationDatabase() {	
		super();
	}
	
	public PersonalisationDatabase(String dbServer, String implClassName) {
		super(dbServer, implClassName);
	}
	

}
