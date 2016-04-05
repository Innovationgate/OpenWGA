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

package de.innovationgate.wga.common;

import java.util.regex.Pattern;

/**
 * Hosts all general WGA constants that are used on multiple "ends" and are not
 * assignable to existing WGUtils classes.
 */
public abstract class Constants {
    
    public static enum AjaxMode {
        
        NORMAL("true"),
        NOREFRESH("norefresh"),
        OFF("false");
        
        private String _value;
        
        private AjaxMode(String value) {
            _value = value;
        }
        
        @Override
        public java.lang.String toString() {
            return _value;
        }
        
    }
    
    
    // Personalisation modes
    /**
     * Personalisation mode "automatic"
     */
    public static final int PERSMODE_AUTO = 1;
    
    /**
     * Personalisation mode "Login based"
     */
    public static final int PERSMODE_LOGIN = 2;
    
    /**
     * Personalisation mode "custom / tmlscript based"
     */
    public static final int PERSMODE_CUSTOM = 3;
    
    /**
     * Personalisation mode "session"
     */
    public static final int PERSMODE_SESSION = 4;
    
    // Personalisation statistics mode
    
    /**
     * All statistics off
     */
    public static final int PERSSTATMODE_OFF = 0;
    

    /**
     * Session based statistics
     */
    public static final int PERSSTATMODE_SESSION = 1;
    
    /**
     * Session and hit based statistics
     */
    public static final int PERSSTATMODE_HIT = 2;
    
    // Default values
    /**
     * File cache default max entry size
     */
    public static final int DEFAULT_FILECACHE_ENTRIES = 1000;
    /**
     * File cache default size threshold in KB
     */
    public static final int DEFAULT_FILECACHE_THRESHOLD = 10;
    
    /**
     * Postprocessed resources cache default max entry size
     */
    public static final int DEFAULT_PPRCACHE_ENTRIES = 1000;

    
    // Frequently used regexp patterns
    /**
     * RegEx pattern for valid synchronisation filenames
     */
    public static final Pattern PATTERN_FILENAMES = Pattern.compile("[a-zA-Z0-9_\\.\\-\\: öäüÖÄÜß\\$\\(\\)]+");
    /**
     * RegEx pattern for valid design keys
     */
    public static final Pattern PATTERN_KEYS = Pattern.compile("[a-zA-Z0-9_\\.\\-\\:\\$]+");
    
    /**
     * The port for WGA remote application log
     */
    public static final int LOGSERVER_PORT = 24299;
        
    /**
     * RegEx pattern for ACL role names 
     */
    public static final Pattern PATTERN_ROLENAMES = Pattern.compile("[a-zA-Z0-9_\\-\\$\\#\\[\\]]+");

    /**
     * The user name for anonymous users
     */
    public static final String ANONYMOUS_USER = "anonymous";


    // Names of the default design collections
    public static final String DESIGNCOL_DB = "db";
    public static final String DESIGNCOL_PLUGIN = "plugin";
    public static final String DESIGNCOL_FILESYSTEM = "fs-designs";
    
    // Modes of warnings output on page
    public static final String WARNINGS_TML_AS_HTML = "html";
    public static final String WARNINGS_TML_AS_COMMENT = "comment";
    public static final String WARNINGS_TML_OFF = "off";
    

}
