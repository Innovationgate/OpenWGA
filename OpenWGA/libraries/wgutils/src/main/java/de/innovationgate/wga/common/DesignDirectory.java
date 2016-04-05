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

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;





/**
 * Holds constants about WGA design directory structure
 *
 */
public class DesignDirectory {
    
    /**
     * Script module type "groovy"
     */
    private static final String SCRIPTTYPE_GROOVY = "groovy";
    /**
     * Script module type "Visual Basic Script"
     */
    public static final String SCRIPTTYPE_VBS = "vbs";
    /**
     * Script module type "XML"
     */
    public static final String SCRIPTTYPE_XML = "xml";
    /**
     * Script module type "TMLScript"
     */
    public static final String SCRIPTTYPE_TMLSCRIPT = "tmlscript";
    /**
     * Script module type "JavaScript"
     */
    public static final String SCRIPTTYPE_JS = "js";
    /**
     * Script module type "CSS"
     */
    public static final String SCRIPTTYPE_CSS = "css";
    
    /**
     * Cumumated information about a script module type in design synchronisation
     */
    public static class ScriptInformation {
        
        private boolean _autoCreate;


        public ScriptInformation(String type, String folder, String suffix, boolean autoCreate) {
            _type = type;
            _folder = folder;
            _suffix = suffix;
            _autoCreate = autoCreate;
            
            _scriptTypes.put(type, this);
            _scriptTypesBySuffix.put(suffix, this);
            
        }
        
        private String _suffix;
        private String _folder;
        private String _type;
        
        
        /**
         * Returns the script module folder in design directory
         */
        public String getFolder() {
            return _folder;
        }
        /**
         * Sets the script module folder in design directory
         * @param folder The folder to set.
         */
        public void setFolder(String folder) {
            this._folder = folder;
        }
        /**
         * Returns the file suffix for module files
         */
        public String getSuffix() {
            return _suffix;
        }
        /**
         * Sets the file suffix for module files
         * @param suffix The suffix to set.
         */
        public void setSuffix(String suffix) {
            this._suffix = suffix;
        }
        /**
         * Returns script type as constant SCRIPTTYPE_...
         * @return Returns the type.
         */
        public String getType() {
            return _type;
        }
        /**
         * Sets the script type.
         * @param type The type to set. Use constants SCRIPTTYPE_...
         */
        public void setType(String type) {
            _type = type;
        }
        /**
         * Returns if the script folder should be automatically created by design synchronisation if it does not exist
         */
        public boolean isAutoCreate() {
            return _autoCreate;
        }
    }
    /**
     * Name of the (deprecated) sync information file
     */
    public static final String SYNCINFO_FILE = "syncinfo.xml";
    
    /**
     * Name of the design definition file
     */
    public static final String DESIGN_DEFINITION_FILE = "design.xml";
    
    /**
     * Flag file name indicating design obfuscation
     */
    public static final String OBFUSCATE_FLAGFILE = "obfuscate.flg";
    
    /**
     * Name of design metadata directory
     */
    public static final String NAME_METADATADIR = "metadata";

    /**
     * Suffix for WebTML module files
     */
    public static final String SUFFIX_TML = ".tml";
    /**
     * Suffix for Script module files of type "CSS"
     */
    public static final String SUFFIX_SCRIPT_CSS = ".css";
    /**
     * Suffix for design metadata files
     */
    public static final String SUFFIX_METADATA = ".metadata.xml";
    /**
     * Suffix for Script module files of type "JavaScript"
     */
    public static final String SUFFIX_SCRIPT_JAVASCRIPT = ".js";
    /**
     * Suffix for Script module files of type "Visual Basic Script"
     */
    public static final String SUFFIX_SCRIPT_VBS = ".vbs";
    /**
     * Suffix for Script module files of type "TMLScript"
     */
    public static final String SUFFIX_SCRIPT_TMLSCRIPT = ".tmlscript";
    /**
     * Suffix for Script module files of type "XML"
     */
    public static final String SUFFIX_SCRIPT_XML = ".xml";
    /**
     * Suffix for Script module files of type "Groovy"
     */
    public static final String SUFFIX_SCRIPT_GROOVY = ".groovy";
    /**
     * Name of the optional design folder containing java classes
     */
    public static final String FOLDERNAME_JAVA = "java";
    /**
     * Name of the design folder containing WebTML modules
     */
    public static final String FOLDERNAME_TML = "tml";
    /**
     * Name of the design folder containing Script modules
     */
    public static final String FOLDERNAME_SCRIPT = "scripts";
    /**
     * Name of the design folder containing file container directories
     */
    public static final String FOLDERNAME_FILES = "files";
    /**
     * Name of the folder containing Script modules of type "TMLScript"
     */
    public static final String FOLDERNAME_SCRIPT_TMLSCRIPT = SCRIPTTYPE_TMLSCRIPT;
    /**
     * Name of the folder containing Script modules of type "XML"
     */
    public static final String FOLDERNAME_SCRIPT_XML = SCRIPTTYPE_XML;
    /**
     * Name of the folder containing Script modules of type "CSS"
     */
    public static final String FOLDERNAME_SCRIPT_CSS = SCRIPTTYPE_CSS;
    /**
     * Name of the folder containing Script modules of type "Groovy"
     */
    public static final String FOLDERNAME_SCRIPT_GROOVY = SCRIPTTYPE_GROOVY;
    /**
     * Name of the folder containing Script modules of type "JavaScript"
     */
    public static final String FOLDERNAME_SCRIPT_JAVASCRIPT = SCRIPTTYPE_JS;
    /**
     * Name of the folder containing Script modules of type "Visual Basic Script"
     */
    public static final String FOLDERNAME_SCRIPT_VBS = SCRIPTTYPE_VBS;
    
    
    private static final Map _scriptTypes = new HashMap();
    /**
     * Map containing {@link ScriptInformation} objects, mapped by their script file suffixes (constants SUFFIX_SCRIPT_...)
     */
    private static final Map _scriptTypesBySuffix = new HashMap();
    static {
        new ScriptInformation(SCRIPTTYPE_CSS, DesignDirectory.FOLDERNAME_SCRIPT_CSS, DesignDirectory.SUFFIX_SCRIPT_CSS, true);
        new ScriptInformation(SCRIPTTYPE_JS , DesignDirectory.FOLDERNAME_SCRIPT_JAVASCRIPT, DesignDirectory.SUFFIX_SCRIPT_JAVASCRIPT, true);
        new ScriptInformation(SCRIPTTYPE_TMLSCRIPT, DesignDirectory.FOLDERNAME_SCRIPT_TMLSCRIPT, DesignDirectory.SUFFIX_SCRIPT_TMLSCRIPT, true);
        new ScriptInformation(SCRIPTTYPE_XML, DesignDirectory.FOLDERNAME_SCRIPT_XML, DesignDirectory.SUFFIX_SCRIPT_XML, true);
        //new ScriptInformation(SCRIPTTYPE_VBS, DesignDirectory.FOLDERNAME_SCRIPT_VBS, DesignDirectory.SUFFIX_SCRIPT_VBS, true);
        //new ScriptInformation(SCRIPTTYPE_GROOVY, DesignDirectory.FOLDERNAME_SCRIPT_GROOVY, DesignDirectory.SUFFIX_SCRIPT_GROOVY, false);
    }
    
    /**
     * Returns a Map containing {@link ScriptInformation} objects, mapped by their script types (constants SCRIPTTYPE_...)
     */
    public static Map getScriptTypes() {
        return _scriptTypes;
    }
    
    /**
     * Returns script type information for the given script type
     * @param type The script type. Use constants SCRIPTTYPE_...
     */
    public static ScriptInformation getScriptInformation(String type) {
        return (ScriptInformation) _scriptTypes.get(type);
    }
    
    /**
     * Returns script type information for the script type whose design files use the given suffix
     * @param suffix The suffix for script files
     */
    public static ScriptInformation getScriptInformationBySuffix(String suffix) {
        return (ScriptInformation) _scriptTypesBySuffix.get(suffix);
    }
    
    public static FileObject getDesignDefinitionFile(FileObject parent) throws FileSystemException {
        
        FileObject defFile = parent.getChild(DESIGN_DEFINITION_FILE);
        if (defFile == null) {
            defFile = parent.getChild(SYNCINFO_FILE);
        }
        return defFile;
        
    }
    
    public static File getDesignDefinitionFile(File parent) {
        
        File defFile = new File(parent, DESIGN_DEFINITION_FILE);
        if (!defFile.exists()) {
            defFile = new File(parent, SYNCINFO_FILE);
        }
        return defFile;
        
    }
    

    /**
     * Returns the expected TMLScript object name from the name of a TMLScript module file
     * @param sourceFileName module file name;
     */
    public static String getTMLScriptObjectName(String sourceFileName) {
        String withoutSuffix = sourceFileName.substring(0, sourceFileName.lastIndexOf("."));
        String objectName;
        int objectNameDelimiterPos = withoutSuffix.lastIndexOf(".");
        if (objectNameDelimiterPos != -1) {
            objectName = withoutSuffix.substring(objectNameDelimiterPos + 1);
        }
        else {
            objectName = withoutSuffix;
        }
        return objectName;
    }

    
}
