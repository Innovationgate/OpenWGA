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

package de.innovationgate.wga.common.beans;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.vfs2.FileObject;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.wga.common.DesignDirectory;

/**
 * Represents the definition of a design directory stored in file design.xml or syncinfo.xml (deprecated)
 *
 */
public class DesignDefinition {
    

    
    /**
     * Placeholder for file encoding field if the encoding is to be retrieved from the csconfig.xml
     */
    public static final String FILEENCODING_CSCONFIG_DEFINED = "csconfig";
    
    /**
     * Loads syncinfo data from a file. Character data is decoded as UTF-8.
     * @param file The file
     * @throws IOException
     */
    public static DesignDefinition load(File file) throws IOException {
        XStream xStream = createXStreamForFile(file.getName());
        return (DesignDefinition) XStreamUtils.loadUtf8FromFile(xStream, file);
    }
    
    /**
     * Loads syncinfo data from a VFS file object. Character data is decoded as UTF-8.
     * @param file The file object
     * @throws IOException
     */
    public static DesignDefinition load(FileObject file) throws IOException {
        XStream xStream = createXStreamForFile(file.getName().getBaseName());
        return (DesignDefinition) XStreamUtils.loadUtf8FromFileObject(xStream, file);
    }

    
    /**
     * Writes syncinfo data to a UTF-8-encoded file
     * @param file File to write to
     * @throws IOException
     */
    public synchronized void write(File file) throws IOException {
        XStream xStream = createXStreamForFile(file.getName());
        XStreamUtils.writeUtf8ToFile(this, xStream, file);
    }
    
    private static XStream createXStreamForFile(String name) {
        
        XStream xStream = XStreamUtils.createXStream();
        
        // The last registered alias wins when resolving from class to name
        // We register both aliases, but the one that should be used for writing as the last
        if (name.equals(DesignDirectory.DESIGN_DEFINITION_FILE)) {
            xStream.alias("DesignSyncInfo", DesignDefinition.class);
            xStream.alias("DesignDefinition", DesignDefinition.class);
        }
        else {
            xStream.alias("DesignDefinition", DesignDefinition.class);
            xStream.alias("DesignSyncInfo", DesignDefinition.class);
        }
        
        return xStream;
        
    }

    /**
     * Writes syncinfo data to a UTF-8-encoded VFS file object
     * @param file
     * @throws IOException
     */
    public void write(FileObject file) throws IOException {
        XStream xStream = createXStreamForFile(file.getName().getBaseName());
        XStreamUtils.writeUtf8ToFileObject(this, xStream, file);
    }
    
    private String _designKey;
    
    private String _fileEncoding = null;

    /**
     * @return Returns the designKey.
     */
    public String getDesignKey() {
        return _designKey;
    }

    /**
     * @param designKey The designKey to set.
     */
    public void setDesignKey(String designKey) {
        _designKey = designKey;
    }
    
    /**
     * Returns the encoding to use for reading and writing deigns
     */
    public String getFileEncoding() {
        if (_fileEncoding != null && _fileEncoding.trim().equals("")) {
                return null;
        } else {
            return _fileEncoding;
        }
    }

    /**
     * Sets the file encoding for reading and writing files
     */
    public void setFileEncoding(String fileEncoding) {
        _fileEncoding = fileEncoding;
    }
    

}
