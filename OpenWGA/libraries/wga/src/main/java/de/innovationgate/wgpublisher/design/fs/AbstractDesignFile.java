/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.design.fs;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.crypto.CipherInputStream;

import org.apache.commons.vfs2.Capability;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.log4j.Logger;

import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.model.DesignMetadataInfo;
import de.innovationgate.wga.model.FCMetadataInfo;
import de.innovationgate.wga.model.ScriptMetadataInfo;
import de.innovationgate.wga.model.TMLMetadataInfo;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager.DesignInformation;
import de.innovationgate.wgpublisher.design.sync.DesignSyncManager;
import de.innovationgate.wgpublisher.design.sync.ScriptDeployment;
import de.innovationgate.wgpublisher.design.sync.TMLDeployment;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;

public abstract class AbstractDesignFile {
    
    public transient static final Logger LOG = Logger.getLogger("wga.filedesign");
    public static final String FILECONTAINER_METADATA_FILENAME = "filecontainer";
    private int _type;
    private String _name;
    private String _suffix = null;
    private String _codeFilePath;
    private transient Date _objectCreated;
    
    protected AbstractDesignFile() {
    }
    
    public AbstractDesignFile(FileSystemDesignManager manager, String codeFilePath, String codeFileName, int type) throws FileSystemException, WGDesignSyncException {
        _objectCreated = new Date();
        _type = type;
        _codeFilePath = codeFilePath;
        
        // Determine Name
        int suffixPos = codeFileName.lastIndexOf(".");
        if (suffixPos != -1) {
            _name = codeFileName.substring(0, suffixPos);
            _suffix = codeFileName.substring(suffixPos + 1);
        }
        else {
            _name = codeFileName;
        }
    }
    
    public AbstractDesignFile(FileSystemDesignManager manager, FileObject file, int type) throws FileSystemException, WGDesignSyncException {
        this(manager, manager.getRelativePath(file), file.getName().getBaseName(), type);
    }
    
    protected Logger getLog() {
        return LOG;
    }
    
    protected abstract FileSystemDesignManager getManager();

    /**
     * @return Returns the codeFile.
     * @throws FileSystemException 
     * @throws WGDesignSyncException 
     */
    public FileObject getCodeFile() throws FileSystemException, WGDesignSyncException {
        return getManager().getBaseFolder().resolveFile(_codeFilePath);
    }

    /**
     * creates a reader which depends on the configured fileEncoding for DesignSync
     * F000037B2
     * @param file
     * @return
     * @throws UnsupportedEncodingException
     * @throws FileNotFoundException
     * @throws FileSystemException 
     */
    protected Reader createReader(FileObject file) throws UnsupportedEncodingException, FileNotFoundException, FileSystemException {
        
        InputStream in = file.getContent().getInputStream();
        
        // If obfuscation enabled, use cipher to read code/metadata of script and tml deployments
        DESEncrypter cipher = getManager().getCipher();
        if (cipher != null && (getType() == WGDocument.TYPE_TML || getType() == WGDocument.TYPE_CSSJS)) {
            in = new CipherInputStream(in, cipher.getDcipher());
        }
               
        String encoding = getManager().getFileEncoding();
        // No more used because this creates a lot of problems with empty files
        // CharsetDecoder deco = Charset.forName(encoding).newDecoder();
        // deco.onMalformedInput(CodingErrorAction.REPORT);
        // deco.onUnmappableCharacter(CodingErrorAction.REPORT);
        return new BufferedReader(new InputStreamReader(in, encoding));
    
    }

    protected String readCode(DesignMetadata md) throws FileNotFoundException, IOException, WGDesignSyncException, InstantiationException, IllegalAccessException {
        
        // No, filecontainers have no code, but thanks for asking....
        if (getType() == WGDocument.TYPE_FILECONTAINER) {
            return null;
        }
        
        FileObject codeFile = getCodeFile();
        if (!codeFile.exists()) {
            throw new WGDesignSyncException("Code of file '" + getCodeFile().getName().getPath() + "' could not be read because the file does not exist.");
        }
        
    
        LineNumberReader reader = new LineNumberReader(createReader(codeFile));
        StringWriter writer = new StringWriter();
        int headerLines = 0;
        try {
            String line;
            boolean lookForHeaders = true;
            boolean firstLine = true;
            
            while ((line = reader.readLine()) != null) {
                if (lookForHeaders == true && line.startsWith("##")) {
                    processDesignHeader(line, md.getInfo());
                    headerLines++;
                }
                else {
                    lookForHeaders = false;

                    if (!firstLine) {
                        writer.write("\n");
                    }
                    else {
                        firstLine = false;
                    }
                    
                    writer.write(line);
                }
            }
        }
        finally {
            reader.close();
            codeFile.getContent().close();
        }
        writer.close();
        md.setHeaderLines(headerLines);
        String code = writer.toString();
        return code;
    }

    public void processDesignHeader(String line, DesignMetadataInfo md) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        if (line.startsWith("##MDFILE")) {
            //getOrCreateMetadataFile();
        }
        else  {
            try {
                md.processDesignHeader(line);
            }
            // Thrown on problems parsing the design header
            catch (IllegalArgumentException e) {
                getLog().error(e.getMessage());
            }
        }
        
        
    }

    /**
     * @return Returns the type.
     */
    public int getType() {
        return _type;
    }

    protected Date getObjectCreated() {
        return _objectCreated;
    }

    protected FileObject getOrCreateMetadataFile() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        FileObject metadataFile = getMetadataFile();
        
        if (!metadataFile.exists() && metadataFile.getFileSystem().hasCapability(Capability.CREATE)) {
            getOrCreateMetadataDir();
            createMetadataFile(metadataFile);
        }
        
        return metadataFile;
    }

    protected void createMetadataFile(FileObject metadataFile) throws InstantiationException, IllegalAccessException, UnsupportedEncodingException, FileSystemException, IOException {
        DesignMetadata metadata = createDefaultMetadata();
        Writer writer = new OutputStreamWriter(metadataFile.getContent().getOutputStream(), getManager().getFileEncoding());
        writer.write(DesignSyncManager.getXstream().toXML(metadata.getInfo()));
        writer.close();
    }

    private FileObject getOrCreateMetadataDir() throws FileSystemException, WGDesignSyncException {
        FileObject metadataDir = getMetadataDir();
        
        if (!metadataDir.exists() && metadataDir.getFileSystem().hasCapability(Capability.CREATE)) {
            metadataDir.createFolder();
        }
    
        return metadataDir;
    }

    private FileObject getMetadataDir() throws FileSystemException, WGDesignSyncException {
        FileObject codeFile = getCodeFile();
        FileObject metadataDir = codeFile.resolveFile("../" + DesignDirectory.NAME_METADATADIR);
        return metadataDir;
    }

    /**
     * @return Returns the metadataFile.
     * @throws IOException 
     * @throws IllegalAccessException 
     * @throws InstantiationException 
     * @throws WGDesignSyncException 
     */
    public FileObject getMetadataFile() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        if (_type == WGDocument.TYPE_FILECONTAINER) {
            return getCodeFile().resolveFile(AbstractDesignFile.FILECONTAINER_METADATA_FILENAME + DesignDirectory.SUFFIX_METADATA);   
        }
        else {
            return getMetadataDir().resolveFile(_name + DesignDirectory.SUFFIX_METADATA);
        }
    }

    protected DesignMetadata readMetaData() throws FileNotFoundException, IOException, InstantiationException, IllegalAccessException, WGDesignSyncException {
        
        FileObject metadataFile = getMetadataFile();
        
        // If not exists we return a default
        if (!metadataFile.exists()) {
            return createDefaultMetadata();
        }
        
        // If the file exists, but is empty, we put default metadata information into it
        if (metadataFile.getContent().getSize() == 0) {
            createMetadataFile(metadataFile);
        }
        
        String metaXML;
        Reader reader = createReader(metadataFile); 
        try {
            metaXML = WGUtils.readString(reader);
        }
        finally {
            reader.close();
            metadataFile.close();
        }
        
        DesignMetadata metaData;
        if (metaXML.trim().equals("")) {
            createMetadataFile(metadataFile);
            metaData = createDefaultMetadata();
        }
        else {
        	DesignMetadataInfo info =(DesignMetadataInfo) DesignSyncManager.getXstream().fromXML(metaXML);
        	if (info instanceof TMLMetadataInfo) {
        		metaData = new TMLMetadata();        		
        	} else if (info instanceof FCMetadataInfo) {
        		metaData = new FCMetadata();
        	} else if (info instanceof ScriptMetadataInfo) {
        		metaData = new ScriptMetadata();
        	} else {
        		metaData = new DesignMetadata();
        	}
    		metaData.setInfo(info);
        }
        
         
        return metaData;
    }

    protected DesignMetadata createDefaultMetadata() throws InstantiationException, IllegalAccessException {
        DesignInformation info = DesignSyncManager.getDesignInformation(_type);
        DesignMetadata metadata = (DesignMetadata) info.getMetaDataClass().newInstance();
        
        // Set dynamic metadata defaults
        if (metadata instanceof TMLMetadata) {
            TMLMetadata tmlmd = (TMLMetadata) metadata;
            tmlmd.setDirectAccess(getManager().isDirectAccessDefault());
        }
        
        return metadata;
    }

    protected List<FileObject> getFileContainerFiles() throws FileSystemException, WGDesignSyncException {
        FileObject[] filesArray = getCodeFile().getChildren();
        if (filesArray == null) {
            throw new WGDesignSyncException("Cannot collect files from directory '" + getCodeFile().getName().getPathDecoded() + "'. Please verify directory existence.");
        }
        
        List<FileObject> files = new ArrayList<FileObject>();
        for (FileObject fileobj : filesArray) {
        	if (fileobj.getType().equals(FileType.FILE) && !isExcludedFileContainerFile(fileobj)) {
        		files.add(fileobj);		
        	}
        }        
        return files;
    }
    
    protected FileObject getFileContainerFile(String name) throws FileSystemException, WGDesignSyncException {
        
        // First step: Fast lookup for the given file in the given and lower case
        FileObject file = getCodeFile().resolveFile(name);
        if (file.exists()) {
            return (!isExcludedFileContainerFile(file) ? file : null); 
        }
        
        file = getCodeFile().resolveFile(name.toLowerCase());
        if (file.exists()) {
            return (!isExcludedFileContainerFile(file) ? file : null);
        }
        
        // Second step: Slow but case insensitive search for file 
        Iterator<FileObject> files = getFileContainerFiles().iterator();
        while (files.hasNext()) {
            file = files.next();
            if (file.getName().getBaseName().equalsIgnoreCase(name)) {
                return (!isExcludedFileContainerFile(file) ? file : null);
            }
        }
        
        return null;
        
        
    }

    protected boolean isExcludedFileContainerFile(FileObject file) throws FileSystemException {
        
        if (!getManager().isValidDesignFile(file)) {
            return true;
        }
    
        if (file.getType().equals(FileType.FOLDER)) {
            return true;
        }
        
        if (file.getName().getBaseName().equals(FILECONTAINER_METADATA_FILENAME + DesignDirectory.SUFFIX_METADATA)) {
            return true;
        }
    
        return false;
    }

    public String getCodeFilePath() {
        return _codeFilePath;
    }

    public String getSuffix() {
        return _suffix;
    }

}
