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

package de.innovationgate.wgpublisher.webtml.form;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.codec.binary.Hex;

import de.innovationgate.utils.MD5HashingInputStream;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContext;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * A context for continuous processes on a user session spanning multiple requests. Used on the POST chain of WebTML forms
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE, beanMode=CodeCompletion.BEAN_MODE_ALL)
public class TMLFormProcessContext extends ProcessContext implements Map<String,Object> {
    
    /**
     * A file attached to a process context
     */
    public interface PCFile {
        
        public String getMd5Checksum() throws WGAPIException;
        
        public InputStream getData() throws IOException;
        
        public String getName() throws WGAPIException;
        
        public File getDiskFile() throws WGAPIException, IOException;
        
        public long getSize() throws IOException;
        
        public long lastModified() throws WGAPIException;
        
        public void openSession(HttpServletRequest req, WGACore core) throws WGException;
        
    }
    
    /**
     * A process context file stored on disk
     */
    public class DiskPCFile implements PCFile {
        
        private String _name;
        private File _file;
        private String _md5Checksum;

        public DiskPCFile(File file, String name, String md5sum) {
            _file = file;
            _name = name;
            _md5Checksum = md5sum;
        }

        public String getMd5Checksum() {
            return _md5Checksum;
        }

        public InputStream getData() throws IOException {
            return new FileInputStream(_file);
        }

        public String getName() {
            return _name;
        }

        public File getDiskFile() {
            return _file;
        }

        public long getSize() {
            return _file.length();
        }

        @Override
        public long lastModified() throws WGAPIException {
            return _file.lastModified();
        }

        @Override
        public void openSession(HttpServletRequest req, WGACore core) throws WGAPIException {
        }
        
    }
    
    /**
     * A process context file that points to a file attached to a document
     */
    public class DocumentPCFile implements PCFile {
        
        private WGDocument _doc;
        private String _fileName;
        
        private TemporaryFile _diskFile = null;

        public DocumentPCFile(WGDocument doc, String fileName) {
            _doc = doc;
            _fileName = fileName;
        }
    
        @Override
        public String getMd5Checksum() throws WGAPIException {
            return _doc.getFileMetaData(_fileName).getMd5Checksum();
        }
    
        @Override
        public InputStream getData() throws IOException {
            try {
                return _doc.getFileData(_fileName);
            }
            catch (WGAPIException e) {
                throw new IOException("Exception retrieving document file data", e);
            }        
        }
    
        @Override
        public String getName() {
            return _fileName;
        }
    
        @Override
        public synchronized File getDiskFile() throws WGAPIException, IOException {

            if (_diskFile == null) {
                _diskFile = new TemporaryFile(_fileName, getData(), WGFactory.getTempDir());
            }
            
            return _diskFile.getFile();
            
        }
        
        @Override
        protected void finalize() throws Throwable {
            if (_diskFile == null) {
                _diskFile.delete();
                _diskFile = null;
            }
        }
    
        @Override
        public long getSize() throws IOException {
            try {
                return _doc.getFileMetaData(_fileName).getSize();
            }
            catch (WGAPIException e) {
                throw new IOException("Exception retrieving WebTML form file size", e);
            }
        }

        @Override
        public long lastModified() throws WGAPIException {
            return _doc.getFileMetaData(_fileName).getLastmodified().getTime();
        }
        
        @Override
        public void openSession(HttpServletRequest req, WGACore core) throws WGException {
            core.openContentDB(_doc.getDatabase(), req, false);
        }

        public WGDocument getDoc() {
            return _doc;
        }
    
    }

    Map<String, PCFile> _files = new ConcurrentHashMap<String, PCFile>();
    private WGDocument _createdDoc = null;
    
    /**
     * Constructor. Only for internal use
     * @param processid The process id
     * @param parentId The id of the entity to which this process context should belong
     * @param registration The registration map for process contexts
     */
    public TMLFormProcessContext(String processid, String parentId, ProcessContextRegistration registration) {
        super(processid, parentId, registration);
    }
    
    /**
     * Returns all files attached to this process context
     */
    public Map<String, PCFile> getFiles() {
        return _files;
    }
    
    /**
     * Remove a file from this process context
     * @param filename Name of the file
     * @return true if the file was found and removed, false if not
     */
    public boolean removeFile(String filename) {
        
        PCFile file = _files.remove(filename.toLowerCase());
        if (file == null) {
            return false;
        }

        return true;

    }

    /**
     * Add an uploaded file to the process context
     * @param in The data of the file
     * @param fileName The name of the file
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    public void addFile(InputStream in, String fileName) throws IOException, NoSuchAlgorithmException {
        
        MD5HashingInputStream hashIn = new MD5HashingInputStream(in);
        
        TemporaryFile tempFile = new TemporaryFile(fileName, hashIn, TMLContext.getThreadMainContext().getwgacore().getWgaTempDir());
        DiskPCFile pcFile = new DiskPCFile(tempFile.getFile(), fileName, new String(Hex.encodeHex(hashIn.getHashBytes())));
        tempFile.deleteOnEviction(pcFile);

        _files.put(fileName.toLowerCase(), pcFile);

        
    }
    
    protected void addDocumentFile(WGDocument doc, String fileName) {
        _files.put(fileName.toLowerCase(), new DocumentPCFile(doc, fileName));
    }

    /**
     * Remove all files on this process context
     */
    public void clearFiles() {
        _files.clear();
    }

    /**
     * Returns a document that was created on a process in this context
     */
    public WGDocument getCreatedDoc() {
        return _createdDoc;
    }

    /**
     * Set a document that was created on a process in this context
     */
    protected void setCreatedDoc(WGDocument createdDoc) {
        _createdDoc = createdDoc;
    }
    
    @Override
    protected void reset() {
        super.reset();
        _files.clear();
    }
    

}
