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
import de.innovationgate.webgate.api.WGFileMetaData;
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
    
	public class FileMetaData{
		private String _title;
		private String _description;
		private String _copyright;
		private PCFile _file;
		
		public FileMetaData(PCFile file){
			_file = file;
		}
		
		public long getSize() throws IOException{
			return _file.getSize();
		}
		public String getMimeType() throws WGAPIException{
			return _file.getMimeType();
		}
		
		public String getTitle(){
			return _title;
		}
		public FileMetaData setTitle(String t){
			_title = t;
			return this;
		}
		public String getDescription(){
			return _description;			
		}
		public FileMetaData setDescription(String value){
			_description = value;
			return this;
		}
		public String getCopyright(){
			return _copyright;			
		}
		public FileMetaData setCopyright(String value){
			_copyright = value;
			return this;
		}
		
	}
	
    /**
     * A file attached to a process context
     */
    public interface PCFile {
        
        public String getMd5Checksum() throws WGAPIException;
        
        public InputStream getData() throws IOException;
        
        public String getName() throws WGAPIException;
        public void setName(String name);
        
        public void setPrimary(boolean value);
        public boolean isPrimary();
        
        public File getDiskFile() throws WGAPIException, IOException;
        
        public long getSize() throws IOException;
        
        public long lastModified() throws WGAPIException;
        
        public FileMetaData getFileMetaData();
        
        public void openSession(HttpServletRequest req, WGACore core) throws WGException;

        public String getMimeType() throws WGAPIException;
    }
    
    /**
     * A process context file stored on disk
     */
    public class DiskPCFile implements PCFile {
        
        private String _name;
        private File _file;
        private String _md5Checksum;
        private FileMetaData _metaData;
        private boolean _primary;        

        public DiskPCFile(File file, String name, String md5sum) {
            _file = file;
            _name = name;
            _md5Checksum = md5sum;
            _metaData = new FileMetaData(this);
        }

        public String getMimeType(){
        	return WGFactory.getMimetypeDeterminationService().determineByFilename(_name);
        }
        
        public FileMetaData getFileMetaData(){
        	return _metaData;
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
        public void setName(String name){
        	_name = name;
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

		@Override
		public void setPrimary(boolean value) {
			_primary = value;
		}

		@Override
		public boolean isPrimary() {
			return _primary;
		}
        
    }
    
    /**
     * A process context file that points to a file attached to a document
     */
    public class DocumentPCFile implements PCFile {
        
        private WGDocument _doc;
        private String _fileName;
        private String _orgFileName;
        private FileMetaData _metaData;
        private String _md5Checksum;
        private long _lastModified;
        private long _size;
        private boolean _primary;
        private String _mimeType;
        
        private TemporaryFile _diskFile = null;

        public DocumentPCFile(WGDocument doc, String fileName) throws WGAPIException {
            _doc = doc;
            _fileName = _orgFileName = fileName;
            _metaData = new FileMetaData(this);
            
			try {
	            _primary = doc.getPrimaryFileName() != null && doc.getPrimaryFileName().equals(fileName);
	            
	            WGFileMetaData data = _doc.getFileMetaData(_fileName);
	            
	            _md5Checksum = data.getMd5Checksum();
	            _lastModified = data.getLastmodified().getTime();
	            _size = data.getSize();
	            _mimeType = data.getMimeType();	            
	            
	            _metaData.setTitle(data.getTitle());
	            _metaData.setDescription(data.getDescription());
	            _metaData.setCopyright(data.getCopyright());
			} catch (WGAPIException e) {
				throw new WGAPIException("unable to init DocumentPCFile " + fileName);
			}
        }
    
        @Override
        public String getMimeType() throws WGAPIException{
        	return _mimeType;
        }
        
        @Override
        public String getMd5Checksum() throws WGAPIException {
            return _md5Checksum;
        }
    
        @Override
        public InputStream getData() throws IOException {
            try {
                return _doc.getFileData(_orgFileName);
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
        public void setName(String name){
        	_fileName = name;
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
        	return _size;
        }

        @Override
        public long lastModified() throws WGAPIException {
            return _lastModified;
        }
        
        @Override
        public void openSession(HttpServletRequest req, WGACore core) throws WGException {
            core.openContentDB(_doc.getDatabase(), req, false);
        }

        public WGDocument getDoc() {
            return _doc;
        }

		@Override
		public FileMetaData getFileMetaData() {
			return _metaData;
		}

		@Override
		public void setPrimary(boolean value) {
			_primary = value;
		}

		@Override
		public boolean isPrimary() {
			return _primary;
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
    
    protected void addDocumentFile(WGDocument doc, String fileName) throws WGAPIException {
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
