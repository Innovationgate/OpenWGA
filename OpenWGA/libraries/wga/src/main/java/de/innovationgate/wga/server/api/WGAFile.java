package de.innovationgate.wga.server.api;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.WGAList.JSFunction;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class WGAFile {

	private File _file;
	protected WGA _wga;
	private String _default_encoding;

	public class ZipStream{
		
		ZipOutputStream _out;
		
		public ZipStream(OutputStream out){
			_out = new ZipOutputStream(out);
		}
				
		public ZipStream addEntry(String path, InputStream in) throws IOException{
			_out.putNextEntry(new ZipEntry(path));
			IOUtils.copy(in, _out);
			_out.closeEntry();
			return this;
		}
		
		// must only be used for empty folder
		public ZipStream addFolder(String folder) throws IOException{
			_out.putNextEntry(new ZipEntry(folder+"/"));
			return this;
		}

		public ZipStream addFile(WGAFile wgafile) throws IOException{
			return addFile(wgafile, wgafile.getFile().getName());
		}
		public ZipStream addFile(WGAFile wgafile, String path) throws IOException{
			return addFile(wgafile.getFile(), path);
		}
		
		public ZipStream addFile(File file) throws IOException{
			return addFile(file, file.getName());
		}
		public ZipStream addFile(File file, String path) throws IOException{
			try(FileInputStream in = new FileInputStream(file)){
				addEntry(path, in);
			}
			return this;
		}
		
		public ZipStream addFile(WGDocument doc, String filename) throws IOException, WGAPIException{
			return addFile(doc, filename, filename);
		}
		public ZipStream addFile(WGDocument doc, String filename, String path) throws IOException, WGAPIException{
			return addEntry(path, doc.getFileData(filename));
		}
		
		public ZipStream addFile(TMLContext ctx, String filename) throws IOException, WGAPIException{
			return addFile(ctx, filename, filename);
		}
		public ZipStream addFile(TMLContext ctx, String filename, String path) throws IOException, WGAPIException{
			return addFile(ctx.content(), filename, path);
		}
		
		public void close() throws IOException{
			_out.flush();
			_out.close();
		}
	}

	public WGAFile(WGA wga){
		_wga = wga;
		_default_encoding = wga.getCore().getCharacterEncoding();
	}

	public WGAFile(WGA wga, String filename){
		_wga = wga;
		if(filename.startsWith("/"))
			_file = new File(filename);
		else _file = new File(wga.getCore().getConfigFile().getParentFile(), filename);
		_default_encoding = wga.getCore().getCharacterEncoding();
	}
	public WGAFile(WGA wga, File file){
		_wga = wga;
		_file = file;
		_default_encoding = wga.getCore().getCharacterEncoding();
	}
	
	public WGAFile(WGA wga, WGAFile folder, String filename){
		this(wga, new File(folder.getFile(), filename));
	}

	public String getName(){
		return getFile().getName();
	}
	
	public File getFile(){
		return _file;
	}
	public void setFile(File file){
		_file = file;
	}
	
	public boolean exists(){
		if(_file==null)
			return false;
		return _file.exists();
	}
	
	public boolean isFile(){
		if(_file==null)
			return false;
		return _file.isFile();		
	}

	public boolean isDirectory(){
		if(_file==null)
			return false;
		return _file.isDirectory();		
	}
	
	public WGAFile createDir(){
		if(_file!=null && !_file.exists())
			_file.mkdir();
		return this;
	}

	public WGAFile deleteDir() throws IOException{
		if(_file!=null && _file.exists())
			FileUtils.deleteDirectory(_file);
		return this;
	}

	public WGAFile cleanDir() throws IOException{
		if(_file!=null)
			FileUtils.cleanDirectory(_file);
		return this;
	}

	public InputStream getInputStream() throws FileNotFoundException{
		if(_file==null)
			return null;
		return new FileInputStream(_file);
	}
	
	public String toString(){
		if(_file!=null)
			return _file.toString();
		else return "file not defined";
	}
	
	public ZipStream createZipStream() throws IOException{
		return new ZipStream(new FileOutputStream(_file));
	}
	
	/**
	 * Returns file content as String
	 * @param encode
	 * @return file content as string
	 * @throws IOException
	 */
	public String asString(String encode) throws IOException{
		String text = "";
		try(FileInputStream inputStream = new FileInputStream(_file)){
			text = IOUtils.toString(inputStream, encode);
		}
		return text;
	}
	public String asString() throws IOException{
		return asString(_default_encoding);
	}
	
	/**
	 * returns file content as DOM4J Document
	 * @return Dom4j document
	 * @throws DocumentException
	 */
	public Document asXMLDocument(String encode) throws DocumentException{
		SAXReader reader = new SAXReader();
		reader.setEncoding(encode);
		return reader.read(_file);
	}
	public Document asXMLDocument() throws DocumentException{
		return asXMLDocument(_default_encoding);
	}

	/**
	 * Writes a String to file
	 * @param text
	 * @throws IOException
	 */
	public void write(String text, String encode) throws IOException{
		try(FileOutputStream out = new java.io.FileOutputStream(_file)){
			IOUtils.write(text, out, encode);
		}
	}
	public void write(String text) throws IOException{
		write(text, _default_encoding);
	}
	
	/**
	 * Appends a String to file
	 * @param text
	 * @throws IOException
	 */
	public void append(String text, String encode) throws IOException{
		try(FileOutputStream out = new java.io.FileOutputStream(_file, true)){
			IOUtils.write(text, out, encode);
		}
	}
	public void append(String text) throws IOException{
		append(text, _default_encoding);
	}
	
	/**
	 * Writes an XML document to file
	 * @param xml
	 * @throws IOException
	 */
	public void write(Document xml, String encode) throws IOException {
		try(FileOutputStream out = new java.io.FileOutputStream(_file)){
			OutputFormat format = OutputFormat.createPrettyPrint();
			format.setEncoding(encode);
			XMLWriter writer = new XMLWriter(out, format);
			writer.write(xml);
			writer.flush();
			writer.close();
		}
	}
	public void write(Document xml) throws IOException {
		write(xml, _default_encoding);
	}

	/**
	 * Copy some binary content to file 
	 * @param in
	 * @throws IOException
	 */
	
	public void copy(InputStream in) throws IOException{
		try(FileOutputStream out = new java.io.FileOutputStream(_file)){
			IOUtils.copy(in, out);
		}
	}
	public void copy(WGDocument doc, String filename) throws WGAPIException, IOException{
		copy(doc.getFileData(filename));
	}
	public void copy(TMLContext ctx, String filename) throws WGAPIException, IOException{
		copy(ctx.content().getFileData(filename));
	}

	/**
	 * Returns directory list as WGAFile objects including directories
	 * @param filter
	 */
	public WGAList<WGAFile> list(JSFunction filter){
		WGAList<WGAFile> list = new WGAList<WGAFile>();
		if(_file!=null){
			File [] files = _file.listFiles();
			if(files!=null){
				for (File file: files){
					list.add(new WGAFile(_wga, file));				
				}
			}
		}
		return list.filter(filter);
	}
	public WGAList<WGAFile> list(){
		return list(null);
	}

	/**
	 * Returns directory files list as WGAFile objects
	 * @param filter
	 */
	public WGAList<WGAFile> listFiles(JSFunction f){
		return list(new JSFunction(){
			@Override
			public Object call(Object a, Object b) {
				return ((WGAFile)a).isFile();
			}			
		}).filter(f);
	}
	public WGAList<WGAFile> listFiles(){
		return listFiles(null);
	}
	
	/**
	 * Returns directory folders as WGAFile objects
	 * @param filter
	 */
	public WGAList<WGAFile> listFolders(final JSFunction f){
		return list(new JSFunction(){
			@Override
			public Object call(Object a, Object b) {
				return ((WGAFile)a).isDirectory();
			}
		}).filter(f); 
	}
	public WGAList<WGAFile> listFolders(){
		return listFolders(null);
	}
	
	/**
	 * Returns the mimeType of a file
	 */
	public String getMimeType(){
		if(_file!=null)
			return _wga.getCore().getServletContext().getMimeType(_file.getName());
		else return null;
	}
	
	/**
	 * Sets file permission. File must already exist to work.
	 * Sample: setPosixFilePermissions("rw-rw-rw-")
	 * @param permissions
	 * @throws IOException
	 */
	public void setPosixFilePermissions(String permissions) throws IOException{
		if(_file!=null){
			Files.setPosixFilePermissions(_file.toPath(), 
				PosixFilePermissions.fromString(permissions)
			);
		}
	}
	
}
