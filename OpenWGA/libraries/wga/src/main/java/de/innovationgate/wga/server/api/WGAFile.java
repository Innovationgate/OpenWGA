package de.innovationgate.wga.server.api;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class WGAFile {

	private File _file;
	private WGA _wga;
	
	public static String DEFAULT_ENCODING = "UTF-8";
	
	public WGAFile(WGA wga, String filename){
		_wga = wga;
		_file = new File(_wga.getCore().getConfigFile().getParentFile(), filename);
	}
	public WGAFile(WGA wga, File file){
		_wga = wga;
		_file = file;
	}

	public File getFile(){
		return _file;
	}
	public void setFile(File file){
		_file = file;
	}
	
	public String toString(){
		if(_file!=null)
			return _file.toString();
		else return "file not defined";
	}
	
	/**
	 * Returns file content as String
	 * @param encode
	 * @return file content as string
	 * @throws IOException
	 */
	public String asString(String encode) throws IOException{
		String text = "";
		try(FileInputStream inputStream = new java.io.FileInputStream(_file)){
			text = IOUtils.toString(inputStream, encode);
		}
		return text;
	}
	public String asString() throws IOException{
		return asString(DEFAULT_ENCODING);
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
		return asXMLDocument(DEFAULT_ENCODING);
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
		write(text, DEFAULT_ENCODING);
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
		append(text, DEFAULT_ENCODING);
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
		write(xml, DEFAULT_ENCODING);
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
	 * Returns directory list as WGAFile objects
	 * @param filter
	 * @return
	 */
	public List<WGAFile> listFiles(FileFilter filter){
		ArrayList<WGAFile> list = new ArrayList<WGAFile>();
		if(_file!=null){
			File [] files = _file.listFiles(filter);
			if(files!=null){
				for (File file: files){
					list.add(new WGAFile(_wga, file));				
				}
			}
		}
		return list;
	}
	public List<WGAFile> listFiles(){
		return listFiles(null);
	}

}
