package de.innovationgate.wga.server.api;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.SAXReader;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class WGAFile {

	File _file;
	WGA _wga;
	
	public static String DEFAULT_ENCODING = "utf8";
	
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
	
	public String asString(String encode) throws IOException{
		FileInputStream inputStream = new java.io.FileInputStream(_file);
		String text = IOUtils.toString(inputStream, encode);
		inputStream.close();
		return text;
	}
	public String asString() throws IOException{
		return asString(DEFAULT_ENCODING);
	}
	
	public Document asXMLDocument() throws DocumentException{
		SAXReader reader = new SAXReader();
		return reader.read(_file);
	}

	public void write(String text) throws IOException{
		FileWriter out = new java.io.FileWriter(_file);
		IOUtils.write(text, out);
		out.close();
	}
	public void append(String text) throws IOException{
		FileWriter out = new java.io.FileWriter(_file, true);
		IOUtils.write(text, out);
		out.close();
	}
	
	public void copy(InputStream in) throws IOException{
		FileOutputStream out = new java.io.FileOutputStream(_file);
		IOUtils.copy(in, out);
		out.close();
	}
	public void copy(WGDocument doc, String filename) throws WGAPIException, IOException{
		copy(doc.getFileData(filename));
	}
	public void copy(TMLContext ctx, String filename) throws WGAPIException, IOException{
		copy(ctx.content().getFileData(filename));
	}

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
