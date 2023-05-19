package de.innovationgate.wga.server.api;

import java.io.IOException;

import javax.servlet.http.HttpSession;

import org.apache.log4j.Logger;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.WGPDispatcher.TemporaryDownload;
import de.innovationgate.wgpublisher.WGPDispatcher.TemporaryDownloadsMap;

public class WGATempFile extends WGAFile{

	TemporaryFile _tmpfile;
	public static final Logger LOG = Logger.getLogger("wga.tempfiles");
	
	public WGATempFile(WGA wga, String filename) throws IOException, WGException  {
		super(wga);
		_tmpfile = new TemporaryFile(filename, null, null);
		setFile(_tmpfile.getFile());

		if(_wga.isHttpSessionAvailable())
			addTemporaryFileToSession(_wga.getHttpSession());

	}

	public String addToDownloads() throws WGException{
		return WGPDispatcher.getPublisherURL(_wga.getRequest()) 
				+ "/" + WGPRequestPath.PATHCMD_TEMP_DOWNLOAD 
				+ "/" + _wga.getCore().getDispatcher().addTemporaryDownload(_wga.getRequest().getSession(), _tmpfile);	
	}
	
	public void deleteOnEviction(Object obj) {
		_tmpfile.deleteOnEviction(obj);
	}

	public void delete(){
		try{
			_tmpfile.delete();			
			LOG.info("TempFile deleted: " + getName());
		}
		catch(Exception e){}
	}
	
    private synchronized void addTemporaryFileToSession(HttpSession session) {
        TemporaryDownloadsMap temporaryFiles = (TemporaryDownloadsMap) session.getAttribute("TempFiles");
        if(temporaryFiles==null){
        	temporaryFiles = new TemporaryDownloadsMap();
        	session.setAttribute("TempFiles", temporaryFiles);
        }
        TemporaryDownload tempDownload = new TemporaryDownload(getName(), _tmpfile);
        temporaryFiles.put(_tmpfile.getFile().toString(), tempDownload);
    }

}
