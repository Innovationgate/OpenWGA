package de.innovationgate.wga.server.api;

import java.io.IOException;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;

public class WGATempFile extends WGAFile {

	TemporaryFile _tmpfile;
	
	public WGATempFile(WGA wga, String filename) throws IOException {
		super(wga);
		_tmpfile = new TemporaryFile(filename, null, null);
		setFile(_tmpfile.getFile());
	}

	public String addToDownloads() throws WGException{
		return WGPDispatcher.getPublisherURL(_wga.getRequest()) 
				+ "/" + WGPRequestPath.PATHCMD_TEMP_DOWNLOAD 
				+ "/" + _wga.getCore().getDispatcher().addTemporaryDownload(_wga.getRequest().getSession(), _tmpfile);	
	}
	
}
