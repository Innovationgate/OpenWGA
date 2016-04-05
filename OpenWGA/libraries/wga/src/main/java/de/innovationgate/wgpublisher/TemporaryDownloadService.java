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

package de.innovationgate.wgpublisher;

import java.io.File;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WGAAwareService;


/**
 * A service to provide temporary downloads
 */
public class TemporaryDownloadService implements WGAAwareService {

    private WGA _wga;

    @Override
    public void injectWGA(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Adds a file as temporary download for the current user session
     * @param file The file
     * @return A download name, used to retrieve the download URL
     * @throws WGException
     */
    public String addDownload(File file) throws WGException {
        
        if (!_wga.isRequestAvailable()) {
            throw new WGAServerException("Adding temporary downloads can only be done from normal WebTML requests, not master sessions");
        }
        
        try {
            return _wga.getCore().getDispatcher().addTemporaryDownload(_wga.session().getJavaHttpSession(), file);
        }
        catch (Exception e) {
            throw new WGAServerException("Exception adding temporary download");
        }
        
    }
    
    /**
     * Returns a download URL for the given download name, taking the current request URL into account.
     * @param name The name of the download, as returned by {@link #addDownload(File)}
     * @throws WGException
     */
    public String getDownloadURL(String name) throws WGException {
        
        if (!_wga.isRequestAvailable()) {
            throw new WGAServerException("Requesting temporary download URLs can only be done from normal WebTML requests, not master sessions");
        }
        
        return WGPDispatcher.getPublisherURL(_wga.getRequest(), false) + "/tempdwn/" + name;
        
    }    
    
    
}
