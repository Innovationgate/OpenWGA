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
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.config.DesignReference;

public class DeployedLayout {

    private File _layoutFile;
    
    private long _providerHash;

    private String _mainLibKey;

    private String _mainLibName;

    private String _mainLibMediaKey;

    private String _mainLibDBKey;

    private boolean _deleted;

    private long _mainLibModified;

    private String _characterEncoding;

    private String _layoutKey;

    public DeployedLayout(WGTMLModule lib, String layoutKey, File folder, String characterEncoding) throws WGAPIException, NoSuchAlgorithmException {
        init(lib, layoutKey, folder, characterEncoding);
    }

    public void init(WGTMLModule lib, String layoutKey, File folder, String characterEncoding) throws WGAPIException, NoSuchAlgorithmException {
        
        _layoutKey = layoutKey;
        
        _deleted = false;
        _mainLibModified = 0;
        _characterEncoding = characterEncoding;

        _mainLibDBKey = lib.getDatabase().getDbReference();
        _mainLibName = lib.getName();
        _mainLibMediaKey = lib.getMediaKey();
        _mainLibKey = lib.getDatabase().getDbReference()  + "/" + lib.getName() + "/" + lib.getMediaKey() + (_layoutKey.contains("///") ? "/inline" : "");
        _mainLibModified = lib.getLastModified().getTime();
        
        if (lib.getDatabase().getDesignProvider() != null && lib.getDatabase().getDesignProvider().providesType(WGDocument.TYPE_TML)) {
            _providerHash = lib.getDatabase().getDesignProvider().designHashCode();
        }
        else {
            _providerHash = lib.getDatabase().hashCode();
        }

        _layoutFile = new File(folder, createFileName(layoutKey));
        
    }
    
    private String createFileName(String layoutKey)  throws WGAPIException, NoSuchAlgorithmException {
        return WGUtils.createMD5HEX(layoutKey.getBytes()) + ".jsp";
    } 

    public boolean needsUpdate(WGTMLModule mod) throws WGAPIException {

        // Look if the module was modified since deployment
        if (mod.getLastModified().getTime() != _mainLibModified) {
            return true;
        }

        return false;

    }



    public File getFile() {
        return _layoutFile;
    }

    /**
     * @see java.lang.Object#finalize()
     */
    protected void finalize() throws Throwable {
        
        // Will only delete if it is still on the same state as the last deployment
        if (_layoutFile.lastModified() != _mainLibModified) {
            return;
        }

        WGPDeployer.LOG.info("Removing tml " + this._mainLibDBKey + "/"
                    + this._mainLibName + " (" + this._mainLibMediaKey + ")");

        this._layoutFile.delete();
    }

    /**
     * Returns the deleted.
     * 
     * @return boolean
     */
    public boolean isDeleted() {
        return _deleted;
    }

    /**
     * Sets the deleted.
     * 
     * @param deleted
     *            The deleted to set
     */
    public void setDeleted(boolean deleted) {
        _deleted = deleted;
    }

    public void deploy(String code) throws IOException {
        File file = getFile();

        if (!file.exists()) {
            file.createNewFile();
        }
        //F000037B2
        Writer dspWriter = null;
        if (_characterEncoding != null) {
            dspWriter = new OutputStreamWriter(new FileOutputStream(file), _characterEncoding);
        } else {
            dspWriter = new FileWriter(file);
        }
        dspWriter.write(code);
        dspWriter.close();
    }

    public String getMainLibKey() {
        return _mainLibKey;
    }

    public String getLayoutKey() {
        return _layoutKey;
    }
    
    public boolean isInline() {
        return _layoutKey.contains("///");
    }

    public String getMainLibMediaKey() {
        return _mainLibMediaKey;
    }

    public String getMainLibName() {
        return _mainLibName;
    }

    public String getMainLibDBKey() {
        return _mainLibDBKey;
    }

    public String getResourcePath() {
        
        return "/" + WGPDeployer.FOLDER_DYNAMIC_RESOURCES + "/" + getFile().getName();

    }

}
