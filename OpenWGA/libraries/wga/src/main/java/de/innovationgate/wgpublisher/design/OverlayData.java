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

package de.innovationgate.wgpublisher.design;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.vfs2.FileObject;
import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.core.Persister;

import de.innovationgate.wga.config.ConfigValidationException;
import de.innovationgate.wga.config.NotNull;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.model.ValidationError;

@Root(strict=false)
public class OverlayData {
    
    private static Persister _serializer = new Persister();
    
    public static OverlayData load(FileObject overlayDataFile) throws Exception {
        InputStream in = new BufferedInputStream(overlayDataFile.getContent().getInputStream());
        OverlayData overlayData = OverlayData.read(in);
        in.close();
        return overlayData;
        
    }
    
    public static OverlayData read(InputStream in) throws Exception {
        return _serializer.read(OverlayData.class, in);
    }
    
    public void write(OutputStream out) throws Exception {
        _serializer.write(this, out);
    }
    
    @Root(strict=false)
    public static class ResourceData {
        
        @Attribute(name="md5Hash")
        private String _md5Hash;

        public String getMd5Hash() {
            return _md5Hash;
        }

        public void setMd5Hash(String md5Hash) {
            this._md5Hash = md5Hash;
        }
    
    }
    
    private transient boolean _updated = false;
    
    @Attribute(name="basepluginName", required=true)
    private String _basepluginName;
    
    @Attribute(name="basepluginVersion", required=true)
    private String _basepluginVersion;
    
    @Attribute(name="initialBasepluginVersion", required=false)
    private String _initialBasepluginVersion;
    
    
    @ElementMap(entry="resource", key="path", inline=true, required=false)
    @NotNull
    private Map<String,ResourceData> _overlayResources = new HashMap<String, ResourceData>();

    public String getBasepluginName() {
        return _basepluginName;
    }

    public void setBasepluginName(String designPlugin) {
        _basepluginName = designPlugin;
        _updated = true;
    }

    public Map<String, ResourceData> getOverlayResources() {
        return Collections.unmodifiableMap(_overlayResources);
    }
    
    public void setOverlayResource(String path, ResourceData resourceData) {
        _overlayResources.put(path, resourceData);
        _updated = true;
    }
    
    public void removeOverlayResource(String path) {
        _overlayResources.remove(path);
        _updated = true;
    }

    public String getBasepluginVersion() {
        return _basepluginVersion;
    }

    public void setBasepluginVersion(String basepluginVersion) {
        _basepluginVersion = basepluginVersion;
        _updated = true;
    }

    public boolean isUpdated() {
        return _updated;
    }

    public void setUpdated(boolean updated) {
        _updated = updated;
    }

    public String getInitialBasepluginVersion() {
        return _initialBasepluginVersion;
    }

    public void setInitialBasepluginVersion(String initialBasepluginVersion) {
        _initialBasepluginVersion = initialBasepluginVersion;
    }



}
