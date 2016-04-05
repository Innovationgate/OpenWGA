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

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDesignDocument;

public class DesignResourceReference implements Serializable {
    
    public static class GsonTypeAdapter extends TypeAdapter<DesignResourceReference> {

        @Override
        public DesignResourceReference read(JsonReader in) throws IOException {
            return new DesignResourceReference(in.nextString());
        }

        @Override
        public void write(JsonWriter out, DesignResourceReference ref) throws IOException {
            out.value(ref.normalize().toString());
        }
        
    }
   
    private boolean _explicitDesignAppChoice = false;
    private String _designApp;
    
    public String getDesignApp() {
        return _designApp;
    }
    
    public String getResourceName() {
        return _resourceName;
    }
    
    public String getResourceLocalName() {
        String res = normalize().getResourceName();
        int lastColonPos = res.lastIndexOf(":");
        if (lastColonPos != -1) {
            return res.substring(lastColonPos+1);
        }
        else {
            return res;
        }
    }
    private String _resourceName;
    
    public DesignResourceReference(WGDesignDocument doc) throws WGAPIException {
        _designApp = doc.getDatabase().getDbReference();
        _resourceName = doc.getName();
    }
    
    public DesignResourceReference(String refString) {
        int slashPos = refString.indexOf("/");
        if (slashPos == -1) {
            _designApp = null;
            _resourceName = refString;
        }
        else {
            _designApp = refString.substring(0, slashPos);
            _resourceName = refString.substring(slashPos + 1);
        }
    }
    
    public DesignResourceReference(String designApp, String name) {
        _designApp = designApp;
        _resourceName = name;
        if (_resourceName == null) {
            _resourceName = "";
        }
    }

    @Override
    public String toString() {
        if (_designApp != null) {
            return _designApp + "/" + _resourceName;
        }
        else {
            return _resourceName;
        }
    }

    public void setDesignApp(String designApp) {
        _designApp = designApp;
    }

    public void setResourceName(String resourceName) {
        _resourceName = resourceName;
    }

    public boolean isExplicitDesignAppChoice() {
        return _explicitDesignAppChoice;
    }

    public void setExplicitDesignAppChoice(boolean explicitDesignAppChoice) {
        _explicitDesignAppChoice = explicitDesignAppChoice;
    }
    
    /**
     * Returns of form of the reference where the real target app is contained in {@link #getDesignApp()} and {@link #getResourceName()} does only return the
     * path of the resource.
     * Effectively this just removes the optional database key in the resource name and uses it as design app.
     */
    public DesignResourceReference normalize() {
        
        String refString = getResourceName();
        String designApp = getDesignApp();
        boolean explicitDesignAppChoice = isExplicitDesignAppChoice();
               
        
        int slashIdx = refString.indexOf("/");
        if (slashIdx != -1) {
            designApp = refString.substring(0, slashIdx);
            refString = refString.substring(slashIdx + 1);
            explicitDesignAppChoice = true;
        }
        
        DesignResourceReference normalized = new DesignResourceReference(designApp, refString);
        normalized.setExplicitDesignAppChoice(explicitDesignAppChoice);
        return normalized;
        
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_designApp == null) ? 0 : _designApp.hashCode());
        result = prime * result + ((_resourceName == null) ? 0 : _resourceName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DesignResourceReference other = (DesignResourceReference) obj;
        if (_designApp == null) {
            if (other._designApp != null)
                return false;
        }
        else if (!_designApp.equals(other._designApp))
            return false;
        if (_resourceName == null) {
            if (other._resourceName != null)
                return false;
        }
        else if (!_resourceName.equals(other._resourceName))
            return false;
        return true;
    }
    
    /**
     * Returns the part of the overlay name that adresses the overlay level of the resource. So for a resource "overlay:my_module" returns "overlay".
     */
    public String getResourceOverlayReference() {
        
        
        List<String> outParts = new ArrayList<String>();
        for (String part :WGUtils.deserializeCollection(normalize().getResourceName(), ":")) {
            if (part.equals("overlay")) {
                outParts.add(part);
            }
            else {
                break;
            }
        }
        
        return WGUtils.serializeCollection(outParts, ":");
        
        
    }
    
    

}
