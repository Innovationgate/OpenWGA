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

package de.innovationgate.wgpublisher.design.conversion;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;

public class PostProcessResult implements Serializable {
    
    public static class IntegratedResource {
        
        private WGDocumentKey _key;
        private Date _lastModified;
        private String _dbKey;
        public String getDbKey() {
            return _dbKey;
        }
        public IntegratedResource(String dbKey, WGDocumentKey documentKeyObj, Date lastModified) {
            _dbKey = dbKey;
            _key = documentKeyObj;
            _lastModified = lastModified;
        }
        public WGDocumentKey getKey() {
            return _key;
        }
        public Date getLastModified() {
            return _lastModified;
        }
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbKey == null) ? 0 : _dbKey.hashCode());
            result = prime * result + ((_key == null) ? 0 : _key.hashCode());
            result = prime * result + ((_lastModified == null) ? 0 : _lastModified.hashCode());
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
            IntegratedResource other = (IntegratedResource) obj;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            if (_key == null) {
                if (other._key != null)
                    return false;
            }
            else if (!_key.equals(other._key))
                return false;
            if (_lastModified == null) {
                if (other._lastModified != null)
                    return false;
            }
            else if (!_lastModified.equals(other._lastModified))
                return false;
            return true;
        }
        
        
    }
    
    private String _code;
    private String _mimeType;
    private Set<IntegratedResource> _integratedResources = new HashSet<IntegratedResource>();

    public String getMimeType() {
        return _mimeType;
    }

    public void setMimeType(String mimeType) {
        _mimeType = mimeType;
    }

    public String getCode() {
        return _code;
    }

    public void setCode(String code) {
        _code = code;
    }

    public Set<IntegratedResource> getIntegratedResources() {
        return _integratedResources;
    }
    
    public void addIntegratedResource(WGDesignDocument doc) throws WGException {
        _integratedResources.add(new IntegratedResource(doc.getDatabase().getDbReference(), doc.getDocumentKeyObj(), doc.getLastModified()));
    }
    
}
