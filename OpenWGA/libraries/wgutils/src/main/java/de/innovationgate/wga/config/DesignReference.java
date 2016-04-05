/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.wga.config;

import java.net.URI;
import java.net.URISyntaxException;

import de.innovationgate.wga.common.ImmutableObject;

/**
 * A reference identifying the source of a design document
 * The reference consists of three parts:
 * - A source name, which identifies the type of a source the design was retrieved from
 * - A design name, which identifies an explicit design of the given source
 * - A design path (optionally), which identifies a special design object inside the design
 */
public class DesignReference implements ImmutableObject {
    
    public static final String DESIGN_URI_SCHEME = "wgadesign";

    
    public DesignReference(DesignReference parentReference, String designPath) {
        super();
        _sourceName = parentReference.getSourceName();
        _designName = parentReference.getDesignName();
        _documentKey = designPath;
    }
    
    public DesignReference(String sourceName, String designName, String documentKey) {
        super();
        _sourceName = sourceName;
        _designName = designName;
        _documentKey = documentKey;
    }
    
    public DesignReference(String sourceName, String designName) {
        this(sourceName, designName, null);
    }
    
    public DesignReference(Design designConfig) {
        this(designConfig.getSource(), designConfig.getName());
    }

    private String _sourceName;
    private String _designName;
    private String _documentKey = null;

    public DesignReference(String ref) throws URISyntaxException {
        
        URI uri = new URI(ref);
        if (!DESIGN_URI_SCHEME.equals(uri.getScheme())) {
            throw new IllegalArgumentException("The uri scheme '" + uri.getScheme() + "' cannot be used to construct design references");
        }
        
        _sourceName = uri.getAuthority();
        
        String path = uri.getPath();
        if (path != null && path.startsWith("/")) {
            _designName = path.substring(1);
        }
        if (uri.getQuery() != null) {
            _documentKey = uri.getQuery();
        }
        
    }
    
    public String getSourceName() {
        return _sourceName;
    }

    public String getDesignName() {
        return _designName;
    }

    public String getDocumentKey() {
        return _documentKey;
    }

    @Override
    public String toString() {
        URI uri;
        try {
            uri = new URI(DESIGN_URI_SCHEME, _sourceName, "/" + _designName, (_documentKey != null ? _documentKey : null), null);
            return uri.toString();
        }
        catch (URISyntaxException e) {
            throw new IllegalStateException("Cannot form URI from design reference data");
        }
        
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_designName == null) ? 0 : _designName.hashCode());
        result = prime * result + ((_documentKey == null) ? 0 : _documentKey.hashCode());
        result = prime * result + ((_sourceName == null) ? 0 : _sourceName.hashCode());
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
        DesignReference other = (DesignReference) obj;
        if (_designName == null) {
            if (other._designName != null)
                return false;
        }
        else if (!_designName.equals(other._designName))
            return false;
        if (_documentKey == null) {
            if (other._documentKey != null)
                return false;
        }
        else if (!_documentKey.equals(other._documentKey))
            return false;
        if (_sourceName == null) {
            if (other._sourceName != null)
                return false;
        }
        else if (!_sourceName.equals(other._sourceName))
            return false;
        return true;
    }
    
    public DesignReference getDesignProviderReference() {
        if (_documentKey != null) {
            return new DesignReference(_sourceName, _designName, null);
        }
        else {
            return this;
        }
    }

}
