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

import java.util.Date;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentCoreWrapper;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.config.DesignReference;

public class DesignProviderCoreWrapper extends WGDocumentCoreWrapper {

    private WGADesignProvider _provider;
    private boolean _variant;
    private boolean _overlay;
    public DesignProviderCoreWrapper(WGDocumentCore core, WGADesignProvider prov, boolean variant, boolean overlay) {
        super(core);
        _provider = prov;
        _variant = variant;
        _overlay = overlay;
    }
    
    public Object getMetaData(String type) throws WGAPIException {

        // Return the variant meta
        if (WGDesignDocument.META_VARIANT.equals(type)) {
            return _variant;
        }
        
        Object meta = super.getMetaData(type);

        // If design variant, we need to cutoff the variant suffix from the name
        if (WGDesignDocument.META_NAME.equals(type)) {
            String name = (String) meta;
            String suffix = "." + _provider.getConsumerDatabase().getDbReference();
            if (_variant && name.endsWith(suffix)) {
                meta = cutoffVariantSuffix(name, suffix);
            }
            if (_overlay) {
                meta = OverlayDesignProvider.OVERLAY_PREFIX + name;
            }
        }
        
        if (_overlay && WGDesignDocument.META_DESIGNREFERENCE.equals(type) && meta != null) {
            DesignReference baseRef = (DesignReference) meta;
            de.innovationgate.webgate.api.WGDocumentKey key = new de.innovationgate.webgate.api.WGDocumentKey(baseRef.getDocumentKey());
            key = new de.innovationgate.webgate.api.WGDocumentKey(key.getDocType(), OverlayDesignProvider.OVERLAY_PREFIX + key.getName(), key.getMediakey());
            meta = new DesignReference(baseRef, key.toString());
        }
        
        return meta;
    }

    public WGDatabaseRevision remove() throws WGAPIException {
        throw new WGNotSupportedException("Not supported");
    }

    public WGDatabaseRevision save(Date lastModified) throws WGAPIException {
        throw new WGNotSupportedException("Not supported");
    }

    public static String cutoffVariantSuffix(String name, String suffix) {
        return name.substring(0, name.length() - suffix.length());
    }

    public boolean isOverlay() {
        return _overlay;
    }

    public boolean isVariant() {
        return _variant;
    }

    public WGADesignProvider getProvider() {
        return _provider;
    }

}
