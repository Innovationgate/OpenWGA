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

package de.innovationgate.webgate.api;

import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * Documents representing real design resources
 */
public abstract class WGDesignResourceDocument extends WGDesignDocument {
    
    public static final String META_SOURCEFILENAME = "SOURCEFILENAME";
    public static final MetaInfo METAINFO_SOURCEFILENAME = new MetaInfo(META_SOURCEFILENAME, String.class, null);
    static {
        METAINFO_SOURCEFILENAME.setExtdata(true);
        METAINFO_SOURCEFILENAME.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_NOINDEX);
    }

    public WGDesignResourceDocument(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(db, doc, flags);
    }
    
    @Override
    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException {
        super.performSaveCheck();
        if (db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("You are not allowed to edit this design document", WGAuthorisationException.ERRORCODE_OP_NEEDS_DESIGNER_RIGHTS);
        }
    }
    
    @Override
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException {
        super.performRemoveCheck(deepCheck, deletionRoot);
        if (db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("You are not allowed to remove this design document", WGAuthorisationException.ERRORCODE_OP_NEEDS_DESIGNER_RIGHTS);
        }
    }
    
    /**
     * Returns the exact-cased name of the file from which the source of this resource was read
     * @throws WGAPIException
     */
    public String getSourceFileName() throws WGAPIException {
        return (String) getMetaData(META_SOURCEFILENAME);
    }


}
