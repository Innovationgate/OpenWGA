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

package de.innovationgate.webgate.api.jdbc.filehandling;

import java.util.Date;

import de.innovationgate.webgate.api.jdbc.AttachmentFilePart;
import de.innovationgate.webgate.api.jdbc.Entity;
import de.innovationgate.webgate.api.jdbc.EntityContainingFiles;

public interface CS41FileEntityDescriptor {
    
    public CS41FileAttachmentEntity createNewFile(String convertedFileName, Date now, EntityContainingFiles parentEntity);
    
    /**
     * Name of the entity holding actual file data (optimized file handling only)
     * @param parentEntity The parent entity of the file data object
     */
    public String getHQLFileDataEntity(Entity parentEntity);

    /**
     * Name of the entity holding file metadata (optimized file handling only)
     */
    public String getHQLFileMetaEntity();
    
    /**
     * Name of the property on the file metadata entity that addresses the parent entity
     */
    public String getHQLFileMetaParentProperty();
    
    /**
     * Create a file data entity
     * @param parentEntity The parent entity of the file data object
     * @return
     */
    public AttachmentFilePart createFilePart(Entity parentEntity);
    

}
