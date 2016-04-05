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
import de.innovationgate.webgate.api.jdbc.Content;
import de.innovationgate.webgate.api.jdbc.ContentFileContent;
import de.innovationgate.webgate.api.jdbc.ContentFileContentPart;
import de.innovationgate.webgate.api.jdbc.ContentFileMeta;
import de.innovationgate.webgate.api.jdbc.ContentFilePart;
import de.innovationgate.webgate.api.jdbc.Entity;
import de.innovationgate.webgate.api.jdbc.EntityContainingFiles;

public class CS5P4ContentFileDescriptor implements CS41FileEntityDescriptor {

    @Override
    public ContentFileMeta createNewFile(String name, Date date, EntityContainingFiles parentEntity) {
        ContentFileMeta file = new ContentFileMeta();
        file.setParentcontent((Content) parentEntity);
        file.setName(name);
        file.setCreated(date);
        return file;
    }
        
    @Override
    public String getHQLFileMetaEntity() {
        return "ContentFileMeta";
    }
    
    @Override
    public String getHQLFileDataEntity(Entity parentEntity) {
        if (parentEntity instanceof ContentFileMeta) {
            return ContentFilePart.class.getSimpleName(); // Backward compatility
        }
        else if (parentEntity instanceof ContentFileContent) {
            return ContentFileContentPart.class.getSimpleName();
        }
        else {
            throw new IllegalArgumentException("Illegal parent entity for data entity: " + parentEntity);
        }
    }
    
    @Override
    public String getHQLFileMetaParentProperty() {
        return "parentcontent";
    }

    @Override
    public AttachmentFilePart createFilePart(Entity parentEntity) {
        if (parentEntity instanceof ContentFileMeta) {
            ContentFilePart part = new ContentFilePart();
            part.setParentEntity(parentEntity);
            return part;
        }
        else if (parentEntity instanceof ContentFileContent) {
            ContentFileContentPart part = new ContentFileContentPart();
            part.setParentEntity(parentEntity);
            return part;
        }
        else {
            throw new IllegalArgumentException("Illegal parent entity for data entity: " + parentEntity);
        }
    }
    
    

}
