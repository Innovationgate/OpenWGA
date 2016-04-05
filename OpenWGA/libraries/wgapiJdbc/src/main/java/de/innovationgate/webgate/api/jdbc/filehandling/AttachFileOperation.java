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

import java.util.Map;

import de.innovationgate.webgate.api.jdbc.LogEntry;

public class AttachFileOperation <FileEntity extends FileAttachmentEntity> {
    
    private FileEntity _entity;
    
    private boolean _updateData = true;
    
    private LogEntry _updateLog = null;
    
    public AttachFileOperation(FileEntity entity) {
        _entity = entity;
    }

    public FileEntity getEntity() {
        return _entity;
    }

    public void setEntity(FileEntity entity) {
        _entity = entity;
    }

    public boolean isUpdateData() {
        return _updateData;
    }

    public void setUpdateData(boolean updateData) {
        _updateData = updateData;
    }

    public LogEntry getUpdateLog() {
        return _updateLog;
    }

    public void setUpdateLog(LogEntry updateLog) {
        _updateLog = updateLog;
    }

}
