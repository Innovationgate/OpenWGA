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

import org.apache.commons.vfs2.FileSystemException;

import de.innovationgate.webgate.api.WGException;

public class WGADesignRetrievalException extends WGException {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _design;

    public String getDesign() {
        return _design;
    }

    public WGADesignRetrievalException(String msg) {
        super(msg);
    }
    
    public WGADesignRetrievalException(String msg, String designName) {
        super(msg);
        _design = designName;
    }

    public WGADesignRetrievalException(String string, FileSystemException e) {
        super(string, e);
    }

}
