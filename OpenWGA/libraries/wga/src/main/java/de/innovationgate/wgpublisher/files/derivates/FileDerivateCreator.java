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

package de.innovationgate.wgpublisher.files.derivates;

import java.io.OutputStream;
import java.util.Set;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGAServerException;

/**
 * A class for creation of file derivates. A single {@link FileDerivateCreator} can create multiple derivates.
 */
public interface FileDerivateCreator {
    

    
    /**
     * Returns a list of names for the derivates that this creator can create for the given original file. The names can be freely chosen by the creator and are used by derivate management to check the need for updates of individual derivates.
     * If the creator does not want to create derivates for this file it can return null or an empty set.
     * @Param wga WGA context object
     * @Param db The database containing the original file
     * @Param content The content document containing the original file
     * @param md The metadata of the original file
     * @return A set of {@link DerivateInfo} objects describing derivates that can be created for the given file.
     * @throws WGException 
     */
    public Set<DerivateInfo> getDerivateInfos(WGA wga, WGContent content, WGFileMetaData md) throws WGException;
    
    /**
     * Should create the derivate of the given derivate info for the given attachment and write the data to the given output stream
     * @param wga WGA context object
     * @param content The content holding the original file
     * @param md The metadata of the original file attachment for which a derivate should be created
     * @param derivateInfo The derivate info of the derivate that should be created. This is an instance of this class that was previously returned from {@link #getDerivateInfos(WGA, WGContent, WGFileMetaData)}.
     * @param out The output stream where to write the derivate data to
     * @throws WGException
     */
    public void createDerivate(WGA wga, WGContent content, WGFileMetaData md, DerivateInfo derivateInfo, OutputStream out) throws WGException;

}
