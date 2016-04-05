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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashSet;
import java.util.Set;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGFileMetaData.ManualDerivate;
import de.innovationgate.wga.server.api.WGA;

public class ManualDerivateCreator implements FileDerivateCreator {

    @Override
    public Set<DerivateInfo> getDerivateInfos(WGA wga, WGContent content, WGFileMetaData md) throws WGException {

        Set<DerivateInfo> derivates = new HashSet<DerivateInfo>();
        for (String name : md.getManualDerivateNames()) {
            ManualDerivate der = md.getManualDerivate(name);
            DerivateInfo derivateInfo = new DerivateInfo(der.getName(), der.getUsage());
            derivates.add(derivateInfo);
        }
        return derivates;
        
    }

    @Override
    public void createDerivate(WGA wga, WGContent content, WGFileMetaData md, DerivateInfo derivateInfo, OutputStream out) throws WGException {
        try {
            ManualDerivate der = md.getManualDerivate(derivateInfo.getName());
            BinaryFieldData data = der.getData();
            if (data != null) {
                InputStream in = data.getInputStream();
                WGUtils.inToOut(in, out, 4092);
                in.close();
                out.flush();
            }
        }
        catch (IOException e) {
            throw new WGBackendException("Exception creating manual derivate " + derivateInfo.getName() + " for file '" + md.getName() + "' on document '" + content.getDocumentKey() + "' (app " + content.getDatabase().getDbReference() + ")");
        }

    }

}
