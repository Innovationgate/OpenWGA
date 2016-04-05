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

package de.innovationgate.wgpublisher.vlink;

import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget.Type;

public class NameVirtualLinkResolver implements VirtualLinkResolver {

    @Override
    public VirtualLinkTarget resolve(WGA wga, WGContent content) throws WGException {

        List<String> elements = WGUtils.deserializeCollection(content.getVirtualLink(), "/");
        String name = (String) elements.get(elements.size() - 1);
        
        Context cx = wga.isTMLContextAvailable() ? wga.tmlcontext() : wga.createTMLContext(content);
        Context targetCx = cx.context("name:" + name, false);
        if (targetCx != null) {
            VirtualLinkTarget target = new VirtualLinkTarget(Type.CONTENT);
            target.setContainerKey((String) targetCx.meta("KEY"));
            return target;
        }
        else {
            return null;
        }
        
        
    }

}
