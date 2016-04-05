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

package de.innovationgate.wgpublisher.webtml;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.CreatePage.Status;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class CreateButton extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _message;
    
    @Override
    public void tmlEndTag() throws TMLException, WGAPIException {

        CreatePage.Status createPage = (CreatePage.Status) getStatus().getAncestorTag(CreatePage.class);
        if (createPage == null) {
            throw new TMLException("<tml:createbutton> is only valid inside <tml:createpage>", true);
        }
        
        String message = getMessage();
        if (message != null) {
            createPage.setMessage(message);
        }
        
        setResult(createPage.toHTML());
        
    }

    public String getMessage() {
        return getTagAttributeValue("message", _message, null);
    }

    public void setMessage(String message) {
        _message = message;
    }

}
