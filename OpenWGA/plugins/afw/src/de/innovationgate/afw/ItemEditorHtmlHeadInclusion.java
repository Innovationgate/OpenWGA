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

package de.innovationgate.afw;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.HTMLHeadInclusion;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class ItemEditorHtmlHeadInclusion implements HTMLHeadInclusion {

    public CharSequence processInclusion(TMLContext context) {

        try {
            // Determine, which (if any) document can be edited in this request
            if (context.isbrowserinterface()) {
                WGContent content = context.content();
                
                String dbkey = content.getDatabase().getDbReference();
                String prefLanguage = content.getLanguage().getName();
                context.getEnvironment().getPageContext().getSession().setAttribute("AFW."+dbkey+".PreferredLanguage", prefLanguage);
                
                List userNamesList = new ArrayList();
                userNamesList.add(content.getAuthor());

                if (content.getStatus().equals(WGContent.STATUS_DRAFT) && (!content.hasCompleteRelationships() || content.getStructEntry().mayEditEntryAndContent() == null)
                        && content.getLanguage().mayCreateContent() && content.getDatabase().isMemberOfUserList(userNamesList) && !content.hasItem("remote_info")) {

                    context.getEnvironment().getPageContext().getRequest().setAttribute(WGACore.ATTRIB_EDITDOCUMENT, content.getContentKey().toString());
                }
                
                if( content.hasCompleteRelationships() && content.getStructEntry().getArea().getName().equals("$trash"))
                	context.getEnvironment().getPageContext().getRequest().removeAttribute(WGACore.ATTRIB_EDITDOCUMENT);
                
				return "<link rel=\"stylesheet\" type=\"text/css\" href=\"" + 
							context.fileurl("plugin-wga-app-framework", "cms", "bi.css") +
							"\">";
				
                
            }
        }
        catch (WGException e) {
            context.getlog().error("Exception processing HTML head inclusion for Content Manager" ,e);
        }
        
        return null;
    }

}
