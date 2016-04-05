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

import java.io.Serializable;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wgpublisher.DeployedLayout;
import de.innovationgate.wgpublisher.DeployerException;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

public class Inline extends Option {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class Pointer implements Serializable {
        
        private static final long serialVersionUID = 1L;
        private String _layoutKey;
        private String _description;
        private String _designDB;
        private String _portletKey;
        private Object _moduleController;
        
        public Pointer(String description, String layoutKey, String designDB, String portletKey, Object renderer) {
            _description = description;
            _layoutKey = layoutKey;
            _designDB = designDB;
            _portletKey = portletKey;
            _moduleController = renderer;
        }

        public String getLayoutKey() {
            return _layoutKey;
        }

        public String getDescription() {
            return _description;
        }
        
        public String getDesignDB() {
            return _designDB;
        }

        protected String getPortletKey() {
            return _portletKey;
        }

        protected Object getModuleController() {
            return _moduleController;
        }
        
        
    }
    

    
    protected Object retrieveOptionValue(Object tagResult) throws WGException {
        
        
        Object optionValue = super.retrieveOptionValue(tagResult);
        if (optionValue == null) {
            throw new TMLException("Cannot create inline with value of null", true); 
        }
        
        WGTMLModule mod = getTMLContext().designdb().getTMLModule(getStatus().getTMLModuleName(), getStatus().getTMLModuleMediaKey());
        DeployedLayout layout = getCore().getDeployer().deployInlineTML(mod, "option \"" + this.getName() + "\" on line " + getSourceline(), String.valueOf(optionValue), Integer.parseInt(getSourceline()), getPageContext().getRequest());
        return new Pointer("WebTML Option " + getName(), layout.getLayoutKey(), getTMLContext().getDesignDBKey(), getTMLContext().getportlet().getportletkey(), getTMLContext().option(Base.OPTION_MODULE_CONTROLLER));
        
    }

    @SuppressWarnings("unused")
    private String getRawTagContent(WGTMLModule mod) throws WGException {
        
        //TODO: Not functional yet. Tried to extract the tag content from the module code, se we could set back bodycontent of this tag to JSP, therefor avoid cascading issues.
        // However that would need to implement some JSP parsing functionality which is beyond trivial if it should work in all situations. Refraining from this for now. 
        
        String code = mod.getCode();
        int uidPos = code.indexOf("uid=\"" + getUid() + "\"");
        if (uidPos == -1) {
            throw new TMLException("Could not localize inline tag content: UID of inline tag not found", true);
        }
        
        int cascadeLevel = 0;
        int endPos = -1;
        
        // Find end of start tag.
        int startPos = -1;
        
        // Inside code: Find the correct tml:inline end tag
        while (true) {
            
            uidPos = code.indexOf("tml:inline", uidPos);
            if (uidPos == -1) {
                break;
            }
            
            // A cascaded tml:inline: Go up one cascadeLevel
            if (uidPos > 0 && code.substring(uidPos - 1, uidPos).equals("<")) {
                cascadeLevel++;
            }
            
            // tml:inline close tag: Go down cascadeLevel or end search
            else if (uidPos > 1 && code.substring(uidPos-2, uidPos).equals("</")) {
                if (cascadeLevel > 0) {
                    cascadeLevel--;
                }
                else {
                    endPos = uidPos-2;
                    break;
                }
            }
            
            uidPos++;
            
        }
        
        
        
        if (endPos == -1) {
            throw new TMLException("Could not localize inline tag content: Unable to find end tag of inline tag", true);
        }
        return code.substring(startPos, endPos);
                
        
    }
    
    @Override
    public String getScope() {
        return getTagAttributeValue("scope", _scope, TMLOption.SCOPE_LOCAL);
    }

}
