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

import java.util.Iterator;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class CreatePage extends Base {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _area;
    private String _contenttype;
    private String _message;
    private String _container;
    private String _cssclass;
    private String _cssstyle;
    
    public class Status extends BaseTagStatus {
        private String _area;
        private String _structKey;
        private String _contentType;
        private String _message = null;
        
        public String toHTML() {
            
            String msg = _message;
            if (msg == null) {
                msg = getTMLContext().systemLabel("authoring", "createpage.message");
            }
            
            StringBuffer html = new StringBuffer();
            html.append("<span class=\"BI-create\" style=\"display:none\">");
            html.append("<span style=\"display:none\">");
            html.append("{");
            if (_area != null) {
                html.append("area:'" + _area + "'");
            } else if (_structKey != null) {
                html.append("structkey:'" + _structKey + "'");
            } 
            if (_contentType != null) {
                html.append(", contenttype:'" + _contenttype + "'");
            }
            html.append("}");
            html.append("</span>");
            html.append("<span>");
            html.append(_message);
            html.append("</span>");
            html.append("</span>");
            return html.toString();
        }
        
        
        @Override
        public void initAttributeDelegates(Base tag) {
            super.initAttributeDelegates(tag);
            CreatePage createPageTag = (CreatePage) tag;
            _area = createPageTag.getArea();
            _contentType = createPageTag.getContenttype();
            _message = createPageTag.getMessage();
        }


        public String getMessage() {
            return _message;
        }


        public void setMessage(String message) {
            _message = message;
        }
    }
    
    @Override
    protected BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    @Override
    public void tmlStartTag() throws TMLException, WGAPIException {
        
        Status status = (Status) getStatus();
        
        // Disable tag evaluation. If any test fails an early exit will keep it like that.
        setCancelTag(true);


        // check if BI is enabled
        if (!WGPDispatcher.isBrowserInterface(getPageContext().getSession())) {
            return;
        }
        
        // check db access - minimum author
        if (!(getTMLContext().db().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR)) {
            return;
        }
        
        // Check if we are allowed to edit any language
        Iterator langs = getTMLContext().db().getLanguages().values().iterator();
        boolean langFound = false;
        while (langs.hasNext()) {
            WGLanguage lang = (WGLanguage) langs.next();
            if (lang.mayCreateContent()) {
                langFound = true;
                break;
            }
        }
        if (langFound == false) {
            return;
        }
        
        // Find parent document: Either use specified area or current context
        Status result = null;
        if (getArea() != null) {
            WGArea area = getTMLContext().db().getArea(getArea());
            if (area == null || !area.mayEditPages() || !contentTypeAllowed(area)) {
                return;
            }
        }
        else if (getTMLContext().getcontent() != null && !getTMLContext().getcontent().isDummy() && getTMLContext().getcontent().hasCompleteRelationships() ) {
            WGStructEntry structEntry = getTMLContext().getcontent().getStructEntry();
            if (structEntry != null && structEntry.mayEditChildPages() && contentTypeAllowed(structEntry)) {
                status._structKey = String.valueOf(structEntry.getStructKey());
            }
            else {
                return;
            }
        }
        else {
            addWarning("Cannot build creation link for context '" + getTMLContext().getpath() + "' as this is no valid parent document");
            return;
        }
        
        // All tests passed. Enable tag evaluation
        setCancelTag(false);
        
        // Put out a container tag if we're told to
        String container = getContainer();
        if (container != null) {
            String cssClasses = "createpage";
            String addCssClass = getCssclass();
            if (addCssClass != null) {
                cssClasses += " " + addCssClass;
            }
            
            String cssStyle = getCssstyle();
            if (!WGUtils.isEmpty(cssStyle)) {
                cssStyle = " style=\"" + cssStyle + "\"";
            }
            
            setPrefix("<" + container + " class=\"" + cssClasses + "\"" + cssStyle + ">");
            setSuffix("</" + container + ">");
        }
        
    }
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
     */
    public void tmlEndTag() throws TMLException, WGAPIException {
        
        Status status = (Status) getStatus();
        
        // Put out button directly if <tml:createpage> has no content. Otherwise there should have been a <tml:createbutton> in the content doing this.
        String tagContent = getResultString();
        if (WGUtils.isEmpty(tagContent)) {
            setResult(status.toHTML());
        }
        
    }
    
    public String getMessage() {
        return getTagAttributeValue("message", _message, null);
    }


    public void setMessage(String message) {
        _message = message;
    }


    private boolean contentTypeAllowed(WGDocument doc) throws WGAPIException {

        // With specified content type
        if (getContenttype() != null) {
            WGContentType contentType = getTMLContext().db().getContentType(getContenttype());
            if (contentType != null) {
                return contentType.mayCreateContent() && contentType.mayCreateChildEntry(doc);
            } 
            else {
                addWarning("Contenttype '" + getContenttype() + "' could not be found in database '" + getTMLContext().db().getDbReference() + "'.");
                return false;
            }
        }

        // no content type specified. Look if any content type is usable at this position
        else {
            Iterator contentTypes = getTMLContext().db().getContentTypes().iterator();
            boolean ctFound = false;
            while (contentTypes.hasNext()) {
                WGContentType ct = (WGContentType) contentTypes.next();
                if (ct.mayCreateContent() && ct.mayCreateChildEntry(doc)) {
                    ctFound = true;
                    break;
                }
            }
            return ctFound;
        }
    }
    
    public String getArea() {
        return getTagAttributeValue("area", _area, null);
    }
    public void setArea(String area) {
        _area = area;
    }
    
    public String getContenttype() {
        return getTagAttributeValue("contentType", _contenttype, null);
    }
    public void setContenttype(String contenttype) {
        _contenttype = contenttype;
    }

    public String getContainer() {
        return getTagAttributeValue("container", _container, null);
    }

    public void setContainer(String container) {
        _container = container;
    }

    public String getCssclass() {
        return getTagAttributeValue("cssclass", _cssclass, null);
    }

    public void setCssclass(String cssclass) {
        _cssclass = cssclass;
    }

    public String getCssstyle() {
        return getTagAttributeValue("cssstyle", _cssstyle, "");
    }

    public void setCssstyle(String cssstyle) {
        _cssstyle = cssstyle;
    }

}
