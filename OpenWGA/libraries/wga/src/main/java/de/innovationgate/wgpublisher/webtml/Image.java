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

import java.awt.Dimension;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.DynamicAttributes;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.webtml.utils.ImageLink;
import de.innovationgate.wgpublisher.webtml.utils.ImageLinkReader;
import de.innovationgate.wgpublisher.webtml.utils.SrcSetCreator;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;

public class Image extends Base implements DynamicAttributes {
	
    private static final long serialVersionUID = 1L;

    public static final String OPTION_EDITOR_FIELD = SYSTEMOPTION_PREFIX + "editorField";
	
	private String _doc;
	private String _file;
	private String _item;
	private String _label;
	private String _db;
	private String _cssclass;
	private String _cssstyle;
	private String _maxwidth;
	private String _maxheight;
	private String _absolute;
	private String _derivate;


	public String getDerivate() {
        return getTagAttributeValue("derivate", _derivate, (String) getTMLContext().option(OPTION_IMAGE_DERIVATES));
    }

    public void setDerivate(String derivate) {
        _derivate = derivate;
    }

    /**
	 * Gets the name
	 * @return Returns a String
	 * @deprecated use getDoc() instead
	 */
    @Deprecated
	public String getName() {
		return getDoc();
	}
	
	public String getDoc() {
		return this.getTagAttributeValue("doc", _doc, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 * @deprecated use setDoc() instead
	 */
	@Deprecated
	public void setName(String name) {
		setDoc(name);
	}
	
	public void setDoc(String name) {
		_doc = name;
	}

	/**
	 * Gets the src
	 * @return Returns a String
	 * @deprecated use setFile() instead
	 */
	public String getSrc() {
		return getFile();
	}
	
	public String getFile() {
		return this.getTagAttributeValue("file", _file, null);
	}
	/**
	 * Sets the file to be rendered
	 * @param src The src to set
	 * @deprecated use setFile() instead
	 */
	@Deprecated
	public void setSrc(String src) {
		setFile(src);
	}
	
	public void setFile(String file) {
		_file = file;
	}

	public void tmlEndTag() throws WGAPIException, TMLException {
		
		try {
            // Retrieve info from doc/file attributes or from item attribute
            String doc = null;
            String file = null;
            String db = null;
            String cssClass = null;
            String cssStyle = null;
            
            String titleAttribute = null;
            String altAttribute = null;
            String srcSetAttribute ="";
            String imgAlign = "";
            StringBuffer borderAttributesHTML = new StringBuffer("");
            
            TMLContext urlRetrievalContext = getTMLContext();
            
            String item = this.getItem();
            WGA wga = WGA.get(getTMLContext());
            if (item != null) {
            	@SuppressWarnings("unchecked")
                List<String> itemValues = WGUtils.toString(this.getTMLContext().itemlist(item));
            	ImageLinkReader iliReader = wga.service(ImageLinkReader.class);
            	ImageLink ili = iliReader.read(itemValues);
            	
            	// For image items we must always use a design context from base, so fileurl() does not try to resolve a file container name with overlay addressation
            	Design baseDesignContext = wga.design().resolve("@base");
                urlRetrievalContext = urlRetrievalContext.designContext(baseDesignContext.getDesignContext()); 
            	        
            	db = ili.getDb();
            	doc = ili.getDoc();
            	file = ili.getFile();
            	cssClass = ili.getCssClass();
            	if (cssClass == null) {
            	    cssClass = getCssclass();
            	}
            	cssStyle = ili.getCssStyle();
            	if (cssStyle == null) {
            	    cssStyle =getCssstyle();
            	}
            	
            	titleAttribute = ili.getTitle();
            	altAttribute = ili.getAlt();
            	if(altAttribute==null || altAttribute.isEmpty())
            		altAttribute = file;

                if( !WGUtils.isEmpty(ili.getBorder())){
                    borderAttributesHTML.append(" border=\"");
                    borderAttributesHTML.append(ili.getBorder());
                    borderAttributesHTML.append("\" ");
                }
                if( !WGUtils.isEmpty(ili.getBorderColor())){
                    borderAttributesHTML.append(" style=\"border-color:");
                    borderAttributesHTML.append(ili.getBorderColor());
                    borderAttributesHTML.append("\" ");
                }
                
                if(ili.getAlign() != null){
                    imgAlign = " align=\"" + ili.getAlign() + "\" ";
                }
                
            }
            else {
            	doc = this.getDoc();
            	file = this.getFile();
            	db = this.getDb();
            	cssClass = this.getCssclass();
            	cssStyle = this.getCssstyle();
            }
                        
            // Cleanup file and doc
            if (file != null && file.equals("")) {
            	file = null;
            }
            if (doc != null && doc.equals("")) {
            	doc = null;
            }
            
            if(file==null && item==null) {
            	if(doc!=null){
            		this.addWarning("No file attribute given on <tml:image doc=\"" + doc + "\">", false);
            		return;
            	}
            	// use primary file or first file in content
            	file = getTMLContext().getcontent().getPrimaryFileName();
            	if(file==null){
            		List<String> filenames = getTMLContext().getcontent().getFileNames();
            		if(filenames!=null && filenames.size()>0)
            			file = filenames.get(0);
            		else {
            			//this.addWarning("<tml:image/>: No files found on content", false);
            			return;
            		}
            	}
            }

            String titleAttributeString="";
            if(titleAttribute!=null && !titleAttribute.equals("")) {
            	titleAttributeString=" title=\"" + titleAttribute + "\" ";
            }
            String altAttributeString="";
            if(altAttribute!=null && !altAttribute.equals("")) {
            	altAttributeString=" alt=\"" + altAttribute + "\"";
            }
            else {
            	String html_alt=getDynamicHtmlAttribute("alt");
            	if(html_alt==null) {
            		int i = file.indexOf("."); 
            		altAttributeString = " alt=\"" + (i>0 ? file.substring(0, i) : file) + "\"";
            	}
            }

            // Determine CSS
            StringBuffer css = new StringBuffer();
            if (cssClass != null) {
                css.append("class=\"" + cssClass + "\" ");
            }
            if (cssStyle != null) {
                css.append("style=\"" + cssStyle + "\" ");
            } 
            
            // Build image html
            String imageHTML = null;
            if (file != null) { // May be empty if an image item is not yet available
            	
            	String url = urlRetrievalContext.fileImageURL(db, doc, file);
            	
                URLBuilder fileurl = wga.urlBuilder(url);
                boolean doSrcSet = false;
                
                // Append derivate information to URL
                String derivate = getDerivate();
                DerivateQuery derivateQuery = null;
                if (derivate != null && doc==null) {
                    derivateQuery = getTMLContext().enhanceFileDerivateQuery(derivate);
                    if (!derivateQuery.isNoDerivate()) {
                        fileurl.setParameter(WGPDispatcher.URLPARAM_DERIVATE, derivateQuery.toString());
                        doSrcSet = true;
                    }
                }
                
                // Append scaling information to URL
                String maxWidth = getMaxwidth();
                if (maxWidth != null) {
                    fileurl.setParameter(WGPDispatcher.URLPARAM_MAXWIDTH, (new Integer(WGUtils.parseInt(maxWidth))).toString());
                    doSrcSet = true;
                }
                String maxHeight = getMaxheight();
                if (maxHeight != null) {
                    fileurl.setParameter(WGPDispatcher.URLPARAM_MAXHEIGHT, (new Integer(WGUtils.parseInt(maxHeight))).toString());
                    doSrcSet = true;
                }
                
                // Append srcset attribute to URL
                if (doSrcSet && ((Boolean) wga.database().getPublisherOption(WGACore.DBATTRIB_USE_NONFINAL_HT_FEATURES)) == true) {
                    WGContent content = urlRetrievalContext.content();
                    if (doc != null) {
                        content = WGPDispatcher.getContentByAnyKey(doc, (db != null ? urlRetrievalContext.db(db) : urlRetrievalContext.db()), getTMLContext().getrequest());
                    }
                    if (content != null && derivateQuery!=null && !derivateQuery.isNoDerivate()) {
                    	
                    	String srcset = urlRetrievalContext.createSrcSet(file, derivateQuery.toString());
                    	if(!srcset.isEmpty())
                    		srcSetAttribute = "srcset=\"" + srcset + "\" ";

                    }
                }
                
                String fileurlStr = (stringToBoolean(getAbsolute()) ? fileurl.build(true) : fileurl.buildLikeGiven());
                imageHTML = "<img" + buildDynamicHtmlAttributes() + imgAlign + borderAttributesHTML.toString() + altAttributeString + titleAttributeString + " src=\"" + fileurlStr + "\" " + srcSetAttribute + css.toString() + this.getResultString(false) + ">";
            }
            
            // If in edit mode, show editing link
            Object attribEdit = this.getPageContext().getRequest().getAttribute( WGACore.ATTRIB_EDITDOCUMENT );
            if( attribEdit != null && attribEdit.equals(this.getTMLContext().getcontent().getContentKey().toString()) && item != null){
                String theLabel = getLabel();
                StringBuffer prefix = createItemEditorDeclaration(item, "image", theLabel);
                prefix.append("<div class=\"WGA-Item-Value\" id=\"item_"+ item +"\">");
                setPrefix(prefix.toString());
                setSuffix("</div>\n</div>");
                
                if (imageHTML == null) {
                    imageHTML = "";
                }
                
            }
            
            this.setResult(imageHTML);
            
        }
        catch (Exception e) {
           throw new TMLException("Exception rendering image tag", e, true); 
        }


	}
        
    

	/**
	 * Returns the item.
	 * @return String
	 */
	public String getItem() {
		return this.getTagAttributeValue("item", _item, null);
	}

	/**
	 * Sets the item.
	 * @param item The item to set
	 */
	public void setItem(String item) {
		this._item = item;
	}
	

	/**
	 * Returns the title.
	 * @return String
	 */
	public String getLabel() {
		return this.getTagAttributeValue("label", _label, "Insert image here");
	}

	/**
	 * Sets the title.
	 * @param title The title to set
	 */
	public void setLabel(String title) {
		this._label = title;
	}

    /**
     * @return Returns the db.
     */
    public String getDb() {
        return getTagAttributeValue("db", _db, null);
    }
    /**
     * @param db The db to set.
     */
    public void setDb(String db) {
        this._db = db;
    }

    public String getCssclass() {
        return getTagAttributeValue("cssclass", _cssclass, null);
    }

    public void setCssclass(String cssclass) {
        this._cssclass = cssclass;
    }

    public String getCssstyle() {
        return getTagAttributeValue("cssstyle", _cssstyle, null);
    }

    public void setCssstyle(String cssstyle) {
        this._cssstyle = cssstyle;
    }

    public String getMaxwidth() {
        return getTagAttributeValue("maxwidth", _maxwidth, null);
    }

    public void setMaxwidth(String maxwidth) {
        this._maxwidth = maxwidth;
    }

    public String getMaxheight() {
        return getTagAttributeValue("maxheight", _maxheight, null);
    }

    public void setMaxheight(String maxheight) {
        this._maxheight = maxheight;
    }

    public String getAbsolute() {
        return getTagAttributeValue("absolute", _absolute, "false");
    }

    public void setAbsolute(String absolute) {
        _absolute = absolute;
    }
        
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "html"); 
    }
}


