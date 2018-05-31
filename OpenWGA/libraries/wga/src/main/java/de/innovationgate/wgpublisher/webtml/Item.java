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
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.tagext.DynamicAttributes;

import org.apache.commons.jxpath.JXPathContext;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Node;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.RTFEncodingFormatter;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.RootTagReceptor;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class Item extends FormBase implements DynamicAttributes {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String type;
	private String name;
	private String editor;
	private String label;
	private String scriptlets;
	private String xpath;
    private String aliases;
    private String aliasesitem;
    private String highlight;
    private String highlightprefix;
    private String highlightsuffix;
    private String saveaction;
	

	public static final String OPTION_EDITOR_FIELD = SYSTEMOPTION_PREFIX + "editorField";
    

	public void tmlEndTag() throws WGException, TMLException {
		
		String itemName = this.getName();
		TMLContext tmlContext = this.getTMLContext();
		List result = null;
		String type = this.getType();
        
        // add warning on illegal use of highlight attribute
        if (stringToBoolean(getHighlight())) {
            if (!type.equals("content")) {
                addWarning("Highlighting can only be used with type 'content' - skipped.");
            } 
        }
		
		// Retrieve value
		if (type.equals("content")) {
			result = tmlContext.itemlist(itemName, buildNamedActionParameters(false));
            if (stringToBoolean(getHighlight())) {
                if (this.stringToBoolean(this.getScriptlets())) {
                    addWarning("Highlighting cannot be used with scriptlets - skipped.");                    
                } else if (this.getAliases() != null) {
                	addWarning("Highlighting cannot be used with aliases - skipped.");    
                } else if (getXpath() != null) {
                    addWarning("Highlighting cannot be used together with xpath - skipped.");
                } else {
                    // highlight itemvalue with information from lucene query
                    result = Collections.singletonList(tmlContext.highlightitem(itemName, getHighlightprefix(), getHighlightsuffix(), getStatus().encode));
                    getStatus().encode = "none";
                }
            }
		}
		else if (type.equals("profile")) {
			TMLUserProfile profile = tmlContext.getprofile();
			if (profile == null) {
				this.addWarning("Current user has no profile", true);
				return;
			}
			result = profile.itemlist(itemName);
		}
		else if (type.equals("portlet")) {
			TMLPortlet portlet = tmlContext.getportlet();
			if (portlet == null) {
				this.addWarning("Current user has no portlet registration", true);
				return;
			}
			result = portlet.itemlist(itemName);
		}
        else if (type.equals("tmlform")) {
            TMLForm form = tmlContext.gettmlform();
            if (form == null) {
                addWarning("There is no current WebTML form at this position in the current request");
                return;
            }
            result = form.fieldlist(itemName);
        }
		
        // The item does not exist or is empty. Treat as empty list.
		if (result == null) {
			result = new ArrayList();
		}
		
		// Eventually execute xpath
		String xpath = getXpath();
		if (xpath != null && result.size() > 0) {
			Object firstResult = result.get(0);
			if (firstResult instanceof Node) {
				Node resultNode = (Node) firstResult;
				result = resultNode.selectNodes(xpath);
			}
			else if (firstResult instanceof String) {
				try {
					Document doc = DocumentHelper.parseText((String) firstResult);
					result = doc.selectNodes(xpath);
				}
				catch (DocumentException e) {
					addWarning("Unable to parse item content '" + itemName + "' as XML: " + firstResult);
				}
			}
            else if (ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(firstResult) == RhinoExpressionEngine.TYPE_XMLOBJECT) {
                result = ExpressionEngineFactory.getTMLScriptEngine().xpathTMLScriptBean(firstResult, xpath);
            }
			else {
				JXPathContext jxcontext = JXPathContext.newContext(firstResult);
				Iterator jxresults = jxcontext.iterate(xpath);
				result = new ArrayList();
				while (jxresults.hasNext()) {
					result.add(jxresults.next());
				}
			}
		}
		
		// Eventually resolve scriptlets
		if (result.size() > 0 && this.stringToBoolean(this.getScriptlets()) == true) {
			RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
            String resolvedResultStr = null;
            try {
            	Map params = new HashMap();
            	params.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_SCRIPTLETS);
                resolvedResultStr = engine.resolveScriptlets(result.get(0), getTMLContext(), params);
                result = new ArrayList();
                result.add(resolvedResultStr);
            }
            catch (Exception e) {
                throw new TMLException("Exception parsing scriptlets", e, false);
            } 
		}
		
		// BI-Editor (See Root-Class for attrib WGACore.ATTRIB_EDITDOCUMENT)
		Object attribEdit = this.getPageContext().getRequest().getAttribute( WGACore.ATTRIB_EDITDOCUMENT );
		if( attribEdit != null 
			&& getEditor() != null
			&& attribEdit.equals(this.getTMLContext().getcontent().getContentKey().toString()) ){
			    buildEditor(itemName, result);
	            setResult(result);  
		}
		else {			            
            // if aliases are defined, replace values with aliases
            List aliases = this.retrieveAliases();
            if(aliases.isEmpty())
            	this.setResult(result);
            else {
            	WGA wga = WGA.get();
            	this.setResult(wga.aliases(WGUtils.toString(result), aliases));
            }

		}
	}

    private List retrieveAliases() throws WGAPIException {
        
        // Fetch aliases, either directly from item (Attribute aliasesitem) or as comma-separated string (Attribute aliases)
       String aliasesItem = getAliasesitem();
       if (aliasesItem != null) {
           return WGUtils.toString(getTMLContext().itemlist(aliasesItem));
       }
       else {
           String aliases = this.getAliases();
           if (aliases == null) {
               return new ArrayList<String>();
           }
           return WGUtils.deserializeCollection(aliases, ",");
       }
    }

	private void buildEditor(String itemName, List result) throws WGException {

		String rawLabel = (this.getLabel() == null ? this.getName() : this.getLabel());
		itemName = itemName.toLowerCase();
		String editor = this.getEditor();

		StringBuffer prefix = createItemEditorDeclaration(itemName, editor, rawLabel);
		
		prefix.append("<div class=\"WGA-Item-Format\" style=\"display:none\">");
		if(getFormat()!=null)
			prefix.append(getFormat());
		prefix.append("</div>");
		
		prefix.append("<div class=\"WGA-Item-Value\" id=\"item_"+itemName+"\">");
		
		// <-- item content will be inserted here between prefix and suffix
		
		StringBuffer suffix = new StringBuffer("</div>\n");
		
		if(editor.equalsIgnoreCase("rtf") || editor.equalsIgnoreCase("textblock")){
            // Add unencoded version to suffix. Will be used by RTF editor for updates
            suffix.append("<div class=\"WGA-Item-Value-Unencoded\" id=\"item_"+itemName+"_unencoded\" style=\"display:none\" >");
            if (result.size() >= 1) {
                RTFEncodingFormatter formatter = new RTFEncodingFormatter(true);
                formatter.setContext(getTMLContext());
                try {
                    suffix.append(formatter.format(result.get(0)));
                }
                catch (FormattingException e) {
                    addWarning(e.getMessage());
                    suffix.append(String.valueOf(result.get(0)));
                }
            }
            suffix.append("</div>\n");
		}
		else if (editor.equalsIgnoreCase("date")) {
			suffix.append("<div class=\"WGA-Item-Value-Unencoded\" style=\"display:none\" >");
			if (result.size() > 0)
				suffix.append(WGA.get(getTMLContext()).format(result.get(0), "dd.MM.yyyy"));
			suffix.append("</div>\n");
		}
		else if (editor.equalsIgnoreCase("number")) {
			suffix.append("<div class=\"WGA-Item-Value-Unencoded\" style=\"display:none\" >");
			if (result.size() > 0)
				suffix.append(WGA.get(getTMLContext()).format(result.get(0), "decimal"));
			suffix.append("</div>\n");
		}
		else if (editor.equalsIgnoreCase("custom")) {
		    suffix.append(getCustomEditorCode());
		}
        suffix.append("\n</div>\n");
		
        this.setPrefix(prefix.toString());
		this.setSuffix(suffix.toString());

	}

    /**
	 * Method getCustomEditor.
	 * @param editor
	 * @return Editor code as String
     * @throws WGException 
	 * @throws UnsupportedEncodingException 
	 */
	private String getCustomEditorCode() throws WGException {
	    
	    FormStatus status = getFormStatus();
		
		// Calculate output
		
		PropertyResourceBundle labels = null;
		try{
			labels = (PropertyResourceBundle)ResourceBundle.getBundle("de.innovationgate.wgpublisher.labels.common" , pageContext.getRequest().getLocale() ,this.getClass().getClassLoader());
		}
		catch(MissingResourceException e){	
			System.out.println( e.getMessage() );
		}
		
		status.mode = TMLFormInfo.EDIT_MODE; 
		
		String frmId = status.formInfo.getFormId();
		
		StringBuffer editorCode = new StringBuffer("");
		String LS = System.getProperty("line.separator");
		
		editorCode.append(LS);
		editorCode.append("<div class=\"WGA-Custom-Form\" style=\"display:none;\">");
		editorCode.append(LS);		
		
		StringBuffer submitCode = new StringBuffer();
        submitCode.append("callAction('");
        String action = buildCallActionLink(getSaveaction() , frmId, null, null, null, null);
        submitCode.append(action);
        submitCode.append("')");
				
		String formStartTag = renderFormStartTag(frmId, submitCode.toString(), "display:inline", null );
		editorCode.append(formStartTag);
		
		String prefix = this.getPrefix();		
		this.setPrefix("");
		editorCode.append(this.getResultString(false));
		this.setPrefix(prefix);	
		
		editorCode.append(renderFormInfo(status.formInfo, this.getTMLContext()));
		editorCode.append(LS);
					
		editorCode.append(renderAdditionHiddenFormFields());
		editorCode.append(LS);		
		editorCode.append("&nbsp;");
		editorCode.append(LS);
		editorCode.append(renderFormEndTag());
		editorCode.append(LS);
		editorCode.append("</div>");
		
		if( status.thisForm.hasmessages() ){
		    editorCode.append("<div class=\"WGA-Custom-Form-Errors\" style=\"display:none;\">");
		    for (String message : (List<String>) status.thisForm.getmessages()) {
		        editorCode.append("<div class=\"WGA-Custom-Form-Error-Message\">" + WGUtils.encodeHTML(message) + "</div>");
		    }
		    
		    editorCode.append("</div>");
		}
		
		return editorCode.toString();
	}

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Gets the editor
	 * @return Returns a String
	 */
	public String getEditor() {
		return this.getTagAttributeValue("editor", editor, null);
	}
	/**
	 * Sets the editor
	 * @param editor The editor to set
	 */
	public void setEditor(String editor) {
		this.editor = editor;
	}

	/**
	 * Gets the label
	 * @return Returns a String
	 */
	public String getLabel() {
		return this.getTagAttributeValue("label", label, null);
	}
	/**
	 * Sets the label
	 * @param label The label to set
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * Returns the type.
	 * @return String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", type, "content");
	}

	/**
	 * Sets the type.
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}



	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlStartTag()
	 */
	public void tmlStartTag() throws WGException {
		String editor = this.getEditor();
		if (editor == null) {
			this.setEvalBody(false);
			return;
		}
	    
	    if( editor.equalsIgnoreCase("custom")) {
			super.tmlStartTag();
	    }
	}

	/**
	 * Returns the scriptlets.
	 * @return String
	 */
	public String getScriptlets() {
		return this.getTagAttributeValue("scriptlets", scriptlets, "false");
	}

	/**
	 * Sets the scriptlets.
	 * @param scriptlets The scriptlets to set
	 */
	public void setScriptlets(String scriptlets) {
		this.scriptlets = scriptlets;
	}

	/**
	 * @see de.innovationgate.wgpublisher.webtml.Base#getEncode()
	 */
	public String getEncode() {
		return this.getTagAttributeValue("encode", encode, getDefaultItemEncoding());
	}

    public String getDefaultItemEncoding() {
        
        String editor = getEditor();
        if (editor != null) {
            if (editor.equalsIgnoreCase("rtf")) {
                return "rtf";
            }
            else if (editor.equalsIgnoreCase("textblock")) {
                return "none";
            }
        }
        
        return (String) this.getTMLContext().getDesignContext().getDesignDB().getAttribute(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING);
    }

	/**
	 * @return
	 */
	public String getXpath() {
		return getTagAttributeValue("xpath", xpath, null);
	}

	/**
	 * @param string
	 */
	public void setXpath(String string) {
		xpath = string;
	}

    public String getAliases() {
        return getTagAttributeValue("aliases", aliases, null);
    }

    public void setAliases(String aliases) {
        this.aliases = aliases;
    }

    public String getHighlight() {
        return getTagAttributeValue("highlight", highlight, "false");
    }

    public void setHighlight(String highlight) {
        this.highlight = highlight;
    }

    public String getHighlightprefix() {
        return getTagAttributeValue("highlightprefix", highlightprefix, "<B>");
    }

    public void setHighlightprefix(String highlightprefix) {
        this.highlightprefix = highlightprefix;
    }

    public String getHighlightsuffix() {
        return getTagAttributeValue("highlightsuffix", highlightsuffix, "</B>");
    }

    public void setHighlightsuffix(String highlightsuffix) {
        this.highlightsuffix = highlightsuffix;
    }

    public String getSaveaction() {
        return getTagAttributeValue("saveaction", saveaction, "$store");
    }

    public void setSaveaction(String saveaction) {
        this.saveaction = saveaction;
    }
	
    public String getAliasesitem() {
        return getTagAttributeValue("aliasesitem", aliasesitem, null);
    }

    public void setAliasesitem(String aliasesitem) {
        this.aliasesitem = aliasesitem;
    }

    @Override
    protected String getFormId() {
        return "_" + getName().toLowerCase() + "_";
    }
        
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "a"); 
    }

	
}
