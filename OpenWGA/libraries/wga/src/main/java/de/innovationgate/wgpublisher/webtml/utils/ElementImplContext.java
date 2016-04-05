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
package de.innovationgate.wgpublisher.webtml.utils;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Map;

import javassist.bytecode.CodeIterator.Gap;

import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
/**
 * The context of the tml runtime, provided to the methods of an element implementation to communicate with the runtime. Can be used to 
 * request various information from the runtime or to produce output.
 */
public class ElementImplContext {
	
	private Map options;
	private TMLContext context;
	private de.innovationgate.wgpublisher.webtml.Element.Status tagContext;
	private StringBuffer result = new StringBuffer();
	
	/**
	 * Constructor of the context. Only to be used by wga functionalities itself.
	 * @param options Contents of option tags for this element.
	 * @param context The tml context object of this tag.
	 * @param tagContext The element tag that is currently rendered
	 */
	public ElementImplContext(Map options, TMLContext context, de.innovationgate.wgpublisher.webtml.Element.Status tagContext) {
		this.options = options;
		this.context = context;
		this.tagContext = tagContext;
	}

	/**
	 * Retrieves the tml context object of the element tag.
	 * @return Returns a TMLContext.
	 * @deprecated Use {@link #getContext()} to retrieve the server API interface object
	 */
	public TMLContext getTMLContext() {
		return context;
	}
	
	/**
     * Retrieves the WebTML context of the element tag
     * @return Returns a TMLContext.
     */
    public Context getContext() {
        return context;
    }

	/**
	 * Returns the options passed to this tml:element via tml:option tags as key/value pairs.
	 * @return Returns a Map
	 */
	public Map getOptions() {
		return options;
	}

	/**
	 * Gets the output result of the current tag as StringBuffer.
	 * @return Returns a StringBuffer
	 */
	public StringBuffer getResult() {
		return result;
	}

	/**
	 * Appends text to the current output result of the tag.
	 * @param result The text to append.
	 */
	public void appendResult(String result) {
		this.result.append(result);
	}
	
	/**
	 * Retrieves the WGAPI content object that is in the current tml context.
	 * @return The content object.
	 */
	public WGContent getContent() {
		return this.context.content();
	}
	
	/**
		 * Retrieves the WGAPI content object that is in the current tml context.
		 * @return The content object.
		 */
	public WGDocument getDocument() {
		return (this.context.content());
	}
	
	/**
	 * Changes the context of the tml:body tag for it's next iteration to the given content.
	 * @param content A WGAPI content object
	 * @throws WGAPIException 
	 */
	public void setContentForBody(WGContent content) throws WGAPIException {
		
		Context childContext = this.context.context(content);
		if (childContext != null) {
			this.tagContext.bodyTag.setChildTagContext((TMLContext) childContext);
		}
		 
	}
	
	/**
	 * Retrieves the WGAPI database object of the content currently in tml context.
	 * @return The database object
	 */
	public WGDatabase getDatabase() {
		return this.context.content().getDatabase();
	}
	/**
	 * Retrieves the tml tag object of the tml element tag.
	 * @return A tml tag object.
	 */
	public de.innovationgate.wgpublisher.webtml.BaseTagStatus getTagContext() {
		return tagContext;
	}

	/**
	 * Used to put out a warning, displayable via tml:warnings tag.
	 * @param text Text of the warning.
	 */
	public void addWarning(String text) {
		this.context.addwarning(text);
	}
	/**
	 * Retrieves all database keys of currently active content databases in WGA.
	 * @return Collection of database key strings.
	 */
	public java.util.Collection getDatabaseKeys() {
		return ((TMLContext) this.context).getwgacore().getContentdbs().keySet();
	}
	
	/**
	 * The main tml context of the currently rendered tml page, i.e. the context initially requested by the calling URL.
	 * @return The tml context object
	 * @throws WGAPIException 
	 */
	public Context getMainContext() throws WGAPIException {
		return this.context.context("main");
	}
	 
	/**
	 * Retrieves the tag object of the tml tag with the given id.
	 * @param id The id of the tag to retrieve
	 * @return A tml tag object.
	 */
	public BaseTagStatus getTagById(String id) {
		return this.tagContext.getTagStatusById(id);
	}
	
	/**
	 * Retrieves the URL base path, under which WGA is available, including hostname and j2ee project path. Can be used to build URLs to other WGA resources.
	 * @return URL base path string.
	 */
	public String getWGPPath() {
		return ((TMLContext) this.context).getEnvironment().getPublisherURL();
	}
	
	/**
	 * Clears the current output result.
	 */
	public void clearResult() {
		this.result = new StringBuffer();
	}
	
	/**
	 * Retrieves the JSP page context object, that normally is available in JSP scripts as object "pageContext".
	 * @return A JSP page context object.
	 */
	public javax.servlet.jsp.PageContext getPageContext() {
		return ((TMLContext) this.context).getEnvironment().getPageContext();
	}
	
	/**
	 * Retrieves the output stream of the current request to put out binary data. This is only usable if the media key of the current request is binary.
	 * @return An output stream object
	 */
	public OutputStream getOutputStream() {
	    try {
		    return ((TMLContext) this.context).getEnvironment().getPageContext().getResponse().getOutputStream();
    	}
        catch (IOException e) {
            getLog().error("Error retrieving output stream", e);
            return null;
        }
	}
	
	/**
	 * Retrieves the HTTP servlet response object of the current request.
	 * @return A http servlet response object
	 */
	public HttpServletResponse getResponse() {
		return (HttpServletResponse) ((TMLContext) this.context).getEnvironment().getPageContext().getResponse();
	}
	
	/**
	 * Retrieves the application log object, to log output to application log directly. The WGAPI uses LOG4J from Apache Jakarta project for logging purposes.
	 * For details about the returned object and how to use it see the documentation on their homepage: http://jakarta.apache.org/log4j
	 * @return A logger object
	 */
	public Logger getLog() {
		return this.context.getlog();
	}
}

