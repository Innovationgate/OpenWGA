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

package de.innovationgate.wga.server.api.tml;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TagInfo;

/**
 * The Context is the base object of the WebTML runtime. It combines two purposes:
 * It represents the document context under which a WebTML functionality is executed, mostly the context of some WebTML tag or TMLScript script. That is it points to a special content document and offers it's item and metadata fields, just like <tml:item> and <tml:meta> do in WebTML.
 * It offers the most frequently used TMLScript functionalities as methods, like for example many condition tests, context changes, and access to WGAPI objects
 * This object is the same as the TMLContext object in TMLScript. Documentation on this object and its methods in more detail can therefor be found on the TMLScript reference of the OpenWGA documenation library of the respective OpenWGA version for Object "TMLContext". 
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public interface Context {

    /**
     * Adds a custom WebTML warning
     * @param msg Message of the warning
     */
    @CodeCompletion(preferredCase="addWarning")
    public abstract void addwarning(String msg);

    /**
     * Adds a custom WebTML warning
     * @param msg Message of the warning
     * @param severe Flag that marks the warning as "severe". Severe warnings will cancel the execution of the current functionality and wil be displayed with a different icon in <tml:warnings/>. Defaults to false. 
     */
    @CodeCompletion(preferredCase="addWarning")
    public abstract void addwarning(String msg, boolean severe);
    
    /**
     * Adds a custom WebTML warning
     * @param msg Message of the warning
     * @param severe Flag that marks the warning as "severe". Severe warnings will cancel the execution of the current functionality and wil be displayed with a different icon in <tml:warnings/>. Defaults to false.
     * @param cause A Java exception that caused the warning. It's stack trace will be put out to the log, if warnings logging is enabled 
     */
    @CodeCompletion(preferredCase="addWarning")
    void addwarning(String msg, boolean severe, Throwable cause);
    
    /**
     * Appends a value to a WebTML variable as list element 
     * @param name Name of a WebTML variable
     * @param value Value to append as list element to the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="appendVar")
    public abstract void appendvar(String name, Object value) throws WGAPIException;

    /**
     * Returns a URL to the content document in context using login parameter
     */
    @CodeCompletion(preferredCase="loginURL")
    public abstract String loginurl() throws WGException;
    
    
    /**
     * Provides the WGAPI object "WGContent" which represents the current content document in context
     */
    @CodeCompletion
    public abstract WGContent content();

    /**
     * Returns a URL to the content document in context
     * @throws WGException
     */
    @CodeCompletion(preferredCase="contentURL")
    public abstract String contenturl() throws WGException;

    /**
     * Returns a URL to the content document in context
     * @param mediaKey Mediakey of the WebTML module that should be used as outer layout for the document. Specify null to render it with its content types layouts.
     * @throws WGException
     */
    @CodeCompletion(preferredCase="contentURL")
    public abstract String contenturl(String mediaKey) throws WGException;

    /**
     * Returns a URL to the content document in context
     * @param mediaKey Mediakey of the WebTML module that should be used as outer layout for the document. Specify null to render it with its content types layouts.
     * @param layoutKey Name of the WebTML module that should be used as outer layout for the document. Specify null to render it with its content types layouts.
     * @throws WGException
     */
    @CodeCompletion(preferredCase="contentURL")
    public abstract String contenturl(String mediaKey, String layoutKey) throws WGException;

    /**
     * Returns a URL to the content document in context
     * @param mediaKey Mediakey of the WebTML module that should be used as outer layout for the document. Specify null to render it with its content types layouts.
     * @param layoutKey Name of the WebTML module that should be used as outer layout for the document. Specify null to render it with its content types layouts.
     * @param ignoreVirtualLink If true and the context document is virtual this method returns a normal URL to the virtual document instead of the virtual target URL. 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="contentURL")
    public abstract String contenturl(String mediaKey, String layoutKey, boolean ignoreVirtualLink) throws WGException;

    /**
     * Creates a TMLContext object for a different content document.
     * This method offers the same functionality as the WebTML attribute context. It received a context expression, retrieves the content document that is addressed by it and returns a TMLContext object that has this document in context. 
     * @param expression An OpenWGA context expression. Syntax is identical to that of WebTML attribute context
     * @return  A TMLContext that matches the content that was addressed in the context expression. If context change fails returns the original TMLContext.  
     */
    @CodeCompletion
    public abstract Context context(String expression);

    /**
     * Creates a TMLContext object for a different content document.
     * This method offers the same functionality as the WebTML attribute context. It received a context expression, retrieves the content document that is addressed by it and returns a TMLContext object that has this document in context. 
     * @param expression An OpenWGA context expression. Syntax is identical to that of WebTML attribute context
     * @param returnContextOnError Determines if on context change error the original context is to be returned. If false this method returns null on a failed context change.
     * @return A TMLContext that matches the content that was addressed in the context expression. If context change fails returns either the original TMLContext or null if parameter "returnContextOnError" was set to false.   
     */
    @CodeCompletion
    public abstract Context context(String expression, boolean returnContextOnError);

    /**
     * Takes a content document in form of the WGAPI object "WGContent" and constructs a TMLContext object that has it in context.
     * @param content The WGAPI content object
     * @return A TMLContext representing the given content document
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Context context(WGContent content) throws WGAPIException;

    /**
     * Creates a new PortletEvent object
     * @param name Name of the portlet event. All event receivers subscribing to this name will receive it when it is thrown.
     */
    @CodeCompletion(preferredCase="createEvent")
    public abstract PortletEvent createevent(String name);

    /**
     * Returns a WGAPI database object for the database of the current context
     */
    @CodeCompletion
    public abstract WGDatabase db();

    /**
     * Creates a URL pointing to a file attachment on the current context document
     * @param fileName Name of the file attachment on file container or content document. If the file is a ZIP archive you can address files inside it by specifying the path in the ZIP file after the name, divided by slashes.
     * @return URL pointing to a file attachment 
     * @throws UnsupportedEncodingException
     * @throws WGException
     */
    @CodeCompletion(preferredCase="fileURL")
    public abstract String fileurl(String fileName) throws UnsupportedEncodingException, WGException;

    /**
     * Creates a URL pointing to a file attachment on a content document or file container in the context database
     * @param containerName Name of a file container or key of a content document 
     * @param fileName Name of the file attachment on file container or content document. If the file is a ZIP archive you can address files inside it by specifying the path in the ZIP file after the name, divided by slashes.
     * @return      URL pointing to a file attachment 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="fileURL")
    public abstract String fileurl(String containerName, String fileName) throws WGException;

    /**
     * Creates a URL pointing to a file attachment on a content document or file container
     * @param dbKey Database key of the database whose file is to be served.
     * @param containerName Name of a file container or key of a content document 
     * @param fileName Name of the file attachment on file container or content document. If the file is a ZIP archive you can address files inside it by specifying the path in the ZIP file after the name, divided by slashes.
     * @return      URL pointing to a file attachment 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="fileURL")
    public abstract String fileurl(String dbKey, String containerName, String fileName) throws WGException;

    @CodeCompletion(preferredCase="fileDataURL")
    public abstract String filedataurl(String fileName, Map<String,String> config) throws WGException;

    @CodeCompletion(preferredCase="fileDataURL")
    public abstract String filedataurl(String fileName) throws WGException;

    /**
     * Returns the JavaEE object representing the current HTTP browser session
     */
    @CodeCompletion(preferredCase="httpSession",isProperty=true)
    public abstract HttpSession gethttpsession();

    /**
     * Returns the error message of the last failed context expression that was done from this context object
     */
    @CodeCompletion(preferredCase="lastError")
    public abstract String getlasterror();

    /**
     * Provides an object for logging output to the application log
     */
    @CodeCompletion(preferredCase="log",isProperty=true)
    public abstract Logger getlog();

    /**
     * Returns an absolute context path to the current context document
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="path",isProperty=true)
    public abstract String getpath() throws WGAPIException;

    /**
     * Returns a portlet object for the current WebTML portlet
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="portlet",isProperty=true)
    public abstract Portlet getportlet() throws WGAPIException;

    /**
     * Returns the users personalisation profile as profile object 
     */
    @CodeCompletion(preferredCase="profile",isProperty=true)
    public abstract UserProfile getprofile();

    /**
     * Returns the JavaEE object representing the current HTTP browser request
     */
    @CodeCompletion(preferredCase="request",isProperty=true)
    public abstract javax.servlet.http.HttpServletRequest getrequest();

    /**
     *  Returns the JavaEE object representing the current HTTP server response
     */
    @CodeCompletion
    public abstract HttpServletResponse getresponse();

    /**
     * Retrieves a WebTML session variable
     */
    @CodeCompletion(preferredCase="getSessionVar")
    public abstract Object getsessionvar(String name);

    /**
     * Returns the TMLForm object representing the current WebTML form
     */
    @CodeCompletion(preferredCase="tmlform",isProperty=true)
    public abstract Form gettmlform();

    /**
     * Retrieves a normal WebTML variable
     * @throws WGAPIException 
     */
    @CodeCompletion(preferredCase="getVar")
    public abstract Object getvar(String name) throws WGAPIException;

    /**
     * Returns if the current context document has child documents
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasChildren")
    public abstract boolean haschildren() throws WGAPIException;

    /**
     * Tests if the user is member of the specified group 
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasGroup")
    public abstract boolean hasgroup(String group) throws WGAPIException;

    /**
     * Tests if a label is available for the given key that can be served to the user
     * Uses either the default label container/file or the one determined for the current WebTML range. 
     * @param key The label key
     */
    @CodeCompletion(preferredCase="hasLabel")
    public abstract boolean haslabel(String key);

    /**
     * Tests if a label is available for the given key that can be served to the user
     * Uses either the default label container "labels_<i>language</i>" or the one determined for the current WebTML range.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     */
    @CodeCompletion(preferredCase="hasLabel")
    public abstract boolean haslabel(String fileName, String key);

    /**
     * Tests if a label is available for the given key that can be served to the user
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     */
    @CodeCompletion(preferredCase="hasLabel")
    public abstract boolean haslabel(String containerName, String fileName, String key);

    /**
     * Tests if an WebTML option of the given name is defined
     */
    @CodeCompletion(preferredCase="hasOption")
    public abstract boolean hasoption(String option);

    /**
     * Returns if the user currently has a user profile
     */
    @CodeCompletion(preferredCase="hasProfile")   
    public abstract boolean hasprofile();

    /**
     * Tests if the user has the given user role
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasRole")
    public abstract boolean hasrole(String role) throws WGAPIException;

    /**
     * Returns if the current context document has sibling documents
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasSiblings")
    public abstract boolean hassiblings() throws WGAPIException;

    /**
     * Returns a  WGAPI HDB object for the application in context
     * @throws WGException
     */
    @CodeCompletion
    public abstract WGHierarchicalDatabase hdb() throws WGException;

    /**
     * Tests if the current user is anonymous
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isAnonymous")
    public abstract boolean isanonymous() throws WGAPIException;

    /**
     * Returns true if the WebTML page is rendered in some kind of OpenWGA authoring client like the OpenWGA Content Manager
     */
    @CodeCompletion(preferredCase="isBrowserInterface")
    public abstract boolean isbrowserinterface();

    /**
     * Tests if a name is defined as WebTML variable or item on the context document
     * @param name Name of WebTML variable or item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isDefined")
    public abstract boolean isdefined(String name) throws WGAPIException;

    /**
     * Returns if the given WebTML variable or item is regarded "empty".
     * Empty means being either nonexistent, null, empty/whitespace-only string, an empty list or a string with a single HTML linebreak
     * @param itemName Name of a WebTML variable or item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isEmpty")
    public abstract boolean isempty(String itemName) throws WGAPIException;

    /**
     * Returns if the given value is regarded "empty"
     * Empty means being either nonexistent, null, empty/whitespace-only string, an empty list or a string with a single HTML linebreak
     * @param value Value to test
     */
    @CodeCompletion(preferredCase="isEmptyValue")
    public abstract boolean isemptyvalue(Object value);

    /**
     * Returns if the contents of an item or WebTML variable is regarded as "filled", meaning it is not empty like defined on {@link #isempty(String)}.
     * This method returns always true when in authoring mode for a document.
     * @param attValue The name of the item or WebTML variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isFilled")
    public abstract boolean isfilled(String attValue) throws WGAPIException;

    /**
     * Tests if a tag <tml:foreach> is in its first iteration.
     * This method uses the "nearest" tag of that type up the tag cascading hierarchy.
     */
    @CodeCompletion(preferredCase="isFirstLoop")
    public abstract boolean isfirstloop();

    /**
     * Tests if a tag <tml:foreach> is in its first iteration.
     * @param tagId id of the foreach tag
     */
    @CodeCompletion(preferredCase="isFirstLoop")
    public abstract boolean isfirstloop(String tagId);

    /**
     * Tests if a tag <tml:foreach> is in its last iteration.
     * This method uses the "nearest" tag of that type up the tag cascading hierarchy.
     */
    @CodeCompletion(preferredCase="isLastLoop")
    public abstract boolean islastloop();

    /**
     * Tests if a tag <tml:foreach> is in its last iteration.
     * @param tagId id of the foreach tag
     */
    @CodeCompletion(preferredCase="isLastLoop")
    public abstract boolean islastloop(String tagId);

    /**
     * Returns if the document in context is the main document of the request
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isMainDocument")
    public abstract boolean ismaindocument() throws WGAPIException;

    /**
     * Returns if the current request is the first request in a new browser session
     */
    @CodeCompletion(preferredCase="isNewSession")
    public abstract boolean isnewsession();

    /**
     * Returns if the current context document is a root document
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isRoot")
    public abstract boolean isroot() throws WGAPIException;

    /**
     * Returns if the current document is the main document of the current request or an ancestor document of it
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isSelected")
    public abstract boolean isselected() throws WGAPIException;
    
    /**
     * Returns if the current document is either the document of the given main context or an ancestor document of it
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isSelected")
    public abstract boolean isselected(Context mainContext) throws WGAPIException;

    /**
     * Tests if a WebTML tag with the given id is available 
     * @param tagid
     */
    @CodeCompletion(preferredCase="isTagIDValid")
    public abstract boolean istagidvalid(String tagid);

    /**
     *  Returns if the contents of an item or WebTML variable is regarded "false".
     * @param varname The name of the item or WebTML variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isFalse")
    public abstract boolean isfalse(String varname) throws WGAPIException;

    /**
     *  Returns if the contents of an item or WebTML variable is regarded "true".
     *  Values that are regarded true depends on version complience:
     *  before 7.2: JavaScript/Java boolean value of true, the string "true", a numeric value of 1 or -1
     *  7.2 and later: like in javascript
     * @param varname The name of the item or WebTML variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="isTrue")
    public abstract boolean istrue(String varname) throws WGAPIException;

    /**
     *  Returns if the current TMLScript code is executed in the context of a WebTML request
     */
    @CodeCompletion(preferredCase="isWebEnvironment")
    public abstract boolean iswebenvironment();

    /**
     * Returns the value of an item or a WebTML variable as single value.
     * @param name Name of the item or WebTML variable
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object item(String name) throws WGAPIException;

    /**
     * Returns the value of an item or WebTML variable as list value
     * @param name Name of the item or WebTML variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="itemList")
    public abstract List<Object> itemlist(String name) throws WGAPIException;

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either default the label container/file or the ones specified for the current WebTML range 
     * @param key The label key
     */
    @CodeCompletion
    public abstract String label(String key);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either default the label container/file or the ones specified for the current WebTML range 
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     */
    @CodeCompletion
    public abstract String label(String key, List<String> params);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     */
    @CodeCompletion
    public abstract String label(String fileName, String key);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     */
    @CodeCompletion
    public abstract String label(String fileName, String key, List<String> params);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     */
    @CodeCompletion
    public abstract String label(String containerName, String fileName, String key);
    
    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     */
    @CodeCompletion
    public abstract String label(String containerName, String fileName, String key, List<String> params);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     * @param usePlaceholder Specify true to return a label placeholder in case the label does not exist. Specify false to return null in that case.
     */
    @CodeCompletion
    public abstract String label(String containerName, String fileName, String key, List<String> params, boolean usePlaceholder);

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param designDB The WGAPI database object of the app from which to load labels
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     */
    @CodeCompletion
    public abstract String label(WGDatabase designDB, String containerName, String fileName, String key, List<String> params) throws WGException;

    /**
     * Returns a WebTML label in a language that the current user should understand.
     * This variant uses either the label container or the one specified for the current WebTML range
     * @param designDB The WGAPI database object of the app from which to load labels
     * @param containerName Label container base name. Omit the language suffix.
     * @param fileName Label file holding label definitions. Omit the suffix ".properties".
     * @param key The label key
     * @param params Label parameters that are injected into the label text
     * @param usePlaceholder Specify true to return a label placeholder in case the label does not exist. Specify false to return null in that case.
     */
    @CodeCompletion
    public abstract String label(WGDatabase designDB, String containerName, String fileName, String key, List<String> params, boolean usePlaceholder) throws WGException;

    /**
     * Returns the value of a metadata field of type "content" as single value.
     * @param name The name of the metadata field
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object meta(String name) throws WGAPIException;

    /**
     * Returns the value of a metadata field as single value.
     * @param type The type of metadata field
     * @param name The name of the metadata field
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object meta(String type, String name) throws WGAPIException;

    /**
     * Returns the value of a metadata field of type "content" as list value.
     * @param name The name of the metadata field
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="metaList")
    public abstract List<Object> metalist(String name) throws WGAPIException;

    /**
     * Returns the value of a metadata field as list value.
     * @param type The type of metadata field
     * @param name The name of the metadata field
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="metaList")
    public abstract List<Object> metalist(String type, String name) throws WGAPIException;

    /**
     * Retrieves the value of a WebTML option. Return null if the option does not exist.
     * @param option Name of the option
     */
    @CodeCompletion
    public abstract Object option(String option);

    /**
     * Retrieves the value of a WebTML option.
     * @param option Name of the option
     * @param defaultValue A value to return if the option does not exist
     */
    @CodeCompletion
    public abstract Object option(String option, Object defaultValue);

    /**
     *  Retrieves a portlet by a portlet path expression
     * @param path The portlet path
     * @throws WGException
     */
    @CodeCompletion(preferredCase="portletByPath")
    public abstract Portlet portletbypath(String path) throws WGException;

    /**
     * Removes a WebTML session variable
     * @param name Name of the variable
     */
    @CodeCompletion(preferredCase="removeSessionVar")
    public abstract void removesessionvar(String name);

    /**
     * Removes the form of the given ID from form registry
     * @param formid Id of the form
     */
    @CodeCompletion(preferredCase="removeTMLForm")
    public abstract void removetmlform(String formid);

    /**
     *  Removes a (normal) WebTML variable
     * @param name Name of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="removeVar")
    public abstract void removevar(String name) throws WGAPIException;

    /**
     * Sets a WebTML option
     * @param name Name of the option
     * @param value Value of the option
     */
    @CodeCompletion(preferredCase="setOption")
    public abstract void setoption(String name, Object value);

    /**
     * Sets a WebTML option
     * @param name Name of the option
     * @param value Value of the option
     * @param scope Scope of the option. Use strings "global" or "local".
     */
    @CodeCompletion(preferredCase="setOption")
    public abstract void setoption(String name, Object value, String scope);

    /**
     * Sets or updates a WebTML session variable. Variables set by this variant are allowed for serialisation.
     * @param name Name of the variable
     * @param value Value of the variable
     */
    @CodeCompletion(preferredCase="setSessionVar")
    public abstract void setsessionvar(String name, Object value);


    /**
     * Sets or updates a WebTML session variable.
     * @param name Name of the variable
     * @param value Value of the variable
     * @param allowSerialisation Determines if the variable value can be serialized and therefor transported to other nodes in a cluster 
     */
    @CodeCompletion(preferredCase="setSessionVar")
    public abstract void setsessionvar(String name, Object value, boolean allowSerialisation);

    /**
     * Creates or updates a (normal) WebTML variable
     * @param name Name of the variable
     * @param value Value of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setVar")
    public abstract void setvar(String name, Object value) throws WGAPIException;
    
    /**
     * Retrieves a WebTML tag information
     * @Deprecated use tag(id).getInfo() instead
     * @param tagId id of the tag to retrieve the info from
     * @param name Name of the info field
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="tagInfo")
    @Deprecated
    public abstract Object taginfo(String tagId, String name) throws WGAPIException;
    
    /**
     * Retrieves a WebTML tag information as Object
     * @param tagId id of the tag to retrieve the info from
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="tag")
    public abstract TagInfo tag(String tagId) throws WGAPIException;
    @CodeCompletion(preferredCase="tag")
    public abstract TagInfo tag() throws WGAPIException;

    /**
     * Returns a TMLForm object representing the WebTML form of the given ID
     * @param id id of the form
     */
    @CodeCompletion(preferredCase="tmlformByID")
    public abstract Form tmlformbyid(String id);


    @CodeCompletion(preferredCase="isHomePage")
    public abstract boolean ishomepage();
 
}
