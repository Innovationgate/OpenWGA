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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.apache.commons.httpclient.URIException;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext;

/**
 * Represents a WebTML Form, containing form fields ans uploaded files
 * This object is the same as the TMLForm object in TMLScript. Documentation on this object and its methods in more detail can therefor be found on the TMLScript reference of the OpenWGA documenation library of the respective OpenWGA version for Object "TMLForm".
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public interface Form {

    /**
     * Adds a file from the servers file system to the form
     * @param source The file
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    @CodeCompletion(preferredCase="addFile")
    public abstract void addfile(File source) throws IOException, NoSuchAlgorithmException;

    /**
     * Adds a file from the servers file system to the form
     * @param source The file whose data is attached
     * @param fileName The name under which the file should get attached
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    @CodeCompletion(preferredCase="addFile")
    public abstract void addfile(File source, String fileName) throws IOException, NoSuchAlgorithmException;

    /**
     * Adds a file from the servers file system to the form
     * @param stream The input stream serving the files ata
     * @param fileName The name under which the file should get attached
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    @CodeCompletion(preferredCase="addFile")
    public abstract void addfile(InputStream stream, String fileName) throws IOException, NoSuchAlgorithmException;

    /**
     * Adds a new global validation message
     * @param message The message
     */
    @CodeCompletion(preferredCase="addMessage")
    public abstract void addmessage(String message);

    /**
     * Appends a value to the value list of a field
     * @param name Name of the field
     * @param value Value to append
     * @throws WGException
     */
    @CodeCompletion(preferredCase="appendToField")
    public abstract void appendtofield(String name, Object value) throws WGException;

    /**
     * Attaches all files stored on this form to a document
     * @param doc
     * @throws IOException
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract boolean attach(WGDocument doc) throws IOException, WGAPIException;

    /**
     * Clears all validation messages on the form
     */
    @CodeCompletion(preferredCase="clearMessages")
    public abstract void clearmessages();

    /**
     * Returns the unprocessed value of a field like the user entered it
     * @param fieldname The field name
     */
    @CodeCompletion(preferredCase="enteredValue")
    public abstract Object enteredvalue(String fieldname);

    /**
     * Returns the value of a field as a single value.
     * @param name Name of the field
     */
    @CodeCompletion
    public abstract Object field(String name);

    /**
     * Returns the value of a field as a list value.
     * @param name Name of the field
     */
    @CodeCompletion(preferredCase="fieldList")
    public abstract List<Object> fieldlist(String name);

    /**
     * Returns a URL to the uploaded file of the given name
     * @param fileName Name of the file
     * @throws URIException
     */
    @CodeCompletion(preferredCase="fileURL")
    public abstract String fileurl(String fileName) throws URIException;

    /**
     * Retrieves the document that was created by a TMLForm store operation 
     */
    @CodeCompletion(preferredCase="getCreatedDoc()")
    public abstract WGDocument getcreateddoc();

    /**
     * Lists data type conversion errors 
     */
    @CodeCompletion(preferredCase="errors",isProperty=true)
    public abstract List<String> geterrors();

    /**
     * Lists the names of the fields on this form
     */
    @CodeCompletion(preferredCase="fieldNames",isProperty=true)
    public abstract List<String> getfieldnames();

    /**
     * Returns a Java file object for an uploaded file
     * @param name Name of the file
     * @throws WGAPIException
     * @throws IOException
     */
    @CodeCompletion(preferredCase="getFile")
    public abstract File getfile(String name) throws WGAPIException, IOException;

    /**
     * Lists the names of files that have been uploaded to this form
     */
    @CodeCompletion(preferredCase="fileNames",isProperty=true)
    public abstract List<String> getfilenames();

    /**
     * Returns the text contents of an uploaded file
     * @param name Name of the file
     * @throws IOException
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="getFileText")
    public abstract String getfiletext(String name) throws IOException, WGAPIException;

    /**
     * Returns the ID of the form
     */
    @CodeCompletion(preferredCase="formId",isProperty=true)
    public abstract String getformid();

    /**
     * Grants access to the FormInfo object of this WebTML form
     */
    @CodeCompletion(preferredCase="formInfo")
    public abstract FormInfo getforminfo();

    /**
     * Returns all validation messages that are global, i.e. not bound to a specific field
     */
    @CodeCompletion(preferredCase="globalMessages",isProperty=true)
    public List<String> getglobalmessages();

    /**
     * Lists names of fields with validation errors
     */
    @CodeCompletion(preferredCase="invalidFields")
    public abstract List<Object>  getinvalidfields();

    /**
     * Returns the validation message of a field with failed validation
     * @param field Name of the field
     */
    @CodeCompletion(preferredCase="getMessage")
    public abstract String getmessage(String field);

    /**
     * Lists all current validation messages
     */
    @CodeCompletion(preferredCase="messages",isProperty=true)
    public abstract List<String> getmessages();

    /**
     * Provides a ProcessContext to store arbitrary data for the form process 
     */
    @CodeCompletion(preferredCase="processContext",isProperty=true)
    public abstract TMLFormProcessContext getprocesscontext();

    /**
     * Returns the context for which this form was instantiated. This may influence the data source of the form.
     */
    @CodeCompletion(preferredCase="targetContext",isProperty=true)
    public Context gettargetcontext();

    /**
     * Tests if the form contains a field of the given name
     * @param name Name of the field
     */
    @CodeCompletion(preferredCase="hasField")
    public abstract boolean hasfield(String name);

    /**
     * Tests if a field specific validation message exists for the given field
     * @param field Name of the field
     */
    @CodeCompletion(preferredCase="hasMessage")
    public abstract boolean hasmessage(String field);

    /**
     *  Tests if any validation messages exist
     */
    @CodeCompletion(preferredCase="hasMessages")
    public abstract boolean hasmessages();

    /**
     * Returns if this form is currently in edit mode 
     */
    @CodeCompletion(preferredCase="editable",isProperty=true)
    public abstract boolean iseditable();

    /**
     * Indicates if an uploaded file has been dropped because of its size 
     */
    @CodeCompletion(preferredCase="filesDropped",isProperty=true)
    public abstract boolean isfilesdropped();

    /**
     * Shows if the form is persistent
     */
    @CodeCompletion(preferredCase="persistent",isProperty=true)
    public abstract boolean ispersistent();

    /**
     * Shows if the form was submitted from a previous request
     */
    @CodeCompletion(preferredCase="submitted",isProperty=true)
    public abstract boolean issubmitted();

    /**
     * Returns the display mode of the WebTML form
     */
    @CodeCompletion
    public abstract String mode();

    /**
     * Returns the processed value of the field in its target type
     * @param fieldname Name of the field
     */
    @CodeCompletion(preferredCase="parsedValue")
    public abstract Object parsedvalue(String fieldname);

    /**
     * Removes the current form from form registry 
     */
    @CodeCompletion
    public abstract void remove();

    /**
     * Removes a field from the form and its data 
     * @param name Name of the field
     */
    @CodeCompletion(preferredCase="removeField")
    public abstract void removefield(String name);

    /**
     * Removes a file from the form
     * @param filename Name of the file
     * @return true, if a file was removed, false if no file of the given name was found 
     */
    @CodeCompletion(preferredCase="removeFile")
    public abstract boolean removefile(String filename);

    /**
     * Clears all fields and uploaded files from the form, resetting it completely
     */
    @CodeCompletion
    public abstract void reset() throws WGAPIException;

    /**
     * Sets the value of a field
     * @param name Name of the field
     * @param value Value of the field
     * @throws WGException
     */
    @CodeCompletion(preferredCase="setField")
    public abstract void setfield(String name, Object value) throws WGException;

    /**
     * Adds a new validation message related to the specified input field
     * @param fieldName The field for which the message is determined
     * @param message The validation error message
     */
    @CodeCompletion(preferredCase="setMessage")
    public abstract void setmessage(String fieldName, String message);

    /**
     * Returns the source type of the form
     */
    @CodeCompletion
    public abstract String source();

    /**
     * Stores the form data to the source of the form
     * @return true if the store operation succeeded, false if it didn't because of failed validations  
     * @throws WGException
     */
    @CodeCompletion
    public abstract boolean store() throws WGException;

    /**
     * Stores a single form field to a document. Uses the field name as item name.
     * @param fieldName The name of the field
     * @param doc The document to store the field to
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="storeField")
    public abstract void storefield(String fieldName, WGDocument doc) throws WGAPIException;

    /**
     * Stores a single form field to a document
     * @param fieldName The name of the field
     * @param doc The document to store the field to
     * @param targetName The name of the item to store this field to.
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="storeField")
    public abstract void storefield(String fieldName, WGDocument doc, String targetName) throws WGAPIException;

    /**
     * Stores the fields of the form to the content document on the forms target context
     * @return true if validation succeeded, false if not 
     * @throws WGException
     * @throws IOException
     */
    @CodeCompletion(preferredCase="storeInContent")
    public abstract boolean storeincontent() throws WGException, IOException;

    /**
     * Stores the fields of the form to a content document
     * @param content The document to store the fields to
     * @return true if validation succeeded, false if not 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="storeInContent")
    public abstract boolean storeincontent(WGContent content) throws WGException;

    /**
     * Stores the form data into the configuration of the current portlet of the WebTML environment
     * @return true if validation succeeded, false if not 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="storeInPortlet")
    public abstract boolean storeinportlet() throws WGException;

    /**
     * Stores the form data into the a portlet configuration
     * @param portlet The portlet to store the configuration to
     * @return true if validation succeeded, false if not 
     * @throws WGException
     */
    @CodeCompletion(preferredCase="storeInPortlet")
    public abstract boolean storeinportlet(Portlet portlet) throws WGException;
    
    /**
     * Stores the form data on the users personalisation profile
     * @return true if validation succeeded, false if not
     * @throws WGException
     */
    @CodeCompletion(preferredCase="storeInProfile")
    public abstract boolean storeinprofile() throws WGException;
    
    /**
     * Manually triggers the form validation
     * @return true if validation succeeded, false otherwise
     */
    @CodeCompletion
    public abstract boolean validate();
    
    /**
     * Returns if a field is empty.
     * This method also respects the field registration and is able to detect empty relation fields, which is not reliably possible via a simple check for an empty field value.
     * @param fieldName Then name of the field
     */
    @CodeCompletion(preferredCase="isEmpty")
    public boolean isempty(String fieldName);

}