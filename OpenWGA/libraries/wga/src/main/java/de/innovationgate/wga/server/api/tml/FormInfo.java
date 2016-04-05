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

import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.Design;

/**
 * FormInfo is an object holding information to create a TMLForm object from TMLScript.
 * This object is the same as the FormInfo object in TMLScript. Documentation on this object and its methods in more detail can therefor be found on the TMLScript reference of the OpenWGA documenation library of the respective OpenWGA version for Object "FormInfo".
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public interface FormInfo {

    /**
     * Returns the content class of documents created and edited with this form.
     */
    @CodeCompletion(preferredCase="contentClass",isProperty=true)
    public abstract String getContentClass();

    /**
     * Returns the ID of the WebTML form
     */
    @CodeCompletion(preferredCase="formID",isProperty=true)
    public abstract String getFormId();

    /**
     * Returns the display mode of the WebTML form
     */
    @CodeCompletion(preferredCase="mode",isProperty=true)
    public abstract String getMode();

    /**
     * Returns the data source of the WebTML form
     */
    @CodeCompletion(preferredCase="source",isProperty=true)
    public abstract String getSource();

    /**
     * Returns if the created form will accept plain HTML input fields
     */
    @CodeCompletion(preferredCase="htmlInput",isProperty=true)
    public abstract boolean isHtmlInput();

    /**
     * Returns if the created form is to be stored persistently on the user session
     */
    @CodeCompletion(preferredCase="persistent",isProperty=true)
    public abstract boolean isPersistent();

    /**
     * Returns if the created form is to automatically trim field contents
     */
    @CodeCompletion(preferredCase="trim",isProperty=true)
    public abstract boolean isTrim();

    /**
     * Sets the content class of documents created and edited with this form.
     * @param contentclass The content class
     */
    @CodeCompletion(preferredCase="contentClass",isProperty=true)
    public abstract void setContentClass(String contentclass);

    /**
     * Sets the ID of the WebTML form. Only effective before creating a form.
     * @param formId The form id
     */
    @CodeCompletion(preferredCase="formID",isProperty=true)
    public abstract void setFormId(String formId);

    /**
     * Sets the display mode of the WebTML form
     * @param mode The mode
     * @throws IllegalArgumentException
     */
    @CodeCompletion(preferredCase="mode",isProperty=true)
    public abstract void setMode(String mode) throws IllegalArgumentException;

    /**
     * Sets the data source of the WebTML form
     * @param source The source name
     */
    @CodeCompletion(preferredCase="source",isProperty=true)
    public abstract void setSource(String source);

    /**
     * Sets if the created form is to automatically trim field contents
     */
    @CodeCompletion(preferredCase="trim",isProperty=true)
    public abstract void setTrim(boolean trim);

    /**
     * Sets the form definition information to the base reference of the given design 
     */
    public abstract void setDefinition(Design design);

}