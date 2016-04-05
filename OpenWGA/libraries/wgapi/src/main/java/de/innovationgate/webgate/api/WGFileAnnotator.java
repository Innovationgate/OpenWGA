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

package de.innovationgate.webgate.api;


import javax.activation.DataSource;

/**
 * An annotator for attached files, to analyze them and add information to the files annotations 
 */
public interface WGFileAnnotator {
    
    /**
     * Method annotating the file of the given data
     * @param originalfileData The file data data source, able to serve multiple input streams. Does not provide specific mime type information (always returns "application/octet-stream"). Only provides name information if the file data originates from a regular attachment (not a derivate).
     * @param fileAnnotations The annotations object of the file, where information is to be added
     */
    public void annotateFile(DataSource originalfileData, WGFileAnnotations fileAnnotations) throws WGAPIException;
    
    /**
     * Returns a number which is used by OpenWGA to determine the order in which annotators are to be executed. Annotators are executed in ascending order of the returned order nr
     */
    public int getOrderNr();

}
