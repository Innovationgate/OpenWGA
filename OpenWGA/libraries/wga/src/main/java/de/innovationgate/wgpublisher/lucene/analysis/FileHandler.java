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
package de.innovationgate.wgpublisher.lucene.analysis;

import java.io.InputStream;
/**
 * interface for lucene filehandler implementations
 * filehandlers are mapped to fileextensions and used by LuceneManager to extract 
 * text of fileattachments from content-documents
 * 
 */
public interface FileHandler {
    
    /**
     * parses the given inputstream, for e.g. a pdf document
     * @param is
     * @throws FileHandlerException when the document cannot be parsed
     */
    public void parse(InputStream is) throws FileHandlerException ;
    
    /**
     * @return the text from the parsed document
     */
    public String getText();

}
