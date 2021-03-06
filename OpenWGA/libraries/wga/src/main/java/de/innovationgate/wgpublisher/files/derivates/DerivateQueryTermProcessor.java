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

package de.innovationgate.wgpublisher.files.derivates;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;

/**
 * A processor of a derivate query term for a special name. 
 */
public interface DerivateQueryTermProcessor {
    
    /**
     * Evaluates how much given file annotations match the terms conditions. Return value 1 is perfect match, 0 is no match at all. Values inbetween represent partial matches.
     * @param fileAnnotations The annotations
     * @param term The term
     * @return The matching value
     * @throws WGException 
     */
    public float matchTermToFileAnnotations(WGFileAnnotations fileAnnotations, DerivateQueryTerm term, ClientHints hints) throws WGException;
    
}
