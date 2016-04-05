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

import java.util.List;

import javax.activation.MimeType;
import javax.activation.MimeTypeParseException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;

public class TypeQueryTermProcessor implements DerivateQueryTermProcessor {

    @Override
    public float matchTermToFileAnnotations(WGFileAnnotations fileAnnotations, DerivateQueryTerm term, ClientHints clientHints) throws WGException {
        String derivateMimeType = fileAnnotations.getMimeType();
        String requestedMimeType = term.getValue();
        return testMimeTypeMatch(derivateMimeType, requestedMimeType);
    }

    public static float testMimeTypeMatch(String fileMime, String queryMime) {
        try {
            MimeType mime = new MimeType(fileMime);
            List<String> requestedMimeParts = WGUtils.deserializeCollection(queryMime, "/");
            if (requestedMimeParts.size() == 1 || (requestedMimeParts.size() == 2 && requestedMimeParts.get(1).equals("*"))) {
                if (mime.getPrimaryType().equals(requestedMimeParts.get(0))) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
            else {
                if (mime.getPrimaryType().equals(requestedMimeParts.get(0)) && mime.getSubType().equals(requestedMimeParts.get(1))) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
        }
        catch (MimeTypeParseException e) {
            return 0;
        }
    }

}
