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

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;

public class DisplaySizeQueryTermProcessor implements DerivateQueryTermProcessor {

    @Override
    public float matchTermToFileAnnotations(WGFileAnnotations fileAnnotations, DerivateQueryTerm term, ClientHints clientHints) throws WGException {
        
        
        int requestedDimension;
        try {
            requestedDimension = Integer.parseInt(term.getValue());
        }
        catch (NumberFormatException e) {
            throw new WGInvalidDerivateQueryException("Value for term '" + term.getName() + "' not parseable as integer: " + term.getValue());
        }
        
        if (clientHints.getDevicePixelRatio() != null) {
            requestedDimension *= clientHints.getDevicePixelRatio();
        }
        
        int fileDimension;
        if (term.getName().equals(DerivateQuery.QUERYTERM_WIDTH)) {
            fileDimension = fileAnnotations.getDisplayWidth();
        }
        else {
            fileDimension = fileAnnotations.getDisplayHeight();
        }
        
        if (term.getOperator().equals("=")) {
            if (fileDimension == requestedDimension) {
                return 1;
            }
            else {
                return 0;
            }
        }
        else if (term.getOperator().equals("~")) {
            return testAboutEqual(fileDimension, requestedDimension,true, fileAnnotations instanceof WGFileMetaData);
        }
        else if (term.getOperator().equals(">=")) {
            return testBiggerOrEqual(fileDimension, requestedDimension, true);
        }
        else if (term.getOperator().equals("<=")) {
            return testLowerOrEqual(fileDimension, requestedDimension, true);
        }
        else if (term.getOperator().equals(">")) {
            return testBiggerOrEqual(fileDimension, requestedDimension, false);
        }
        else if (term.getOperator().equals("<")) {
            return testLowerOrEqual(fileDimension, requestedDimension, false);
        }
        else {
            throw new WGInvalidDerivateQueryException("Invalid operator: " + term.getOperator());
        }

        
    }

    private float testAboutEqual(int fileDimension, int requestedDimension, boolean allowEqual, boolean isOriginal) {
        if (requestedDimension == fileDimension) {
            return (allowEqual ? 1 : 0);
        }
        
        // Even smaller pics are allowed, but at a much smaller rate
        if (requestedDimension > fileDimension) {
            return (float) 1 / ((requestedDimension - fileDimension) + 10000);
        }
        
        return (float) 100 / (fileDimension - requestedDimension + 100);
    }

    private float testLowerOrEqual(int fileDimension, int requestedDimension, boolean allowEqual) {
        if (requestedDimension == fileDimension) {
            return (allowEqual ? 1 : 0);
        }
        
        if (requestedDimension < fileDimension) {
            return 0;
        }
        
        return (float) fileDimension / requestedDimension;
    }

    private float testBiggerOrEqual(int fileDimension, int requestedDimension, boolean allowEqual) {
        if (requestedDimension == fileDimension) {
            return (allowEqual ? 1 : 0);
        }
        
        if (requestedDimension > fileDimension) {
            return 0;
        }
        
        return (float) 100 / (fileDimension - requestedDimension + 100);

    }

}
