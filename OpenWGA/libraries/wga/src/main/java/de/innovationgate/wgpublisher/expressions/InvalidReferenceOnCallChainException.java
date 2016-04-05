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

package de.innovationgate.wgpublisher.expressions;

import de.innovationgate.webgate.api.WGExpressionException;

public class InvalidReferenceOnCallChainException extends WGExpressionException {
    
    private String _property;
    private String _callChain;
    private String _object;

    public InvalidReferenceOnCallChainException(String property, String callChain, String object) {
        super("Method/property '" + property + "' returned null or undefined on call chain '" + callChain + "' for object '" + object + "' although the call chain is incomplete", "(No code)");
        _property = property;
        _callChain = callChain;
        _object = object;
    }

    public String getProperty() {
        return _property;
    }

    public String getCallChain() {
        return _callChain;
    }

    public String getObject() {
        return _object;
    }

}
