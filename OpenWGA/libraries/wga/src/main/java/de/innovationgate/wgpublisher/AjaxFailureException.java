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

package de.innovationgate.wgpublisher;

import de.innovationgate.webgate.api.WGException;

/**
 * Thrown when an AJAX request should be canceled and the user urged to reload the page
 */
public class AjaxFailureException extends WGException {
    
    public static final int AJAXTYPE_ACTION = 1;
    public static final int AJAXTYPE_FORMPOST = 2;

    private String _labelKey;
    private int _ajaxType;
    
    public String getLabelKey() {
        return _labelKey;
    }

    public AjaxFailureException(String labelKey, int type) {
        super((String) null);
        _labelKey = labelKey;
        _ajaxType = type;
    }

    public AjaxFailureException(String labelKey, int type, Throwable cause) {
        super(cause.getMessage(), cause);
        _labelKey = labelKey;
        _ajaxType = type;
    }

    public AjaxFailureException(String labelKey, int type, String msg, Throwable cause) {
        super(msg, cause);
        _labelKey = labelKey;
        _ajaxType = type;
    }
    
    @Override
    public String getMessage() {
        String msg = super.getMessage();
        if (msg != null) {
            return _labelKey + ": " + msg;
        }
        else {
            return _labelKey;
        }
    }
    
    public String getDetailMessage() {
        return super.getMessage();
    }

    public int getAjaxType() {
        return _ajaxType;
    }

}
