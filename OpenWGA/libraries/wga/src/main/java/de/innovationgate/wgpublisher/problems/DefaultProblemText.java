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

package de.innovationgate.wgpublisher.problems;

import java.util.Locale;

public class DefaultProblemText implements ProblemText {
    
    private String _title;
    private String _description;
    private String _solution = null;
    private String _message;
    
    public DefaultProblemText(String title, String description) {
        _title = title;
        _description = description;
    }

    public DefaultProblemText(String title, String message, String description, String solution) {
        _title = title;
        _message = message;
        _description = description;
        _solution = solution;
    }

    @Override
    public String getTitle(Locale l) {
        return _title;
    }

    @Override
    public String getDescription(Locale l) {
        return _description;
    }

    @Override
    public String getSolution(Locale l) {
        return _solution;
    }

    public String getMessage(Locale l) {
        return _message;
    }

}
