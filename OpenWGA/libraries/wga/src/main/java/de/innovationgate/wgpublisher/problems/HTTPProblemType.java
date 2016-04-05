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

import java.util.List;
import java.util.Locale;

import de.innovationgate.wgpublisher.WGPDispatcher;

public class HTTPProblemType implements ProblemType, UseSpecialProblemImplementation {

    @Override
    public String getTitle(Locale l) {
        return "HTTP Problem Type";
    }

    @Override
    public String getDescription(Locale l) {
        return "URL Paths which are called on this OpenWGA server but could not be served";
    }

    @Override
    public Problem createProblem(ProblemPath problemPath, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<MessageVariableProvider> providers) {
        
        String key = problemPath.getKey();
        
        if (key.startsWith(WGPDispatcher.class.getName() + ".dispatching.http404#")) {
            return new HTTP404Problem(problemPath, text, severity, occasion, throwable, providers);
        }
        else if (key.startsWith(WGPDispatcher.class.getName() + ".dispatching.vhostdenial#")) {
            return new VirtualHostDenialProblem(problemPath, text, severity, occasion, throwable, providers);
        }
        else {
            throw new IllegalArgumentException("Problem key starts with no known problem identifier: " + key);
        }
            
    }


}
