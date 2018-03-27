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

import java.util.Date;
import java.util.List;

public class VirtualHostDenialProblem extends Problem implements AdditiveProblem<Problem> {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Date _lastCall;
    private long _calls;
    private String _uri;
    private String _url;
    private String _host;

    public VirtualHostDenialProblem(ProblemPath problemPath, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<MessageVariableProvider> providers) {
        super(problemPath, text, severity, occasion, throwable, providers);
        _lastCall = new Date();
        _calls = 1;
        _uri = (String) getVariable("uri");
        _url = (String) getVariable("completeurl");
        _host = (String) getVariable("host");
    }
    

    @Override
    public void addProblem(Problem p) {
        _calls++;
        _lastCall = new Date();
    }

    public Date getLastCall() {
        return _lastCall;
    }

    public long getCalls() {
        return _calls;
    }

    public String getUri() {
        return _uri;
    }

    public String getUrl() {
        return _url;
    }

    public String getHost() {
        return _host;
    }

}
