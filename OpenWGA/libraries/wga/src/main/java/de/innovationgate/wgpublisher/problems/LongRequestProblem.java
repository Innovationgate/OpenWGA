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

import de.innovationgate.wgpublisher.log.WGARequestInformation;

public class LongRequestProblem extends Problem implements AdditiveProblem<LongRequestProblem> {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private WGARequestInformation _longestRequestInfo;
    private long _longestDuration;
    private long _timesExceedingDuration;
    private String _uri;
    private String _url;
    private String _host;
    private Date _lastExceeding;

    public LongRequestProblem(ProblemPath path, ProblemText text, ProblemSeverity severity, ProblemOccasion occasion, Throwable throwable, List<MessageVariableProvider> providers) {
        super(path, text, severity, occasion, throwable, providers);
        _longestRequestInfo = (WGARequestInformation) getVariable("reqinfo");
        _uri = (String) getVariable("uri");
        _url = (String) getVariable("completeurl");
        _host = (String) getVariable("host");
        _longestDuration = _longestRequestInfo.getEndTime() - _longestRequestInfo.getStartTime();
        _timesExceedingDuration = 1;
        _lastExceeding = new Date();
    }

    @Override
    public void addProblem(LongRequestProblem p) {
        
        WGARequestInformation reqInfo = p.getLongestRequestInfo(); 
        long duration = _longestRequestInfo.getEndTime() - _longestRequestInfo.getStartTime();
        if (duration > _longestDuration) {
            _longestDuration = duration;
            _longestRequestInfo = reqInfo;
        }
        _timesExceedingDuration++;
        _lastExceeding = new Date();
        
    }

    public WGARequestInformation getLongestRequestInfo() {
        return _longestRequestInfo;
    }

    public long getLongestDuration() {
        return _longestDuration;
    }

    public long getTimesExceedingDuration() {
        return _timesExceedingDuration;
    }
    
    public Date getLastExceeding() {
        return _lastExceeding;
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
