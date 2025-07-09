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

package de.innovationgate.wgpublisher.auth;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import org.apache.log4j.Logger;

public class LoginAttemptInformation implements Serializable {
    
    private static final long serialVersionUID = 1L;
    public static final Logger LOG = Logger.getLogger("wga.api.auth");
    
    public static String createLoginAttemptKey(String domain, String user) {
        return domain + "/" + user;
    }
    
    public static final int DEFAULT_MAX_FAILED_ATTEMPTS = 5;
    public static final int BLOCKED_MINUTES = 30;
    private String _domain;
    private String _name;
    private int _failedAttempts = 0;
    private boolean _blocked = false;
    private Date _blockedDate;
    private int _maxAttempts;
    private int _loginBlockMinutes;
    
    private ArrayList<String> _ips = new ArrayList<String>();
    
    public LoginAttemptInformation(String domain, String name, int maxAttempts, int loginBlockMinutes) {
        _domain = domain;
        _name = name;
        _maxAttempts = maxAttempts;
        _loginBlockMinutes = loginBlockMinutes;
    }
    
    /**
     * @return Returns the list of ip-s
     */
    public ArrayList<String> getIps(){
    	return _ips;
    }
    /**
     * @return Returns the failedAttempts.
     */
    public int getFailedAttempts() {
        return _failedAttempts;
    }
    /**
     * @return Returns the name.
     */
    public String getName() {
        return _name;
    }
    
    public void addFailedAttempt(String ip) {
        _failedAttempts++;
        if(ip!=null && !_ips.contains(ip))
        	_ips.add(ip);
        if (_maxAttempts > 0 && _failedAttempts >= _maxAttempts) {
            _blocked = true;
            _blockedDate = new Date();
            LOG.warn("Blocked login"
            	+ (_ips.size()>0 ? " from IPs " + _ips : "")
            	+ " for user '" + getName() + "' in domain '" + getDomain() + "' because of " + _maxAttempts + " failed login attempts");
        }
    }
    
    public int getLoginBlockMinutes() {
    	return _loginBlockMinutes;
    }

    /**
     * @return Returns the domain.
     */
    public String getDomain() {
        return _domain;
    }
    public void map(Map<String,LoginAttemptInformation> failedLoginAttempts) {
        failedLoginAttempts.put(createLoginAttemptKey(getDomain(), getName()), this);
    }
    /**
     * @return Returns the blocked.
     */
    public boolean isBlocked() {
    	if(_blocked && _blockedDate!=null && _loginBlockMinutes>0){
    		// check if blocked date is older then _loginBlockMinutes minutes reset state.
    		long now = System.currentTimeMillis();
    		if(now - _blockedDate.getTime() > 1000*60*_loginBlockMinutes)
    			reset();
    	}
        return _blocked;
    }
    /**
     * @return Returns the blockedDate.
     */
    public Date getBlockedDate() {
        return _blockedDate;
    }
    public void reset() {
        _blocked = false;
        _blockedDate = null;
        _failedAttempts = 0;
        _ips.clear();
    }
    
    
}
