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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.builder.CompareToBuilder;
import org.apache.log4j.Logger;

import de.innovationgate.utils.security.HashingException;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.RequestAwareAuthenticationModule;
import de.innovationgate.wga.config.Administrator;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cluster.tasks.ClearFailedLoginAttemptsTask;
import de.innovationgate.wgpublisher.cluster.tasks.DistributeFailedLoginAttemptInformationTask;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.mail.WGAMailNotification;

public class BruteForceLoginBlocker {
    
    public static final Logger LOG = Logger.getLogger("wga.api.auth");
    
    private WGACore _core;

    public BruteForceLoginBlocker(WGACore core) {
        _core = core;
    }
    
    public static final String DOMAIN_ADMINLOGINS = WGACore.DOMAIN_ADMINLOGINS;
    
    private Map<String, LoginAttemptInformation> _failedLoginAttempts = new ConcurrentHashMap<String, LoginAttemptInformation>();

    public class LoginAttemptsComparator implements Comparator {

        public int compare(Object arg0, Object arg1) {

            LoginAttemptInformation inf1 = (LoginAttemptInformation) arg0;
            LoginAttemptInformation inf2 = (LoginAttemptInformation) arg1;
            
            return new CompareToBuilder().append(inf1.getDomain(), inf2.getDomain()).append(inf1.getName(), inf2.getName()).toComparison();
            
        }
        
    }
    

    public void clearAllFailedLoginAttempts() {
        clearAllFailedLoginAttempts(true);
    }
    
    public void clearAllFailedLoginAttempts(boolean distribute) {
        _failedLoginAttempts.clear();
        if (distribute) {
            try {
                _core.getClusterService().submitToOthers(new ClearFailedLoginAttemptsTask());
            } catch (Throwable e) {
                _core.getLog().error("Failed to distribute clearFailedLoginAttempts", e);
            }        
        }
    }
    
    public void clearFailedLoginAttempts(String domain, String user) {
        clearFailedLoginAttempts(domain, user, true);
    }
    
    public void clearFailedLoginAttempts(String domain, String user, boolean distribute) {
        _failedLoginAttempts.remove(LoginAttemptInformation.createLoginAttemptKey(domain, user));
        if (distribute) {
            try {
                _core.getClusterService().submitToOthers(new ClearFailedLoginAttemptsTask(domain, user));
            } catch (Throwable e) {
                _core.getLog().error("Failed to distribute clearFailedLoginAttempts", e);
            }
        }
    }
    
    /**
     * @return Returns the failedLoginAttempts.
     */
    public List getFailedLoginAttempts() {
        List list = new ArrayList(_failedLoginAttempts.values());
        Collections.sort(list, new LoginAttemptsComparator());
        return Collections.unmodifiableList(list);
    }
    
    /**
     * @return currently blocked logins
     */
    public List getBlockedLogins() {
        List failedLogins = getFailedLoginAttempts();
        Iterator loginsIt = failedLogins.iterator();
        LoginAttemptInformation inf;
        List blockedLogins = new ArrayList();
        while (loginsIt.hasNext()) {
            inf = (LoginAttemptInformation) loginsIt.next();
            // add only blocked logins to list
            if (!inf.isBlocked()) {
                continue;
            } else {
                blockedLogins.add(inf);
            }
        }
        Collections.sort(blockedLogins, new LoginAttemptsComparator());
        return Collections.unmodifiableList(blockedLogins);
    }

    
    public LoginAttemptInformation getLoginAttemptInformation(String domain, String user) {
        return (LoginAttemptInformation) _failedLoginAttempts.get(LoginAttemptInformation.createLoginAttemptKey(domain, user));
    }
    
    public AuthenticationSession login(WGADomain domain, String username, Object credentials) throws AuthenticationException {
    	return login(domain, username, credentials, null);
    }
    
    public AuthenticationSession login(WGADomain domain, String username, Object credentials, HttpServletRequest request) throws AuthenticationException {
        
    	String ip=null;
        if(request!=null)
        	ip = (String)request.getAttribute(WGAFilter.REQATTRIB_ORIGINAL_IP);

        LoginAttemptInformation inf = getLoginAttemptInformation(domain.getName(), username);
        if (inf != null && inf.isBlocked()) {
            LOG.warn("Login" 
            		+ (ip!=null ? " from IP " + ip : "")
            		+ " for username '" + username + "' is blocked because of too many wrong login attempts (Brute force login blocker on domain " + domain.getName() + ")");
            return null;
        }
        
        AuthenticationModule auth = domain.getAuthModule();
        AuthenticationSession authSession;
        if(auth instanceof RequestAwareAuthenticationModule)
        	authSession = ((RequestAwareAuthenticationModule)auth).login(username, credentials, request);
        else authSession = auth.login(username, credentials);
        if (authSession != null) {
            if (inf != null) {
                inf.reset();
            }
            return authSession;
        }
       
        if (inf == null) {
            inf = new LoginAttemptInformation(domain.getName(), username, domain.getConfig().getMaximumLoginAttempts());
            inf.map(_failedLoginAttempts);
        }
        inf.addFailedAttempt(ip);
        
        if (inf.isBlocked()) {
            try {
                // notify cluster members about blocked login
                _core.getClusterService().submitToOthers(new DistributeFailedLoginAttemptInformationTask(inf));
            } catch (Throwable e) {
                _core.getLog().error("Distribution of blocked login failed." , e);
            }
            
        	WGAMailNotification mail = new WGAMailNotification(WGAMailNotification.TYPE_LOGIN_BLOCKED);
        	mail.setSubject("Login has been blocked");
        	mail.append("<p>Login for user <b>" + username + "</b>"
        			+ " has been blocked in authentication domain <b>" + domain.getName() + "</b> because of " + inf.getFailedAttempts() + " failed login attemps.</p>");
        	if(inf.getIps().size()>0){
        		mail.append("<p>Logins has been requested from the following IP(s):</p>");
        		mail.append("<ul>");
        		for(String requested_from_ip: inf.getIps())
        			mail.append("<li>" + requested_from_ip + "</li>");
        		mail.append("</ul>");
        	}
        	_core.send(mail);
        }
        
        return null;
 
    }
    
    public int login(WGDatabase db, String username, Object credentials) throws WGAPIException {
        return login(db, username, credentials, null, null);
    }
    
    public int login(WGDatabase db, String username, Object credentials, String filter) throws WGAPIException {
    	return login(db, username, credentials, filter, null);
    }
    
    public int login(WGDatabase db, String username, Object credentials, HttpServletRequest request) throws WGAPIException {
    	return login(db, username, credentials, null, request);
    }
    
    public int login(WGDatabase db, String username, Object credentials, String filter, HttpServletRequest request) throws WGAPIException {

    	String ip=null;
        if(request!=null)
        	ip = (String)request.getAttribute(WGAFilter.REQATTRIB_ORIGINAL_IP);

        String domainName = (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN);
        WGADomain domain = _core.getDomains(domainName);
        LoginAttemptInformation inf = getLoginAttemptInformation(domainName, username);
        if (inf != null && inf.isBlocked()) {
            LOG.warn("Failed login for '" + username + "': Username is blocked because of too many wrong login attempts (Brute force login blocker on database " + db.getDbReference() + ")");
            return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
        }
        
        int level = (db.isSessionOpen() ? db.reopenSession(username, credentials, filter) : db.openSession(username, credentials, filter));
         
        if (level == WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
            if (inf == null) {
                inf = new LoginAttemptInformation(domainName, username, domain.getConfig().getMaximumLoginAttempts());
                inf.map(_failedLoginAttempts);
            }
            inf.addFailedAttempt(ip);
            
            if (inf.isBlocked()) {
                try {
                    // notify cluster members about blocked login
                    _core.getClusterService().submitToOthers(new DistributeFailedLoginAttemptInformationTask(inf));
                } catch (Throwable e) {
                    _core.getLog().error("Distribution of blocked login failed." , e);
                }
                
            	WGAMailNotification mail = new WGAMailNotification(WGAMailNotification.TYPE_LOGIN_BLOCKED);
            	
            	mail.setSubject("Login has been blocked");
            	
            	mail.append("Username: " + username);
            	mail.append("<br>Domain: " + domainName);
            	mail.append("<br>");
            	mail.append("<br>Failed Login-Attempts: " + inf.getFailedAttempts());
            	mail.append("<br>Last Login-Application-Key: " + db.getDbReference());
            	mail.append("<br>Last Used Credentials (Password): " + credentials);

            	if(inf.getIps().size()>0){
            		mail.append("<p>Logins has been requested from the following IP(s):</p>");
            		mail.append("<ul>");
            		for(String requested_from_ip: inf.getIps())
            			mail.append("<li>" + requested_from_ip + "</li>");
            		mail.append("</ul>");
            	}

            	_core.send(mail);
            }
        }
        else if (inf != null) {
            inf.reset();
        }
        
        return level;
        
    }
    
    public boolean adminLogin(String username, String password, Administrator admin, HttpServletRequest request) {
        
    	String ip=null;
        if(request!=null)
        	ip = (String)request.getAttribute(WGAFilter.REQATTRIB_ORIGINAL_IP);

        LoginAttemptInformation inf = getLoginAttemptInformation(DOMAIN_ADMINLOGINS, username);
        if (inf != null && inf.isBlocked()) {
            LOG.warn("Admin-Login"
            		+ (ip!=null ? " from IP " + ip : "")
            		+ " for"
            		+ (admin==null ? " (not existing)" : "")
            		+ " account '" + username + "' is blocked because of too many wrong login attempts (Brute force login blocker for admin logins)");
            return false;
        }
        
        try {
			if (admin != null && admin.isPasswordCorrect(password, _core.getModuleRegistry())) {
			    if (inf != null) {
			        inf.reset();
			    }
			    return true;
			}
		} catch (HashingException e) {
			_core.getLog().fatal("Cannot test administrative password because of an error in the hashing process", e);
			return false;
		}
        
        if (inf == null) {
            inf = new LoginAttemptInformation(DOMAIN_ADMINLOGINS, username, LoginAttemptInformation.DEFAULT_MAX_FAILED_ATTEMPTS);
            inf.map(_failedLoginAttempts);
        }
        inf.addFailedAttempt(ip);
        
        if (inf.isBlocked()) {
            try {
                // notify cluster members about blocked login
                _core.getClusterService().submitToOthers(new DistributeFailedLoginAttemptInformationTask(inf));
            } catch (Throwable e) {
                _core.getLog().error("Distribution of blocked login failed." , e);
            }
            
        	WGAMailNotification mail = new WGAMailNotification(WGAMailNotification.TYPE_LOGIN_BLOCKED);
        	mail.setSubject("OpenWGA Administrator login '" + username + "' has been blocked.");
        	mail.append("<p>OpenWGA Administrator login <b>'" + username + "'</b> has been blocked bc. of <b>'" + inf.getFailedAttempts() + "'</b> failed login attemps.</p>");
        	if(admin==null)
        		mail.append("<p><b>This account does not exist but has been blocked anyway.</b></p>");
        	if(inf.getIps().size()>0){
        		mail.append("<p>Logins has been requested from the following IP(s):</p>");
        		mail.append("<ul>");
        		for(String requested_from_ip: inf.getIps())
        			mail.append("<li>" + requested_from_ip + "</li>");
        		mail.append("</ul>");
        	}
        	_core.send(mail);
        }
        
        return false;
        
    }
    
    public boolean isLoginBlocked(String domain, String user) {
        
        LoginAttemptInformation info = getLoginAttemptInformation(domain, user);
        if (info != null && info.isBlocked()) {
            return true;
        }
        else {
            return false;
        }
        
    }

    public void map(LoginAttemptInformation inf) {
        inf.map(_failedLoginAttempts);        
    }

}
