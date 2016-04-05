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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.map.LRUMap;
import org.apache.commons.collections.map.LinkedMap;
import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class WGAUsageStatistics {
    
    public class UpdateStatisticsThread extends Thread {
        
        private volatile boolean _canceled = false;
        
        public void run() {

            Thread.currentThread().setName("OpenWGA Usage Statistics Update Thread");
            
            while (true) {
                
                try {
                    
                    Thread.sleep(3000);
                
                    while (true) {
                        RequestStatistic stat = _requestStatisticQueue.poll();
                        if (stat != null) {
                            processRequestStatistic(stat);
                        }
                        else {
                            break;
                        }
               
                    }
                
                }
                catch (Throwable e) {
                    _wgaCore.getLog().error("Exception processing request statistics. Statistic queue is cleared to prevent overflow", e);
                    _requestStatisticQueue.clear();
                }
            
            
                if (_canceled) {
                    break;
                }
            
            }
            
        }

        public boolean isCanceled() {
            return _canceled;
        }

        public void setCanceled(boolean canceled) {
            _canceled = canceled;
        }

        private void processFilteredRequest(RequestStatistic reqStat) {
            
            String userAgent = reqStat.getUserAgent();
            if (userAgent == null) {
                userAgent = "(Empty user agent)";
            }
            
            Long requests = _filteredRequestsMap.get(userAgent);
            if (requests == null) {
                _filteredRequestsMap.put(userAgent, new Long(1));
            }
            else {
                _filteredRequestsMap.put(userAgent, new Long(++requests));
            }
            
        }

        private synchronized void processRequestStatistic(RequestStatistic reqStat) {
            
            // Look if we must filter this request out based on its user agent
            if (!_wgaCore.getUserAgentVerifier().isValidUserAgent(reqStat.getUserAgent())) {
                processFilteredRequest(reqStat);
                return;
            }
            
            _allRequests++;
        
            // Requests per hour
            Date date = new Date();
            calendar.setTime(date);
        
            String dayKey = DAY_KEY_FORMAT.format(date);
            Map<Integer, HourStatistic> requestsPerHour = (Map<Integer, HourStatistic>) _requestsPerDay.get(dayKey);
            if (requestsPerHour == null) {
                requestsPerHour = new LinkedMap();
                _requestsPerDay.put(dayKey, requestsPerHour);
            }
        
            Integer hourKey = new Integer(calendar.get(GregorianCalendar.HOUR_OF_DAY));
            HourStatistic requestsThisHour = requestsPerHour.get(hourKey);
            if (requestsThisHour == null) {
                requestsThisHour = new HourStatistic();
                requestsThisHour.increment();
                requestsPerHour.put(hourKey, requestsThisHour);
            }
            else {
                requestsThisHour.increment();
            }
        
            while (_requestsPerDay.size() > MAX_DAYS) {
                _requestsPerDay.remove(_requestsPerDay.firstKey());
            }
        
            // Session last access times
            if (!reqStat.isNewSession()) {
                SessionStatistic sessionStatistic = (SessionStatistic) _sessionAccess.get(reqStat.getSessionId());
                if (sessionStatistic == null) {
                    sessionStatistic = new SessionStatistic(reqStat);
                    _sessionAccess.put(reqStat.getSessionId(), sessionStatistic);
                }
                sessionStatistic.addRequest(reqStat);
            }
            
        }
        
    }

    public static class HourStatistic {
    
        private long _requests = 0;
    
        /**
         * @return Returns the requests.
         */
        public long getRequests() {
            return _requests;
        }
    
        public void increment() {
            _requests++;
        }
    
    }

    public static class RequestStatistic {
    
        private String _sessionId;
        
        private Date _sessionCreated;
        
        private String _task;
    
        private Date _lastAccess;
    
        private String _user;
    
        private String _profile;
        
        private String _userAgent;
        
        private String _remoteHost;
        
        private String _database;
        
        private boolean _newSession;
    
        /**
         * @return Returns the lastAccess.
         */
        public Date getLastAccess() {
            return _lastAccess;
        }
    
        /**
         * @param lastAccess
         *            The lastAccess to set.
         */
        public void setLastAccess(Date lastAccess) {
            _lastAccess = lastAccess;
        }
    
        /**
         * @return Returns the task.
         */
        public String getTask() {
            return _task;
        }
    
        /**
         * @param task
         *            The task to set.
         */
        public void setTask(String task) {
            _task = task;
        }
    
        /**
         * @return Returns the user.
         */
        public String getUser() {
            return _user;
        }
    
        /**
         * @param user
         *            The user to set.
         */
        public void setUser(String user) {
            _user = user;
        }
    
        /**
         * @return Returns the profile.
         */
        public String getProfile() {
            return _profile;
        }
    
        /**
         * @param profile
         *            The profile to set.
         */
        public void setProfile(String profile) {
            _profile = profile;
        }
    
        public String getUserAgent() {
            return _userAgent;
        }
    
        public void setUserAgent(String userAgent) {
            _userAgent = userAgent;
        }
    
        public String getRemoteHost() {
            return _remoteHost;
        }
    
        public void setRemoteHost(String remoteHost) {
            _remoteHost = remoteHost;
        }
    
        public String getDatabase() {
            return _database;
        }
    
        public void setDatabase(String database) {
            _database = database;
        }
    
        public String getSessionId() {
            return _sessionId;
        }
    
        public void setSessionId(String sessionId) {
            _sessionId = sessionId;
        }

        protected boolean isNewSession() {
            return _newSession;
        }

        protected void setNewSession(boolean newSession) {
            _newSession = newSession;
        }

        public Date getSessionCreated() {
            return _sessionCreated;
        }

        public void setSessionCreated(Date sessionCreated) {
            _sessionCreated = sessionCreated;
        }
        
    
    
    }

    
    public static class AccessData {
        
        private String _user;
        private String _profile;
        private String _task;
        private Date _lastAccess;
        private String _app;

        public AccessData(RequestStatistic stat) {
            
            _app = stat.getDatabase();
            _user = stat.getUser();
            _profile = stat.getProfile();
            _task = stat.getTask();
            _lastAccess = stat.getLastAccess();
            
        }

        public String getUser() {
            return _user;
        }

        public String getProfile() {
            return _profile;
        }

        public String getTask() {
            return _task;
        }

        public Date getLastAccess() {
            return _lastAccess;
        }

        public String getApp() {
            return _app;
        }
        
    }
    
    public static class SessionStatistic {
    
        private String _sessionId;
        
        private Date _sessionCreated;
        
        private String _task;
    
        private Date _lastAccess;
    
        private long _requests;
    
        private String _user;
    
        private String _profile;
        
        private String _userAgent;
        
        private String _remoteHost;
        
        private String _lastApp;
        
        private String _lastLogin = null;
        
        private String _lastLoginApp = null;
        
        private Map<String,AccessData> _databaseAccess = new HashMap<String,AccessData>();
        
        private SessionStatistic(RequestStatistic stat) {
            _sessionId = stat.getSessionId();
            _sessionCreated = stat.getSessionCreated();
        }
    
        /**
         * @return Returns the lastAccess.
         */
        public Date getLastAccess() {
            return _lastAccess;
        }
    
        /**
         * @param lastAccess
         *            The lastAccess to set.
         */
        public void setLastAccess(Date lastAccess) {
            _lastAccess = lastAccess;
        }
    
        /**
         * @return Returns the task.
         */
        public String getTask() {
            return _task;
        }
    
        /**
         * @param task
         *            The task to set.
         */
        public void setTask(String task) {
            _task = task;
        }
    
        /**
         * @return Returns the requests.
         */
        public long getRequests() {
            return _requests;
        }
    
        /**
         * @return Returns the user.
         */
        public String getUser() {
            return _user;
        }
    
        /**
         * @param user
         *            The user to set.
         */
        public void setUser(String user) {
            _user = user;
        }
    
        /**
         * @return Returns the profile.
         */
        public String getProfile() {
            return _profile;
        }
    
        /**
         * @param profile
         *            The profile to set.
         */
        public void setProfile(String profile) {
            _profile = profile;
        }
    
        public String getUserAgent() {
            return _userAgent;
        }
    
        public void setUserAgent(String userAgent) {
            _userAgent = userAgent;
        }
    
        public String getRemoteHost() {
            return _remoteHost;
        }
    
        public void setRemoteHost(String remoteHost) {
            _remoteHost = remoteHost;
        }
    
        public String getSessionId() {
            return _sessionId;
        }
    
        public void setSessionId(String sessionId) {
            _sessionId = sessionId;
        }
        
        public void addRequest(RequestStatistic stat) {
            _task = stat.getTask();
            _profile = stat.getProfile();
            _userAgent = stat.getUserAgent();
            if (stat.getDatabase() != null) {
                _lastApp = stat.getDatabase();
                _databaseAccess.put(stat.getDatabase(), new AccessData(stat));
            }
            
            if (!WGDatabase.ANONYMOUS_USER.equals(stat.getUser())) {
                _lastLogin = stat.getUser();
                _lastLoginApp = stat.getDatabase();
            }
            
            _lastAccess = stat.getLastAccess();
            _remoteHost = stat.getRemoteHost();
            _requests++;
        }

        public Map<String, AccessData> getDatabaseAccess() {
            return _databaseAccess;
        }

        public void setDatabaseAccess(Map<String, AccessData> databaseAccess) {
            _databaseAccess = databaseAccess;
        }

        public String getLastLogin() {
            return _lastLogin;
        }

        public void setLastLogin(String lastLogin) {
            _lastLogin = lastLogin;
        }

        public String getLastApp() {
            return _lastApp;
        }

        public void setLastApp(String lastApp) {
            _lastApp = lastApp;
        }

        public String getLastLoginApp() {
            return _lastLoginApp;
        }

        public void setLastLoginApp(String lastLoginApp) {
            _lastLoginApp = lastLoginApp;
        }

        public Date getSessionCreated() {
            return _sessionCreated;
        }

        public void setSessionCreated(Date sessionCreated) {
            _sessionCreated = sessionCreated;
        }
    
    }

    /**
     * 
     */
    private final WGACore _wgaCore;

    private final DateFormat DAY_KEY_FORMAT = new SimpleDateFormat("dd.MM.yyyy");

    private final int MAX_SESSIONS = 50;

    private int MAX_DAYS = 7;

    private LinkedMap _requestsPerDay = new LinkedMap();

    private long _allRequests;
    
    private LRUMap _sessionAccess = new LRUMap(MAX_SESSIONS);
    
    private Queue<RequestStatistic> _requestStatisticQueue = new ConcurrentLinkedQueue<RequestStatistic>();

    private Calendar calendar = new GregorianCalendar();

    private boolean _warnedAboutFullQueue = false;

    private UpdateStatisticsThread _updateStatsThread;

    private Map<String,Long> _filteredRequestsMap = new HashMap<String, Long>();
    
    WGAUsageStatistics(WGACore wgaCore) {
        _wgaCore = wgaCore;
        _updateStatsThread = new UpdateStatisticsThread();
        _updateStatsThread.setDaemon(true);
        _updateStatsThread.start();
    }

    public void addRequestStatistic(HttpServletRequest req, HttpSession sess, WGDatabase db, TMLUserProfile profile) {
        try {
                        
            RequestStatistic requestStatistic = new RequestStatistic();
            requestStatistic.setDatabase(db.getDbReference());
            requestStatistic.setLastAccess(new Date());
            requestStatistic.setTask(db.getSessionContext().getTask());
            requestStatistic.setUser(db.getSessionContext().getUser());
            requestStatistic.setSessionId(sess.getId());
            requestStatistic.setSessionCreated(new Date(sess.getCreationTime()));
            requestStatistic.setNewSession(sess.isNew());
            requestStatistic.setRemoteHost(req.getRemoteAddr());
            String userAgent = req.getHeader("USER-AGENT");
            if (userAgent != null) {
                requestStatistic.setUserAgent(userAgent);
            }
            else {
                requestStatistic.setUserAgent("(unknown)");
            }
            
            if (profile != null) {
                requestStatistic.setProfile(profile.getprofile().getName());
            }
            else {
                requestStatistic.setProfile("(none)");
            }
            
            try {
                _requestStatisticQueue.add(requestStatistic);
            }
            catch (IllegalStateException e) {
                if (!_warnedAboutFullQueue) {
                    _wgaCore.getLog().warn("Session statistics queue is temporarily full. Currently displayed statistics may not be accurate.");
                    _warnedAboutFullQueue  = true;
                }
            }
            
        }
        catch (Exception e) {
            _wgaCore.log.error("Unable to update usage statistics.", e);
        }
    }

    /**
     * @return Returns the allRequests.
     */
    public long getAllRequests() {
        return _allRequests;
    }

    public List<Date> getAvailableDays() {

        List<Date> days = new ArrayList<Date>();
        Iterator daysInMap = _requestsPerDay.keySet().iterator();
        while (daysInMap.hasNext()) {
            String dayStr = (String) daysInMap.next();
            try {
                Date dayDate = DAY_KEY_FORMAT.parse(dayStr);
                days.add(dayDate);
            }
            catch (ParseException e) {
                Logger.getLogger("wga").error("Cannot parse day of usage statistic:" + dayStr, e);
            }
        }
        return days;

    }

    public Map getHoursMapForDay(Date date) {

        String dayKey = DAY_KEY_FORMAT.format(date);
        Map hours = (Map) _requestsPerDay.get(dayKey);
        if (hours != null) {
            return Collections.unmodifiableMap(hours);
        }
        else {
            return Collections.emptyMap();
        }

    }

    public void usageTestData() {

        LinkedMap day = new LinkedMap();
        HourStatistic stat = new HourStatistic();

        for (int i = 0; i <= 23; i++) {
            stat.increment();
            day.put(new Integer(i), stat);
        }

        _requestsPerDay.clear();
        _requestsPerDay.put("01.01.2006", day);
        _requestsPerDay.put("02.01.2006", day);
        _requestsPerDay.put("03.01.2006", day);

    }
    
    public List<SessionStatistic> getRecentSessionStatistics() {
        List stats = new ArrayList(_sessionAccess.values());
        Collections.reverse(stats);
        return stats;
    }
    
    public void dispose() {
        _updateStatsThread.setCanceled(true);
        while (_updateStatsThread.isAlive()) {
            Thread.yield();
        }
    }

    public Map<String, Long> getFilteredRequestsMap() {
        return Collections.unmodifiableMap(_filteredRequestsMap);
    }
    

}
