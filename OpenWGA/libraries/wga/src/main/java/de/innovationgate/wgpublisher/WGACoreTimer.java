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

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.ref.WeakReference;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.servlet.http.HttpSession;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabase.WGDatabaseStatistics;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.jdbc.pool.DBCPConnectionProvider;
import de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean;
import de.innovationgate.wga.config.HttpSessionManagerConfiguration;
import de.innovationgate.wgpublisher.log.WGAAsyncLogger;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DBServerScope;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.sessions.WGAHttpSession;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.websockets.TMLPageWebSocket;

public class WGACoreTimer {
    
    public class SessionKeepAliveTask extends TimerTask {

        @Override
        public void run() {
            Thread.currentThread().setName("WGA Session Keep Alive Task");
            
            // If no custom session manager is chosen there is no point in executing this task
            // as we are not able to simulate access on standard Tomcat sessions
            HttpSessionManagerConfiguration config =_core.getWgaConfiguration().getHttpSessionManagerConfiguration();
            if (config == null || config.getImplClassName() == null) {
                return;
            }
            
            int count = 0;
            for (WeakReference<PageConnection> connRef : _core.getPageConnectionManager().getActiveConnections().values()) {
                
                PageConnection conn = connRef.get();
                if (conn == null) {
                    continue;
                }
                
                if (conn.getWebSocket() == null) {
                    continue;
                }
                
                TMLPageWebSocket webSocket = conn.getWebSocket();
                if (!webSocket.isConnected()) {
                    continue;
                }
                
                HttpSession session = conn.getSession();
                if (!(session instanceof WGAHttpSession)) {
                    continue;
                }
                
                WGAHttpSession wgaSession = (WGAHttpSession) session;
                wgaSession.access();
                count++;
                
            }
            
            /*
            if (count > 0) {
                _core.getLog().info("Updated " + WGUtils.DECIMALFORMAT_STANDARD.format(count) + " sessions with active WebSocket connections to be kept alive");
            }
            */
            
        }
        
    }
    
    public class ProblemDeterminationTask extends TimerTask {

    
        @Override
        public void run() {
    
            Thread.currentThread().setName("WGA Scheduled Problem Determination Task");
            ProblemDeterminationTaskOccasion occ = new ProblemDeterminationTaskOccasion();
            try {
                _core.getProblemRegistry().clearProblemOccasion(occ);
                
                // Test cache utilisations
                for (WGDatabase db : _core.getContentdbs().values()) {
                    if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                        WGDatabaseStatistics stats = db.getStatistics();
                        if (stats.getDocumentCount() >= stats.getDocumentCacheMaxSize()) {
                            if (stats.getDocumentCacheUtilisation() <= 75) {
                                Problem.Vars vars = Problem
                                .var("dbkey", db.getDbReference())
                                .var("utilisation", String.valueOf(stats.getDocumentCacheUtilisation()));
                                Problem problem = Problem.create(occ, new DatabaseScope(db.getDbReference()), "cacheProblem.documentLowUtilisation", ProblemSeverity.LOW, vars);
                                _core.getProblemRegistry().addProblem(problem);
                            }
                        }
                    }
                }
                
                // Test Lucene
                LuceneManager luceneManager =_core.getLuceneManager();
                if (luceneManager == null) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "luceneProblem.managerDead", ProblemSeverity.HIGH));
                }
                else if (luceneManager.getIndexer() == null) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "luceneProblem.indexerDead", ProblemSeverity.HIGH));
                }
                else {
                    long timeSinceLastRun = System.currentTimeMillis() -  luceneManager.getIndexer().getLastFullIndexingRun();
                    if (timeSinceLastRun > (1000 * 60 * 60)) {
                        _core.getProblemRegistry().addProblem(Problem.create(occ, "luceneProblem.indexerNotRunning", ProblemSeverity.HIGH, Problem.var("time", timeSinceLastRun / 1000 / 60)));
                    }
                }
                
                if (luceneManager != null) {
	                File luceneIndexFolder = luceneManager.getIndexDirectory();
	                long indexSize=0;
	                for (File file : luceneIndexFolder.listFiles()) {
	                    indexSize+=file.length();
	                }
	                long luceneFreeSpace = luceneIndexFolder.getFreeSpace();
	                if (luceneFreeSpace < indexSize * 2) {
	                    Problem.Vars vars = Problem
	                        .var("indexsize", WGUtils.DECIMALFORMAT_STANDARD.format(indexSize / 1024 / 1024))
	                        .var("freespace", WGUtils.DECIMALFORMAT_STANDARD.format(luceneFreeSpace / 1024 / 1024))
	                        .var("dir", luceneIndexFolder.getAbsolutePath());
	                    _core.getProblemRegistry().addProblem(Problem.create(occ, "luceneProblem.lowSpace", ProblemSeverity.LOW, vars));
	                }
                }
                
                // Test db connection pools
                MBeanServer mxServer = ManagementFactory.getPlatformMBeanServer();
                for (ObjectName name : mxServer.queryNames(new ObjectName(DBCPConnectionProvider.JMX_DBPOOLS_ADDRESS + ",pool=*"), null)) {
                    testPoolBean(occ, mxServer, name);
                }
                for (ObjectName name : mxServer.queryNames(new ObjectName(DBCPConnectionProvider.JMX_SERVERPOOLS_ADDRESS + ",pool=*"), null)) {
                    testPoolBean(occ, mxServer, name);
                }
                
                // Test general system
                File dataFolder = _core.getWgaDataDir();
                long dataFreeSpace = dataFolder.getFreeSpace();
                if (dataFreeSpace < (long) 1024 * 1024 * 100) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "systemProblem.lowDataSpace100", ProblemSeverity.HIGH, Problem.var("freespace", WGUtils.DECIMALFORMAT_STANDARD.format(dataFreeSpace / 1024 / 1024)).var("dir", dataFolder.getAbsolutePath())));
                }
                
                File tempFolder = WGFactory.getTempDir();
                long tempFreeSpace = tempFolder.getFreeSpace();
                if (tempFreeSpace < (long) 1024 * 1024 * 100) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "systemProblem.lowTempSpace100", ProblemSeverity.HIGH, Problem.var("freespace", WGUtils.DECIMALFORMAT_STANDARD.format(tempFreeSpace / 1024 / 1024)).var("dir", tempFolder.getAbsolutePath())));
                }
                
                // Test cluster
                if (_core.getWgaConfiguration().getClusterConfiguration().isEnabled()) {
                    _core.getClusterService().checkHealth();
                }
                
                // Test access logger
                if (_core.getAccessLogger() != null && _core.getAccessLogger().getLogger() instanceof WGAAsyncLogger<?>) {
                    WGAAsyncLogger<?> logger = (WGAAsyncLogger<?>) _core.getAccessLogger().getLogger();
                   if (logger.getLastLogged() < System.currentTimeMillis() - (1000 * 60)) {
                       if (logger.getLastLogged() < System.currentTimeMillis() - (1000 * 60 * 5)) {
                           logger.getQueue().clear();
                           _core.getProblemRegistry().addProblem(Problem.create(occ, "loggerProblem.noLogging5Minutes", ProblemSeverity.HIGH, Problem.var("lastlogged", WGUtils.TIMEFORMAT_STANDARD.format(new Date(logger.getLastLogged())))));
                       }
                       else {
                           _core.getProblemRegistry().addProblem(Problem.create(occ, "loggerProblem.noLogging1Minute", ProblemSeverity.LOW, Problem.var("lastlogged", WGUtils.TIMEFORMAT_STANDARD.format(new Date(logger.getLastLogged())))));
                       }
                   }
                }
                
            }
            catch (Throwable e) {
                _core.getLog().error("Exception running problem determination task", e);
                _core.getProblemRegistry().addProblem(Problem.create(occ, "taskFailed.exception", ProblemSeverity.HIGH, e)); 
            }
            
        }

        private void testPoolBean(ProblemOccasion occ, MBeanServer mxServer, ObjectName name) {
            DBCPPoolInformationMBean poolBean = javax.management.JMX.newMBeanProxy(mxServer, name, DBCPPoolInformationMBean.class);
            int numActive = poolBean.getNumActive();
            if (numActive == 0) {
                return;
            }
            
            int maxActive = poolBean.getMaxActive();
            double warnThreshold = Math.floor((double) maxActive / 2d);
            double critThreshold = Math.floor((double) maxActive / 10.0d * 8.0d);
            
            String keyBase;
            ProblemScope scope;
            if (poolBean.isServer()) {
                scope = new DBServerScope(_core.getDatabaseServers().get(poolBean.getEntityKey()));
                keyBase = "dbConnProblem.server";
            }
            else {
                scope = new DatabaseScope(poolBean.getEntityKey());
                keyBase = "dbConnProblem.db"; 
            }

            if (numActive >= critThreshold) {
                _core.getProblemRegistry().addProblem(Problem.create(occ, scope, keyBase + "LowPoolCrit", ProblemSeverity.HIGH, Problem.var("numactive", WGUtils.DECIMALFORMAT_STANDARD.format(numActive)).var("maxactive", WGUtils.DECIMALFORMAT_STANDARD.format(maxActive))));
            }
            else if (numActive >= warnThreshold) {
                _core.getProblemRegistry().addProblem(Problem.create(occ, scope, keyBase + "LowPoolWarn", ProblemSeverity.HIGH, Problem.var("numactive", String.valueOf(numActive)).var("maxactive", String.valueOf(maxActive))));
            }
            
        }
        
        
        
    }

    public class DevPluginsMonitoringTask extends TimerTask {
        
        private File _dir;
        private long _lastChanged;
    
        public DevPluginsMonitoringTask(File dir) {
            _dir = dir;
            _lastChanged = getLatestChange();
        }
    
        private long getLatestChange() {
            
            long latestChange = Long.MIN_VALUE;
            File[] files = _dir.listFiles();
            for (File f : files) {
                long fileLastMod = f.lastModified();
                latestChange = (fileLastMod > latestChange ? fileLastMod : latestChange);
                if (f.isDirectory()) {
                    File[] childFiles = f.listFiles();
                    for (File f2 : childFiles) {
                        long fileLastMod2 = f2.lastModified();
                        latestChange = (fileLastMod2 > latestChange ? fileLastMod2 : latestChange);    
                    }
                }
            }
            return latestChange;
            
        }
    
        @Override
        public void run() {
            
            try {
                
                Thread.currentThread().setName("WGA Developer Plugins Monitoring Task");
                
                synchronized (WGACoreTimer.this) {
                    boolean anythingRemoved = _core.uninstallRemovedDefaultPlugins();
                    
                    long newLastChanged = getLatestChange();
                    if (anythingRemoved || newLastChanged > _lastChanged) {
                        _lastChanged = newLastChanged;
                        _core.updatePlugins();
                    }
                }
                
            }
            catch (Throwable e) {
                _core.getLog().error("Exception executing developer plugins monitoring task", e);
            }
            
            
            
        }
        
    }

    public class ProblemDeterminationTaskOccasion implements ProblemOccasion, MessageVariableProvider {
    
        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }
    
        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }
    
        @Override
        public Class<?> getDefaultRefClass() {
            return WGACoreTimer.class;
        }
    
        @Override
        public Problem.Vars getDefaultMessageVariables() {
            return null;
        }
    
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
        @Override
        public int hashCode() {
            return getClass().hashCode();
        }
        
        @Override
        public boolean equals(Object obj) {
            return obj instanceof ProblemDeterminationTaskOccasion;
        }
        
        
        
    }

    /**
     * Performs cleanup operations 
     */
    private final class CleanupTask extends TimerTask {
        public void run() {
            
            try {
                Thread.currentThread().setName("WGA Core Cleanup Task");
            
            // Cleanup transient scheduler jobs
            _core.getScheduler().clearTransientJobs();
            
            }
            catch (Throwable e) {
                _core.getLog().error("Exception running cleanup task", e);
            }
    
        }
    }

    public class ConfigUpdateTask extends TimerTask {
    
        private WGACore _core;
    
        public ConfigUpdateTask(WGACore core) {
            _core = core;
        }
    
        /**
         * @see java.lang.Runnable#run()
         */
        public void run() {
    
            Thread.currentThread().setName("WGA.MainTimer");
    
            try {
                synchronized(WGACoreTimer.this) {
                    if (_core.getConfigFile().lastModified() > _core.getConfigFileLastModified()) {
                        _core.updateConfig();
                    }
                }
            }
            catch (Exception e) {
                _core.getLog().error("Error updating configuration", e);
            }
        }
    
    }

    private Timer _timer;
    private WGACore _core;
    
    protected WGACoreTimer(WGACore core) {
        
        _core = core;

        // Cleanup tasks running every minute
        this._timer = new Timer();
        TimerTask task = new CleanupTask();
        long delay = 1000 * 60;
        _timer.scheduleAtFixedRate(task, delay, delay);

        // Timer task for config file update
        task = new ConfigUpdateTask(_core);
        delay = 1000 * 5;
        _timer.scheduleAtFixedRate(task, delay, delay);
        
        // Task for scheduled problem determination
        task = new ProblemDeterminationTask();
        delay = 1000 * 60;
        _timer.scheduleAtFixedRate(task, delay, delay);
        
        // Timer task for monitoring developer plugins 
        String devPluginsPath = System.getProperty(WGACore.SYSPROPERTY_DEVELOPER_PLUGINS);
        if (devPluginsPath != null) {
            File devPluginsFolder = _core.getWGAFile(devPluginsPath);
            if (devPluginsFolder.exists() && devPluginsFolder.isDirectory()) {
                task = new DevPluginsMonitoringTask(devPluginsFolder);
                delay = 1000 * 10;
                this._timer.scheduleAtFixedRate(task, delay, delay);
            }
        }
        
        // Task keeping HTTP sessions with active websocket connections alive
        task = new SessionKeepAliveTask();
        delay = 1000 * 60 * 5;
        _timer.scheduleAtFixedRate(task, delay, delay);

    }
    
    protected void shutdown() {
        
        _timer.cancel();
        _timer = null;
        
    }

}
