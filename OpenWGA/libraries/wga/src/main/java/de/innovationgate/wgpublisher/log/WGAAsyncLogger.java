package de.innovationgate.wgpublisher.log;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.servlet.ServletRequest;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.wga.config.AccessLog;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.log.WGALogger;
import de.innovationgate.wgpublisher.log.WGALoggerException;
import de.innovationgate.wgpublisher.log.WGARequestInformation;

/**
 * Base class to build loggers that log their information asynchronously.
 * The Data to log is loaded into a {@link LogData} object and put up a loggerQueue.
 * Every 10 seconds a background task writes the queue data to the log target. 
 * 
 */
public abstract class WGAAsyncLogger<D extends LogData> implements WGALogger {

    private static long UPDATE_INTERVAL = 10 * 1000;

	private static final int QUEUE_MAX_SIZE = 50000;

	/**
	 * The logger queue, containing all collected RequestObjects not yet logged. 
	 */
	private ConcurrentLinkedQueue<D> _loggerQueue = new ConcurrentLinkedQueue<D>();
	private Timer _logTimer;
	protected WGACore _core;
	protected AccessLog _config;
	private long _lastLogged = System.currentTimeMillis();

	public final void init(AccessLog config, WGACore core) throws WGALoggerException {
		
	    try {
            _core = core;
            _config = config;
            _logTimer = new Timer();
            
            initLogWriting(config, core);
            
            TimerTask timerTask = new TimerTask() {
    
            	@Override
            	public void run() {
            	    try {
            			Thread.currentThread().setName("WGA.AccessLogger");
            			writeLog(_loggerQueue);
            			_lastLogged = System.currentTimeMillis();
            	    }
            	    catch (Throwable t) {
            	        _core.getLog().error("Exception writing access log data", t);
            	    }
            	}
            	
            };
            _logTimer.schedule(timerTask, UPDATE_INTERVAL, UPDATE_INTERVAL);
	    }
	    catch (Throwable e) {
	        throw new WGALoggerException("Exception instantiating async logger", e);
	    }
	        
}
	
	

	protected abstract void initLogWriting(AccessLog config, WGACore core) throws Exception;



    public void logRequest(ServletRequest request) throws WGALoggerException {
	    try {
	    	WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
	    	if (info != null && info.isLoggingEnabled()) {
	    	    D logData = extractLogData(request, info);
	    	    _loggerQueue.add(logData);
	    	}
        }
	    catch (WGInvalidDatabaseException e) {
	        // Swallowed silently bc. that may happen on shutdown
	    }
	    catch (Throwable e) {
	        if (_loggerQueue.size() > QUEUE_MAX_SIZE) {
	            _loggerQueue.clear();
	            throw new WGALoggerException("Exception logging request. Queue has reached max size. Clearing queue", e);
            }
            throw new WGALoggerException("Exception logging request", e);
            
        }
	}

	/**
	 * @see de.innovationgate.wgpublisher.log.WGALogger#close()
	 */
	public void close() {
		this._logTimer.cancel();
	}
	
	protected abstract D extractLogData(ServletRequest req, WGARequestInformation reqInfo) throws Exception;
	
	protected abstract void writeLog(Queue<D> logData) throws Exception;



    public long getLastLogged() {
        return _lastLogged;
    }
    
    public ConcurrentLinkedQueue<D> getQueue() {
        return _loggerQueue;
    }

}
