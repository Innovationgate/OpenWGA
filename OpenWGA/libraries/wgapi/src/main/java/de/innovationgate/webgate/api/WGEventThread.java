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
package de.innovationgate.webgate.api;

import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.locking.LockException;
/**
 * Thread that invokes background events (database updates) on the WGAPI. Automatically started once a database gets opened.
 */
public class WGEventThread {

	public static String SYSPROPERTY_EVENT_SLEEP_TIME = "de.innovationgate.wgapi.eventthread.sleeptime";

	private int _sleepTime = 1000 * 10;
	private Thread _thread;
	private Date _lastFullMaintenance = WGUtils.dateOnly(new Date());
	private volatile AtomicLong _runIndex = new AtomicLong(Long.MIN_VALUE);	
	
	protected void start() {
		this._thread = new Thread(new Runnable() {

            @Override
            public void run() {
                doRun();
            }
		    
		});
		_thread.setDaemon(true);
		_thread.setPriority(Thread.MIN_PRIORITY);
		_thread.start();
	}
	
	public void stop() {
	    
	    // Already stopped
	    if (_thread == null) {
	        return;
	    }
	    
	    Thread theThread = _thread;
		_thread = null;
		
		theThread.interrupt();
		while (true) {
		    if (!theThread.isAlive()) {
		        break;
		    }
		}
		    
	}

	/**
	 * @see Thread#run()
	 */
	private void doRun() {

		Thread meThread = Thread.currentThread();
		meThread.setName("WGEventThread.run");
		WGFactory.setEventThread(true);

		String strSleepTime = System.getProperty(WGEventThread.SYSPROPERTY_EVENT_SLEEP_TIME);
		if (strSleepTime != null) {
			this._sleepTime = Integer.parseInt(strSleepTime);
		}

		while (this._thread == meThread) {
		    
		    
            // Do "full maintenance" on the first event thread run each day
            boolean fullMaintenance = false;
            Date nowDate = WGUtils.dateOnly(new Date());
            if (!nowDate.equals(_lastFullMaintenance)) {
                fullMaintenance = true;
                _lastFullMaintenance = nowDate;
                WGFactory.getLogger().info("Performing full WGAPI event thread maintenance");
            }

			List<WGDatabase> dbsList = WGFactory.getInstance().getOpenedDatabases();
			WGDatabase db;
			for (int i = 0; i < dbsList.size(); i++) {
				db = dbsList.get(i);
				if (db.isReady() && db.isConnected() && db.monitorLastChange()) {
					try {
						db.openSession();
						if (db.isSessionOpen()) {
							db.getSessionContext().setTask("WGAPI Event Thread");
							db.getSessionContext().setBatchProcess(true);
							
							// Since we manage the cache here, we need caching although having a batch process
							db.getSessionContext().setCachingEnabled(true);
							
							db.checkDatabaseUpdates(false);
							
							db.maintenance(fullMaintenance);
							
							db.closeSession();
						}
						else {
							WGFactory.getLogger().error("Event Thread: Unable to open master session on database " + db.getDbReference());
						}
					}
					catch (WGUnavailableException e) {
					    WGFactory.getLogger().error("Event Thread: Unable to open master session on database " + db.getDbReference() + " because it is currently unavailable", e);
					}
					catch (RuntimeException e) {
					    WGFactory.getLogger().error("Runtime exception executing event thread on database " + db.getDbReference(), e);
					}
                    catch (LockException e) {
                        WGFactory.getLogger().info("Lock exception executing event thread on database " + db.getDbReference() + " - service will be done next run.");
                    }
					catch (Throwable e) {
					    WGFactory.getLogger().error(e.getClass().getName() +  " executing event thread on database " + db.getDbReference(), e);
					}

				}
			}

			WGFactory.getInstance().closeSessions();
			try {
			    _runIndex.incrementAndGet();
				Thread.sleep(_sleepTime);
			}
			catch (InterruptedException exc) {}
		}

	}
	
	public void nextRun() {
	    long oldRunIndex = _runIndex.get();
	    _thread.interrupt();
	    int idx = 0;
	    while (oldRunIndex == _runIndex.get()) {
	        try {
                Thread.sleep(100);
            }
            catch (InterruptedException e) {
            }
	        
	        idx++;
	        if (idx > 10) {
	            return;
	        }
	    }
	    
	}
	
	public long getRunIndex() {
	    return _runIndex.get();
	}

}

