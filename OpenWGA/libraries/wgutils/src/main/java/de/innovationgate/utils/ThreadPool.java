/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Simple ThreadPool that just creates n threads and is able to issue a Runnable to an idle thread.
 *
 */
public class ThreadPool {
    
    static class WorkerThread extends Thread {

        private ThreadPool _pool;

        private String _workerName;

        private static int _workerNumber = 0;

        public WorkerThread(ThreadPool pool) {
            _pool = pool;
            _workerName = "ThreadPoolWorker(" + getNextNumber() + ")";
        }

        private static synchronized int getNextNumber() {
            return _workerNumber++;
        }

        /**
         * Scan for and execute tasks.
         */
        public void run() {
            Runnable target = null;
            Thread.currentThread().setName(_workerName);
            do {
                target = _pool.getTarget();
                if (target != null) {
                    target.run();
                }
            } while (target != null);
        }
    }

    protected Thread threads[] = null;

    private Collection _targets = new ArrayList();

    /**
     * Initializes a thread pool with the given number of threads
     * @param size Number of threads
     */
    public ThreadPool(int size) {
        threads = new WorkerThread[size];
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new WorkerThread(this);
            threads[i].start();
        }
    }

    /**
     * Executes a runnable with one thread from the pool.
     * There is no guarantee when this runnable will be started.
     * @param r The runnable to execute
     */
    public synchronized void execute(Runnable r) {
        _targets.add(r);
        // call notify to reflect list updates
        notify();
    }

    private synchronized Runnable getTarget() {
        try {
            while (!_targets.iterator().hasNext()) {
                wait();
            }

            Runnable r = (Runnable) _targets.iterator().next();
            _targets.remove(r);
            return r;
        }
        catch (InterruptedException e) {
            return null;
        }
    }

    protected void finalize() {
        for (int i = 0; i < threads.length; i++) {
            threads[i].interrupt();
            threads[i].destroy();
        }
    }
}

