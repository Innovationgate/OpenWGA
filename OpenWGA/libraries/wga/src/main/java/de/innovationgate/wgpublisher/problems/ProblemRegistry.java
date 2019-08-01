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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.mail.WGAMailNotification;

public class ProblemRegistry {
    
    public static final String CONFIRMED_PROBLEMS_FILE = "confirmed-problems.xml";

    private static final int MAX_SCOPE_SIZE = 1000;

    private Map<Class<? extends ProblemType>,Map<ProblemScope,Map<String,Problem>>> _problems = new ConcurrentHashMap<Class<? extends ProblemType>, Map<ProblemScope,Map<String,Problem>>>();
    
    private ConfirmedProblems _confirmedProblems;
    
    //private Set<ProblemOccasion> _occasionsWithProblems = new HashSet<ProblemOccasion>();
    
    private Queue<ProblemQueueEvent> _problemEvents = new ConcurrentLinkedQueue<ProblemQueueEvent>();

    private WGACore _core;
    
    private Timer _timer;
    
    private Map<Class<? extends ProblemType>, Set<ProblemListener>> _listeners = new HashMap<Class<? extends ProblemType>, Set<ProblemListener>>();
    
    public ProblemRegistry(WGACore core) {
        _core = core;
        
        File confirmedProblemsFile = new File(_core.getWgaDataDir(), CONFIRMED_PROBLEMS_FILE);
        if (confirmedProblemsFile.exists()) {
            FileInputStream in = null;
            try {
                in = new FileInputStream(confirmedProblemsFile);
                _confirmedProblems = ConfirmedProblems.load(in);
                in.close();
            }
            catch (IOException e) {
                _core.getLog().error("Exception loading confirmed problems list", e);
            }
        }
        
        if (_confirmedProblems == null) {
            _confirmedProblems = new ConfirmedProblems();
        }
        
        _timer = new Timer();
        _timer.scheduleAtFixedRate(
                new TimerTask() {
                    
                    @Override
                    public void run() {

                        Thread.currentThread().setName("WGA Problem Registry Update Task");
                        try {
                            updateRegistry();
                        }
                        catch (Throwable e) {
                            _core.getLog().error("Excepion updating problem registry", e);
                        }
                        
                    }
            
                }
                , 10000, 10000);
        
    }
    
    public void addProblem(Problem problem) {
        _problemEvents.add(problem);
    }
    
    public void clearProblemOccasion(ProblemOccasion occ) {
        _problemEvents.add(occ);
    }
    
    public void clearProblemScope(ProblemScope scope) {
        _problemEvents.add(scope);
    }
    
    public void clearProblemType(ProblemType type) {
        _problemEvents.add(type);
    }
    
    public synchronized void updateRegistry() {

        ProblemQueueEvent event = null;
        
        // Collections to keep track of which new problems were added
        Map<Class<? extends ProblemType>, Set<Problem>> addedProblems = new HashMap<Class<? extends ProblemType>, Set<Problem>>();
        Set<ProblemPath> clearedProblemPaths = new HashSet<ProblemPath>();
        
        // Traverse events
        while ((event = _problemEvents.poll()) != null) {
            
            if (event instanceof ProblemType) {
                performClearProblemType(((ProblemType) event).getClass(), addedProblems, clearedProblemPaths);
            }
            if (event instanceof ProblemOccasion) {
                performClearOccasion((ProblemOccasion) event, addedProblems, clearedProblemPaths);
            }
            else if (event instanceof ProblemScope) {
                performClearScope((ProblemScope) event, addedProblems, clearedProblemPaths);
            }
            else if (event instanceof Problem) {
                performAddProblem((Problem) event, addedProblems, clearedProblemPaths);
            }
            
        }
        
        // Notify listeners
        for (Map.Entry<Class<? extends ProblemType>, Set<Problem>> addedTypeEntry : addedProblems.entrySet()) {
            if (addedTypeEntry.getValue().size() > 0) {
                Set<ProblemListener> listeners = _listeners.get(addedTypeEntry.getKey());
                if (listeners != null) {
                    Set<Problem> unmodifiableProblems = Collections.unmodifiableSet(addedTypeEntry.getValue());
                    for (ProblemListener listener:  listeners) {
                        listener.problemsAdded(addedTypeEntry.getKey(), unmodifiableProblems);
                    }
                }
            }
        }
        
    }
    
    private void performClearProblemType(Class<? extends ProblemType> problemType, Map<Class<? extends ProblemType>, Set<Problem>> addedProblems, Set<ProblemPath> clearedProblemPaths) {
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(problemType);
        if (problemsByScope != null) {
            for (Map<String,Problem> problemsByKey : problemsByScope.values()) {
                for (Problem problem : problemsByKey.values()) {
                    problemsByKey.remove(problem.getPath().getKey());
                    clearedProblemPaths.add(problem.getPath());
                    
                    Set<Problem> problems = addedProblems.get(problem.getPath().getType());
                    if (problems != null) {
                        problems.remove(problem);
                    }
                }
            }
        }
        
    }

    private void performClearScope(ProblemScope scope, Map<Class<? extends ProblemType>, Set<Problem>> addedProblems, Set<ProblemPath> clearedProblemPaths) {

        
        for (Map<ProblemScope,Map<String,Problem>> problemsByScope : _problems.values()) {
            
            Map<String,Problem> problemsByKey = problemsByScope.get(scope);
            if (problemsByKey != null) {
                for (Problem problem : problemsByKey.values()) {
                    problemsByKey.remove(problem.getPath().getKey());
                    clearedProblemPaths.add(problem.getPath());
                    
                    Set<Problem> problems = addedProblems.get(problem.getPath().getType());
                    if (problems != null) {
                        problems.remove(problem);
                    }
                }
            }
            
        }
        
    }

    private void performClearOccasion(ProblemOccasion occ, Map<Class<? extends ProblemType>, Set<Problem>> addedProblems, Set<ProblemPath> clearedProblemPaths) {
        
    	/*
        if (!_occasionsWithProblems.contains(occ)) {
            return;
        }
        */
        
        for (Map<ProblemScope,Map<String,Problem>> problemsByScope : _problems.values()) {
            
            for (Map<String,Problem> problemsByKey : problemsByScope.values()) {
                
                for (Problem problem : problemsByKey.values()) {
                    
                    if (occ.equals(problem.getOccasion())) {
                        problemsByKey.remove(problem.getPath().getKey());
                        clearedProblemPaths.add(problem.getPath());
                        
                        Set<Problem> problems = addedProblems.get(problem.getPath().getType());
                        if (problems != null) {
                            problems.remove(problem);
                        }
                    }
                    
                }
                
            }
            
        }
        
        //_occasionsWithProblems.remove(occ);
        
    }
    
    private void performAddProblem(Problem problem, Map<Class<? extends ProblemType>, Set<Problem>> addedProblems, Set<ProblemPath> clearedProblemPaths) {
        
        ProblemPath path = problem.getPath();
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(path.getType());
        if (problemsByScope == null) {
            problemsByScope = new ConcurrentHashMap<ProblemScope, Map<String,Problem>>();
            _problems.put(path.getType(), problemsByScope);
        }
        
        Map<String,Problem> problemsByKey = problemsByScope.get(path.getScope());
        if (problemsByKey == null) {
            problemsByKey = new ConcurrentHashMap<String, Problem>();
            problemsByScope.put(path.getScope(), problemsByKey);
        }
        
        boolean problemAdded = false;
        Problem oldProblem = problemsByKey.get(path.getKey());
        if (oldProblem instanceof AdditiveProblem) {
            ((AdditiveProblem) oldProblem).addProblem(problem);
            problemAdded = true;
        }
        else {
            if (problemsByKey.size() <= MAX_SCOPE_SIZE) {
                problemsByKey.put(path.getKey(), problem);
                problemAdded = true;
            }
        }
        
        if (problemAdded) {
            if (!clearedProblemPaths.contains(path) && !isConfirmedProblem(path)) {
                Set<Problem> problems = addedProblems.get(path.getType());
                if (problems == null) {
                    problems = new HashSet<Problem>();
                    addedProblems.put(path.getType(), problems);
                }
                problems.add(problem);
            }

            if(problem.getSeverity().equals(ProblemSeverity.HIGH)){
            	WGAMailNotification mail = new WGAMailNotification(WGAMailNotification.TYPE_PROBLEM);
            	mail.setSubject(problem.getTitle(Locale.getDefault()));
            	mail.append(problem.getMessage(Locale.getDefault()));
            	mail.append("<br>");
            	mail.append(problem.getDescription(Locale.getDefault()));        	
    			_core.send(mail);
            }

        }
        
    }
    
    public synchronized void confirmProblem(ProblemPath path) throws IOException {
        
        if (_confirmedProblems.getPaths().add(path)) {
            FileOutputStream out = new FileOutputStream(new File(_core.getWgaDataDir(), CONFIRMED_PROBLEMS_FILE));
            _confirmedProblems.write(out);
            out.flush();
            out.close();
        }
        
        
    }
    
    
    public void close() {
        _timer.cancel();
    }
    
    public long size(){
    	java.util.Iterator<Map<ProblemScope, Map<String, Problem>>> problems = _problems.values().iterator();
    	long size = 0;
    	while(problems.hasNext()){
    		Collection<Map<String, Problem>> problem_set = problems.next().values();
    		java.util.Iterator<Map<String, Problem>> it = problem_set.iterator();
    		while(it.hasNext())
    			size += it.next().values().size();
    	}
    	return size;
    }
    
    public synchronized Map getProblems() {
        return _problems;
    }
    
    public synchronized List<Problem> getProblems(Class<? extends ProblemType> type) {
        return getProblems(type, false);
    }

    public synchronized List<Problem> getProblems(Class<? extends ProblemType> type, boolean includeConfirmed) {
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(type);        
        if (problemsByScope == null) {
            return Collections.emptyList();
        }
        
        List<Problem> problems = new ArrayList<Problem>();
        for (Map<String,Problem> problemsByKey : problemsByScope.values()) {
            
            for (Problem problem : problemsByKey.values()) {
                
                if (isValidProblem(problem, includeConfirmed)) {
                    problems.add(problem);
                }
                
            }
            
        }
        
        return problems;
        
    }
    
    public synchronized Set<ProblemScope> getProblemScopes(Class<? extends ProblemType> type) {
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(type);        
        if (problemsByScope == null) {
            return Collections.emptySet();
        }

        return problemsByScope.keySet();
        
    }
    
    public synchronized List<Problem> getProblems(Class<? extends ProblemType> type, ProblemScope scope) {
        return getProblems(type, scope, false);
    }
    
    public synchronized List<Problem> getProblems(Class<? extends ProblemType> type, ProblemScope scope, boolean includeConfirmed) {
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(type);        
        if (problemsByScope == null) {
            return Collections.emptyList();
        }
        
        Map<String,Problem> problemsByKey = problemsByScope.get(scope);
        if (problemsByKey == null) {
            return Collections.emptyList();
        }
        
        List<Problem> problems = new ArrayList<Problem>();
        for (Problem problem : problemsByKey.values()) {
            
            if (isValidProblem(problem, includeConfirmed)) {
                problems.add(problem);
            }
            
        }
        
        return problems;
        
    }

    private boolean isValidProblem(Problem problem, boolean includeConfirmed) {
        return includeConfirmed || !isConfirmedProblem(problem.getPath());
    }

    public boolean isConfirmedProblem(ProblemPath problemPath) {
        return _confirmedProblems.getPaths().contains(problemPath);
    }
    
    public synchronized boolean dismissProblem(ProblemPath path) {
        
        Map<ProblemScope,Map<String,Problem>> problemsByScope = _problems.get(path.getType());
        if (problemsByScope == null) {
            return false;
        }
        
        Map<String,Problem> problemsByKey = problemsByScope.get(path.getScope());
        if (problemsByKey == null) {
            return false;
        }
        
        Problem problem = problemsByKey.get(path.getKey());
        if (problem == null) {
            return false;
        }
        
        problemsByKey.remove(path.getKey());
        
        if (problemsByKey.size() == 0) {
            problemsByScope.remove(path.getScope());
        }
        
        return true;
        
        
    }
    
    public synchronized void addListener(ProblemListener listener, Class<? extends ProblemType>... types) {
        
        for (Class<? extends ProblemType> type : types) {
            
            Set<ProblemListener> typeListeners = _listeners.get(type);
            if (typeListeners == null) {
                typeListeners = new HashSet<ProblemListener>();
                _listeners.put(type, typeListeners);
            }
            typeListeners.add(listener);
            
        }
        
    }
    
    public synchronized void removeListener(ProblemListener listener) {
        
        for (Set<ProblemListener> typeListeners : _listeners.values()) {
            typeListeners.remove(listener);
        }
        
    }
    
    public synchronized void clearConfirmation(ProblemPath path) throws IOException {
        
        if (_confirmedProblems.getPaths().remove(path)) {
            FileOutputStream out = new FileOutputStream(new File(_core.getWgaDataDir(), CONFIRMED_PROBLEMS_FILE));
            _confirmedProblems.write(out);
            out.flush();
            out.close();
        }
        
    }
    
    public synchronized void clear() {
        _problems.clear();
        //_occasionsWithProblems.clear();
    }
    
    /*
    public Set<ProblemOccasion> getOccasionsWithProblems(){
    	return _occasionsWithProblems;
    }
    */
        
}
 