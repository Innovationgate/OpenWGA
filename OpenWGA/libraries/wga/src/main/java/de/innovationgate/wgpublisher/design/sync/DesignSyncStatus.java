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

package de.innovationgate.wgpublisher.design.sync;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wgpublisher.design.fs.AbstractDesignFile;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;

public class DesignSyncStatus {
    
    private String _basePath;
    private Map _tmlDeployments = new HashMap();
    private Map _scriptDeployments = new HashMap();
    private Map _fileContainerDeployments = new HashMap();
    private transient FileSystemDesignManager _manager;
    
    public DesignSyncStatus(FileSystemDesignManager manager, String basePath) {
        _manager = manager;
        _basePath = basePath;
    }
    
    public DesignSyncStatus() {
    }
    
    /**
     * @return Returns the fileContainerDeployments.
     */
    public Map getFileContainerDeployments() {
        return _fileContainerDeployments;
    }
    /**
     * @return Returns the scriptDeployments.
     */
    public Map getScriptDeployments() {
        return _scriptDeployments;
    }
    /**
     * @return Returns the tmlDeployments.
     */
    public Map getTmlDeployments() {
        return _tmlDeployments;
    }
    public List findDeletedDeployments(DesignSyncStatus deployments) {
        List differences = new ArrayList();
        
        if (_manager.getSyncedDoctypes().contains(new Integer(WGDocument.TYPE_CSSJS))) {
            findDeletedDeployments(_scriptDeployments, deployments.getScriptDeployments(), differences);
        }
        
        if (_manager.getSyncedDoctypes().contains(new Integer(WGDocument.TYPE_TML))) {
            findDeletedDeployments(_tmlDeployments, deployments.getTmlDeployments(), differences);
        }
        
        if (_manager.getSyncedDoctypes().contains(new Integer(WGDocument.TYPE_FILECONTAINER))) {
            findDeletedDeployments(_fileContainerDeployments, deployments.getFileContainerDeployments(), differences);
        }
        
        return differences;
    }
    private void findDeletedDeployments(Map oldDeployments, Map newDeployments, List differences) {
        Set newScriptKeys = newDeployments.keySet();
        Set oldScriptKeys = oldDeployments.keySet();
        oldScriptKeys.removeAll(newScriptKeys);
        Iterator scriptIt = oldScriptKeys.iterator();
        String key;
        while (scriptIt.hasNext()) {
            key = (String) scriptIt.next();
            differences.add(oldDeployments.get(key));
        }
    }
    
    public AbstractDesignFile putDeployment(String key, DesignDeployment deployment) {
        
        deployment.setParent(this);
        AbstractDesignFile oldDeployment;
        
        if (deployment instanceof FileContainerDeployment) {
            oldDeployment = (AbstractDesignFile) getFileContainerDeployments().put(key, deployment);
        }
        else if (deployment instanceof ScriptDeployment) {
            oldDeployment = (AbstractDesignFile) getScriptDeployments().put(key, deployment);
        }
        else if (deployment instanceof TMLDeployment) {
            oldDeployment = (AbstractDesignFile) getTmlDeployments().put(key, deployment);
        }
        else {
            throw new IllegalArgumentException("Design deployment of unknown type: " + deployment.getClass().getName());
        }
        
        return oldDeployment;
        
    }
    /**
     * @return Returns the basePath.
     */
    public String getBasePath() {
        return _basePath;
    }
    /**
     * @param basePath The basePath to set.
     */
    public void setBasePath(String basePath) {
        this._basePath = basePath;
    }

    /**
     * @return Returns the manager.
     */
    public FileSystemDesignManager getManager() {
        return _manager;
    }

    /**
     * @param manager The manager to set.
     */
    public void setManager(FileSystemDesignManager manager) {
        _manager = manager;
    }

}
