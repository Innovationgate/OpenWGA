/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General public abstract License as published by
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
 * GNU General public abstract License for more details.
 * 
 * You should have received a copy of the GNU General public abstract License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.cc;

import java.util.List;
import java.util.PropertyResourceBundle;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.Plugin;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion()
public abstract class Design extends de.innovationgate.wga.server.api.Design {
    
    public String baseReference;
    public de.innovationgate.wgpublisher.hdb.HDBModel HDBModel;

    // Just so the class may compile
    protected Design(WGA wga, String dbKey) throws WGException {
        super(wga, dbKey);
    }
    public abstract Object callAction(String actionId, Object... params);
    public abstract Object callAction(TMLContext context, String actionId, Object... params);
    
    public abstract Object createObject(String moduleName, Object... params);
    public abstract Object createObject(Object objectDefinition, Object... params);
    
    public abstract PropertyResourceBundle getLabelBundle(String container, String file, String language);
    public abstract PropertyResourceBundle getLabelBundle(String file, String language);
    public abstract PropertyResourceBundle getLabelBundle(String language);
    
    public abstract boolean isCustomized();
    
    public abstract Object loadObjectDefinition(String moduleName);
    
    public abstract String label(String containerName, String fileName, String key, List<String> params);
    public abstract String label(String fileName, String key, List<String> params);
    public abstract String label(String key, List<String> params);
    
    public abstract String label(String containerName, String fileName, String key);
    public abstract String label(String fileName, String key);
    public abstract String label(String key);
    
    public abstract WGDatabase db();
    
    public abstract Object getGlobal(String name);
    
    public abstract void registerGlobal(String globalName, Object reference);
    public abstract void unregisterGlobal(String globalName);
    public abstract void registerAppGlobal(String globalName, Object reference);
    
    public abstract Plugin plugin();
    
}