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

package de.innovationgate.wgpublisher.plugins;

import java.io.File;

import de.innovationgate.wgpublisher.WGACore;

public class InstallPluginOperation implements WorkspaceOperation {
    
    private File _plugin;
    private int _updateStrategy;
    private String _replaceKey;

    public InstallPluginOperation(File plugin, int updateStrategy) {
        _plugin = plugin;
        _updateStrategy = updateStrategy;
    }

    public WGAPlugin perform(WGAPluginSet set, WGACore core, boolean workspaceOnly) throws PluginOperationException {
        try {
            WGAPlugin newPlugin = set.installPlugin(_plugin, _updateStrategy, workspaceOnly, false, _replaceKey);
            return newPlugin;
        }
        catch (PluginException e) {
            throw new PluginOperationException(this, "Error performing installation", e);
        }
    }

    protected void finalize() throws Throwable {
        _plugin.delete();
    }

    public File getPlugin() {
        return _plugin;
    }

    public void setPlugin(File plugin) {
        _plugin = plugin;
    }

    public int getUpdateStrategy() {
        return _updateStrategy;
    }

    public void setUpdateStrategy(int updateStrategy) {
        _updateStrategy = updateStrategy;
    }
    
    public String getTitle() {
        return "Installation";
    }

    public String getReplaceKey() {
        return _replaceKey;
    }

    public void setReplaceKey(String replaceKey) {
        _replaceKey = replaceKey;
    }

}
