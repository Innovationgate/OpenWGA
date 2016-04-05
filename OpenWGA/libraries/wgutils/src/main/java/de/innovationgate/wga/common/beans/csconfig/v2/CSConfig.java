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

package de.innovationgate.wga.common.beans.csconfig.v2;

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.utils.XStreamUtils;

public class CSConfig extends de.innovationgate.wga.common.beans.csconfig.v1.CSConfig {
    
    private List<Shortcut> shortcuts = new ArrayList<Shortcut>();

    public List<Shortcut> getShortcuts() {
        return shortcuts;
    }

    public void setShortcuts(List pluginShortcuts) {
        this.shortcuts = pluginShortcuts;
    }

    @Override
    public void importOverlayConfig(de.innovationgate.wga.common.beans.csconfig.v1.CSConfig overlayConfig) {
        super.importOverlayConfig(overlayConfig);
        if (overlayConfig instanceof CSConfig) {
            CSConfig v2config = (CSConfig) overlayConfig;
            
            for (Shortcut shortCut : v2config.getShortcuts()) {
                
                switch (shortCut.getType()) {
                    
                    
                    case Shortcut.TYPE_PLUGIN:
                    case Shortcut.TYPE_ITEM_MAPPING: 
                    case Shortcut.TYPE_META_MAPPING:
                    case Shortcut.TYPE_TMLSCRIPT_GLOBAL: {
                        shortcuts.add(shortCut);
                        break;
                    }
                    
                }
                
            }
            
            shortcuts.addAll(v2config.getShortcuts());
        }
        
    }

}
