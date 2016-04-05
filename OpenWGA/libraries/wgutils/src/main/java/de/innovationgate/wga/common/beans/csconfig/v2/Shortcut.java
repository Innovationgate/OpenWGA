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

public class Shortcut {
    
    public static final int TYPE_PLUGIN = 1;
    public static final int TYPE_TMLSCRIPT_GLOBAL = 2;
    public static final int TYPE_ITEM_MAPPING = 3;
    public static final int TYPE_META_MAPPING = 4;
    
    private String reference;
    private int type;
    private String shortcut;
    
    public String getReference() {
        return reference;
    }
    public void setReference(String uniqueName) {
        this.reference = uniqueName;
    }
    public String getShortcut() {
        return shortcut;
    }
    public void setShortcut(String shortcut) {
        this.shortcut = shortcut;
    }
    public int getType() {
        return type;
    }
    public void setType(int type) {
        this.type = type;
    }


}
