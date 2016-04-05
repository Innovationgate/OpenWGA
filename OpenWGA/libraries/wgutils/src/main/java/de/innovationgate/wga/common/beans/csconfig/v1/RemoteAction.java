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

package de.innovationgate.wga.common.beans.csconfig.v1;

import java.util.ArrayList;
import java.util.List;

public class RemoteAction {

    private String moduleName;
    private List callers = new ArrayList();
    private int callerLevel;
    public int getCallerLevel() {
        return callerLevel;
    }
    public void setCallerLevel(int callerLevel) {
        this.callerLevel = callerLevel;
    }
    public List getCallers() {
        return callers;
    }
    public void setCallers(List callers) {
        this.callers = callers;
    }
    public String getModuleName() {
        return moduleName;
    }
    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }
    
}
