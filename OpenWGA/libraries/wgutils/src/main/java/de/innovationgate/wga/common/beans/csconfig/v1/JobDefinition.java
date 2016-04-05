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

public class JobDefinition {
    
    public static final int TYPE_TMLSCRIPTMODULE = 1;
    public static final int TYPE_JAVA = 2;
    
    private String _name;
    private String _description;
    private String _resource;
    private int _type;
    private String schedule;
    public String getResource() {
        return _resource;
    }
    public void setResource(String resource) {
        _resource = resource;
    }
    public int getType() {
        return _type;
    }
    public void setType(int type) {
        _type = type;
    }
    public String getSchedule() {
        return schedule;
    }
    public void setSchedule(String schedule) {
        this.schedule = schedule;
    }
    public String getDescription() {
        return _description;
    }
    public void setDescription(String description) {
        _description = description;
    }
    public String getName() {
        return _name;
    }
    public void setName(String name) {
        _name = name;
    }

}
