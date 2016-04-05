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

package de.innovationgate.wga.modules.options;

/**
 * Defines an option that the current option is dependent on.
 * Dependency means: The current option is only usable if the given option in this object is available and (optionally) contains the given value in this object
 *
 */
public class DependentOption {
    
    private String _name = null;
    private String _neededValue = null;
    
    /**
     * Returns the name of the option we depend on
     */
    public String getName() {
        return _name;
    }

    /**
     * Sets the name of the option we depend on
     */
    public void setName(String name) {
        _name = name;
    }
    
    /**
     * Returns the needed value of the option we depend on. If this isd null there is no needed value and the option just needs to be there
     */
    public String getNeededValue() {
        return _neededValue;
    }

    /**
     * Sets the needed value of the option we depend on. Specify null if there is no needed value and the option just needs to be there
     */
    public void setNeededValue(String neededValue) {
        _neededValue = neededValue;
    }
    
    
    /**
     * Tests if the option we depends on needs to have a special value or just needs to be there
     */
    public boolean hasNeededValue() {
        return (_neededValue != null);
    }

}
