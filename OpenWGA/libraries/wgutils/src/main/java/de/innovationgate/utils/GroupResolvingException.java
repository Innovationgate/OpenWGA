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

/**
 * Thrown by subclasses of {@link AnyTypeGroupMembershipResolver} when an error occured on
 * determining direct group membership
 */
public class GroupResolvingException extends Exception {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Throwable _cause;

    public GroupResolvingException(String msg) {
        super(msg);
    }
    
    public GroupResolvingException(String msg, Throwable cause) {
        super(msg);
        _cause = cause;
    }

    /**
     * @return Returns the cause.
     */
    public Throwable getCause() {
        return _cause;
    }

    /* (non-Javadoc)
     * @see java.lang.Throwable#getMessage()
     */
    public String getMessage() {
        if (_cause != null) {
            return super.getMessage() + " (Cause: " + _cause.getClass().getName() + " - " + _cause.getMessage() + ")";
        }
        else {
            return super.getMessage();
        }
    }

}
