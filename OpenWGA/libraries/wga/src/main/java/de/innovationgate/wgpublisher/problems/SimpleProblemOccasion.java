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

import de.innovationgate.webgate.api.WGException;

/**
 * A simple problem occasion that uses a string key to identify itself and qualify label keys.
 * Labels will be loaded with keys qualified by this occasion key, like: occasionkey.title, occasionkey.problemkey.message
 * The occasion is unique based on its problem scope and its occasion key. Clearing SimpleProblemOccasion therefor will clear all problems for this scope/occasion key combination.
 * Note: Unlike on custom occasions problems on SimpleProblemOccasions are obliged to always use the problem scope and type they were created with, as these are used to identify the scope.
 */
public class SimpleProblemOccasion implements ProblemOccasion, ProblemKeyQualificator {

    private boolean _clearedAutomatically;
    private String _occasionKey;
    private ProblemScope _scope;
    private Class<? extends ProblemType> _type;
    private Class<?> _refClass;
    
    /**
     * Creates an administrative occasion with global scope.
     * @param occKey The key, uniquely identifying this occasion
     * @param refClass The reference class. Usually the class containing the code that produces problems on this occasion. Its "*_problems.properties" file will be used to load problem labels (* being the class local name). 
     * @param clearedAutomatically Indicates if problems registered under this occasions will get cleared automatically. If false they have to be dismissed manually.
     * @return
     * @throws WGException
     */
    public static SimpleProblemOccasion createGlobalAdministrativeOccasion(String occKey, Class<?> refClass, boolean clearedAutomatically) {
        return new SimpleProblemOccasion(occKey, AdministrativeProblemType.class, GlobalScope.INSTANCE, refClass, clearedAutomatically);
    }

    /**
     * Creates an administrative occasion.
     * @param occKey The key, uniquely identifying this occasion
     * 
     * @param refClass The reference class. Usually the class containing the code that produces problems on this occasion. Its "*_problems.properties" file will be used to load problem labels (* being the class local name). 
     * @param clearedAutomatically Indicates if problems registered under this occasions will get cleared automatically. If false they have to be dismissed manually.
     * @return
     * @throws WGException
     */
    public static SimpleProblemOccasion createAdministrativeOccasion(String occKey, ProblemScope scope, Class<?> refClass, boolean clearedAutomatically) {
        return new SimpleProblemOccasion(occKey, AdministrativeProblemType.class, scope, refClass, clearedAutomatically);
    }
    

    /**
     * Creates an occasion with all parameters.
     * Also take note of the static methods that treat frequent use cases withg lesser parameters
     * @param occKey The key, uniquely identifying this occasion
     * @param type The problem type created by this occasion
     * @param scope The problem scope for problems created by this occasion.
     * @param refClass The reference class. Usually the class containing the code that produces problems on this occasion. Its "*_problems.properties" file will be used to load problem labels (* being the class local name).
     * @param clearedAutomatically Indicates if problems registered under this occasions will get cleared automatically. If false they have to be dismissed manually.
     * @throws WGException
     */
    public SimpleProblemOccasion(String occKey, Class<? extends ProblemType> type, ProblemScope scope, Class<?> refClass, boolean clearedAutomatically) {
        _occasionKey = occKey;
        _clearedAutomatically = clearedAutomatically;
        _type = type;
        _scope = scope;
        _refClass = refClass;
    }

    @Override
    public ProblemScope getDefaultScope() {
        return _scope;
    }

    @Override
    public Class<? extends ProblemType> getDefaultType() {
        return _type;
    }

    @Override
    public Class<?> getDefaultRefClass() {
        return _refClass;
    }

    @Override
    public boolean isClearedAutomatically() {
        return _clearedAutomatically;
    }

    @Override
    public String getBaseKey() {
        return _occasionKey;
    }
    
    

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_occasionKey == null) ? 0 : _occasionKey.hashCode());
        result = prime * result + ((_scope == null) ? 0 : _scope.hashCode());
        result = prime * result + ((_type == null) ? 0 : _type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SimpleProblemOccasion other = (SimpleProblemOccasion) obj;
        if (_occasionKey == null) {
            if (other._occasionKey != null)
                return false;
        }
        else if (!_occasionKey.equals(other._occasionKey))
            return false;
        if (_scope == null) {
            if (other._scope != null)
                return false;
        }
        else if (!_scope.equals(other._scope))
            return false;
        if (_type == null) {
            if (other._type != null)
                return false;
        }
        else if (!_type.equals(other._type))
            return false;
        return true;
    }

    
}
