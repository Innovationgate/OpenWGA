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

package de.innovationgate.wga.server.api;

import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;

/**
 * Configures descriptification of TMLScript objects
 */
public class DescriptificationConfig {
    
    /**
     * Determines how JS objects and array are converted to corresponding Java types
     */
    public enum ObjectMode {
        /**
         * Convert JS objects and arrays to Gson JSON objects
         * Pro: Works symmetricly on {@link RhinoExpressionEngine#scriptify(Object, Object)}, i.e. JSON objects get cleanly transferred back, Smaller serialisation payload
         * Con: Contained Non-JSON objects raise errors 
         */
        TO_JSON, 
        /**
         * Convert JS objects to Maps and JS arrays to Lists (The default)
         * Pro: Also descriptifies contained object into Java Counterparts,
         * Con: Will not be transferred back into the type when using {@link RhinoExpressionEngine#scriptify(Object, Object)}. Large serialisation payload
         */
        TO_JAVA_COLLECTIONS
    }
    
    private ObjectMode _objectMode = ObjectMode.TO_JAVA_COLLECTIONS;
    private boolean _forceDescriptification = false;
    
    /**
     * Returns the object descriptifaction mode
     */
    public ObjectMode getObjectMode() {
        return _objectMode;
    }
    /**
     * Sets the object descriptifaction mode, i.e. how JS objects and arrays
     * are converted to Java classes.
     * @param objectMode
     */
    public DescriptificationConfig setObjectMode(ObjectMode objectMode) {
        _objectMode = objectMode;
        return this;
    }
    /**
     * Returns if descriptification is forced
     */
    public boolean isForceDescriptification() {
        return _forceDescriptification;
    }
    
    /**
     * @param Sets if descriptification is forced, defaults to false. If true descriptification
     * will cancel if some objects down in object hierarchy was not descriptifiable
     */
    public DescriptificationConfig setForceDescriptification(boolean forceDescriptification) {
        _forceDescriptification = forceDescriptification;
        return this;
    }
    
    /**
     * Sets object mode {@value ObjectMode#TO_JSON}
     */
    public DescriptificationConfig convertObjectsToJSON() {
        return setObjectMode(ObjectMode.TO_JSON);
    }
    
    /**
     * Sets object mode {@value ObjectMode#TO_JAVA_COLLECTIONS}
     */
    public DescriptificationConfig convertObjectsToJavaCollections() {
        return setObjectMode(ObjectMode.TO_JAVA_COLLECTIONS);
    }

}
