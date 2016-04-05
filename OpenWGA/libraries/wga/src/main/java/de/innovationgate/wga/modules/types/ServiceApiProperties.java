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

package de.innovationgate.wga.modules.types;

import de.innovationgate.wga.server.api.WGA;

/**
 * Properties of service API registrations
 */
public class ServiceApiProperties {
    
    public static interface Fetcher {
        public Object fetchService(WGA wga);
    }
    
    private Class _defaultImplementation = null;
    private boolean _implementable = true;
    private boolean _optionalConfig = false;
    private Fetcher _fetcher = null;

    /**
     * Returns the default implementation of this service
     */
    public Class getDefaultImplementation() {
        return _defaultImplementation;
    }

    /**
     * Returns the default implementation of this service.
     * @param defaultImplementation
     */
    public void setDefaultImplementation(Class defaultImplementation) {
        _defaultImplementation = defaultImplementation;
    }

    /**
     * Returns if the implementation choice setting in server config should be optional. Defaults to false.
     */
    public boolean isOptionalConfig() {
        return _optionalConfig;
    }

    /**
     * Sets if the implementation choice setting in server config should be optional
     */
    public void setOptionalConfig(boolean optionalConfig) {
        _optionalConfig = optionalConfig;
    }
    
    /**
     * Returns if this service is implementable. If false it will always use the default implementation. Defaults to true.
     */
    public boolean isImplementable() {
        return _implementable;
    }
    
    /**
     * Sets if this service is implementable. If false it will always use the default implementation.
     */
    public void setImplementable(boolean implementable) {
        _implementable = implementable;
    }

    /**
     * Returns the service fetcher instance, null if no fetcher is used
     */
    public Fetcher getFetcher() {
        return _fetcher;
    }

    /**
     * Sets a service fetcher instance, null if no fetcher is used
     */
    public void setFetcher(Fetcher fetcher) {
        _fetcher = fetcher;
    }

}
