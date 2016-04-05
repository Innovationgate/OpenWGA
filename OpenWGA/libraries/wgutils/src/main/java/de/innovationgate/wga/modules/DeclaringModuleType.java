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

package de.innovationgate.wga.modules;

/**
 * A module type that declares its mode of usage when creating module definitions. This can be tested by the module registry, therefor identifying wrongly implemented module definitions.
 * Also module developers can use this as a help on how to implement the module.
 */
public interface DeclaringModuleType extends ModuleType {
    
    /**
     * Declares if modules of this type need to be key based, therefor need to implement the {@link KeyBasedModuleDefinition}.
     * Types returning false here still may optionally implement this interface.
     */
    public boolean isKeyBased();
    
    /**
     * Declares if module definitions of this type typically should register their definition class as implementation class.
     * This indicates that the implementation class is not used and the meaning of the type is something different than to register such a class. 
     */
    public boolean isSelfRegistered();
    
    /**
     * Declares a base class for all implementation classes of this module type. Return null to declare no base class.
     */
    public Class<? extends Object> getImplementationBaseClass();    
    
    /**
     * Declares if module definitions of this type need to declare a property object to be returned by {@link ModuleDefinition#getProperties()}
     */
    public boolean isPropertiesNeeded();
    
    /**
     * Returns the type of the property object that needs to be returned by {@link ModuleDefinition#getProperties()}. Return null to declare no property class.
     */
    public Class<? extends Object> getPropertyClass();

}
