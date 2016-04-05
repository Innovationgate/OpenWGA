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

package de.innovationgate.wga.common;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Annotation to customize code completion for OpenWGA developer studio
 */
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.CONSTRUCTOR, ElementType.TYPE})
public @interface CodeCompletion {
    
    /**
     * "Blacklist" mode: Everything is available for code completion that is not explicitly excluded 
     */
    public static final String MODE_EXCLUDE = "exclude";
    
    
    /**
     * "Whitelist" mode: Only explicitly included methods/fields are available for code completion 
     */
    public static final String MODE_INCLUDE = "include";
    
    public static final String BEAN_MODE_LOWERCASED = "beanModeLowercased";
    public static final String BEAN_MODE_ALL = "beanModeAll";
    public static final String BEAN_MODE_NONE = "beanModeNone";

    String methodMode() default MODE_EXCLUDE;
    
    String propertyMode() default MODE_INCLUDE;

    String beanMode() default BEAN_MODE_LOWERCASED;
    
    String preferredCase() default "";
    
    boolean isProperty() default false;
    
    Class<? extends Object> delegate() default CodeCompletion.class; // As we cannot default this to null we will take a setting to CodeCompletion as "no delegate"
    
    // defines which bean getter/setter should be available as properties (lowercased, all or none)
    String beanPropertiesMode() default BEAN_MODE_LOWERCASED;
}
