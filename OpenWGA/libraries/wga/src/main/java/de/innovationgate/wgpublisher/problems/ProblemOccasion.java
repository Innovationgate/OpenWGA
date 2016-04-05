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


/**
 * An occasion at which problems might be registered.
 * Implementors of this interface need to be equality-checkable against equal occasion instances,
 * either by implementing hashCode()/equals() or using the same instances for the same occasions. 
 */
public interface ProblemOccasion extends ProblemQueueEvent {
    
    /**
     * Returns the default problem scope used by problems of this occasion
     */
    public ProblemScope getDefaultScope();
    
    /**
     * Returns the default problem type used by problems of this occasion
     */
    public Class<? extends ProblemType> getDefaultType();
    
    /**
     * Returns a reference class for this occasion, normally the class whose operations may register problems of this occasion.
     * The problem message texts will be loaded from a file "<i>classname</i>_problems.properties" in the package folder of this class.
     */
    public Class<?> getDefaultRefClass();
    
    /**
     * Identifies if problems of this occasion are automatically cleared once the problem is solved. Otherwise problems will be "dismissable" by the administrator.
     */
    public boolean isClearedAutomatically();

}
