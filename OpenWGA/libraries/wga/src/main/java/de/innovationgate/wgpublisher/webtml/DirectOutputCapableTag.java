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

package de.innovationgate.wgpublisher.webtml;

/**
 * Interfaces for tags that are capable of writing their output directly to the output stream.
 * Tags that denote via method {@link #isDirectOutputCapable()} that their content can be directly put out must:
 * <ul>
 * <li>Have decided if they do output their tag content after method tmlEndTag(). It is when this method is called.
 * <li>Not want to modify their output in any special  way (encode and format excluded)
 * <li>Have the tag prefix ready after tmlStartTag()
 * <li>Not do not manually write anything to the result field of BaseTagStatus or use appendResult() for it without having a divider
 * <li>Use method tmlInitBody
 * <li>Use iteration
 * </ul>  
 */
public interface DirectOutputCapableTag {
    
    public boolean isDirectOutputCapable();

}
