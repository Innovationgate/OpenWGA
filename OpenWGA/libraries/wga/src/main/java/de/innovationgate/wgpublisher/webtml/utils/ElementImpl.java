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
package de.innovationgate.wgpublisher.webtml.utils;

import de.innovationgate.webgate.api.WGAPIException;

/**
 * Base interface, used to implement the functionality of a tml:element. Defines the behaviour of the tag.
 */
public interface ElementImpl {

	/**
	 * Called, when the start tag of tml:element is rendered.
	 * @param context The context of the current tml element tag. Used to retrieve various information and to create output.
	 * @throws WGAPIException 
	 */
	public void begin(ElementImplContext context) throws WGAPIException;
	
	/**
	 * If the tml:element tag contains a tag tml:body, called on rendering of the start tag of tml:body.
	 * @param context The context of the current tml element tag. Used to retrieve various information and to create output.
	 * @return true, if the body of the tag tml:body should be rendered. false if the body of the tag should be skipped.
	 */
	public boolean beforeBody(ElementImplContext context) throws WGAPIException;
	
	/**
	 * If the tml:element tag contains a tag tml:body, called on rendering of the end tag of tml:body.
	 * @param context The context of the current tml element tag. Used to retrieve various information and to create output.
	 * @return true, if the body of the tag tml:body should be rendered again, false if there should be no further rendering on it.
	 * @throws WGAPIException 
	 */
	public boolean afterBody(ElementImplContext context) throws WGAPIException;
	
	/**
	 * Called, when the end  tag of tml:element is rendered
	 * @param context The context of the current tml element tag. Used to retrieve various information and to create output.
	 * @throws WGAPIException 
	 */
	public void end(ElementImplContext context) throws WGAPIException;
	
	/**
	 * Called, when another functionality requests tag informations from this element tag, using e.g. <tml:taginfo>. Can be used to provide 
	 * other tml functionalities with information from this tag. 
	 * @param name Name of the requested tag information.
	 * @return The information provided from this tag
	 */
	public Object tagInfo(String name);

}

