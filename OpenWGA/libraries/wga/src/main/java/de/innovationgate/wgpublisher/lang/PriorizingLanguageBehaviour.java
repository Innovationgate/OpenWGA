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

package de.innovationgate.wgpublisher.lang;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * A language behaviour able to determine a priority order of contents language-wise
 */
public interface PriorizingLanguageBehaviour extends LanguageBehaviour {

    /**
     * Orders the given contents in the order they should be priorized language-wise inside of a WebTML request
     * @param entry The entry whose publishable contents to order
     * @param context The WebTML context for whom to decide
     * @param isBI True if authoring mode is enabled, i.E. draft documents may be returned
     * @return A new list giving the contents in the determined priority order
     * @throws WGAPIException
     */
    public List<WGContent> webtmlSelectPriorityOrder(WGStructEntry entry, TMLContext context, boolean isBI) throws WGAPIException;

    /**
     * Orders the given contents in the order they should be priorized language-wise for a request
     * @param entry The entry whose publishable contents to order
     * @param req The servlet request for whom to decide
     * @param isBI True if authoring mode is enabled, i.E. draft documents may be returned
     * @return A new list giving the contents in the determined priority order
     * @throws WGAPIException
     */
    public List<WGContent> requestSelectPriorityOrder(WGStructEntry entry, HttpServletRequest req, boolean isBI) throws WGAPIException;
    
}
