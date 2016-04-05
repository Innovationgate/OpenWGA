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
/**
 * A node in the page hierarchy of a database.
 * This interface is implemented by all WGAPI objects that build the page hierarchy of a database including WGStructEntry, WGArea and WGDatabase itself.
 * All these objects support the same methods to navigate in the hierarchy.
 * 
 */
package de.innovationgate.webgate.api;

import java.util.List;

import de.innovationgate.utils.SkippingIterator;

/**
 * Interface that represents a node in the page hierarchy of a WGA Content Store.
 */
public interface PageHierarchyNode {
    
    /**
     * Returns the child nodes of this hierarchy node
     * @throws WGAPIException
     */
    public List<? extends PageHierarchyNode> getChildNodes() throws WGAPIException;
    
    /**
     * Returns an iterator for the child nodes of this hierarchy node
     * @param pageSize The size of a fetch page, if the implementation fetches childnodes page-wise
     * @throws WGAPIException
     */
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException;
    
    /**
     * Return the class type of child nodes 
     */
    public Class<?> getChildNodeType();
    
    /**
     * Return the parent of this node in hierarchy. Return null at the root level.
     * @throws WGAPIException
     */
    public PageHierarchyNode getParentNode() throws WGAPIException;
    
    /**
     * Returns the display title for the node. If multiple titles for multiple languages exist we choose a specific one for the given language.
     * @param language The language whose title is wanted
     * @return The title
     * @throws WGAPIException
     */
    public String getNodeTitle(String language) throws WGAPIException;
    
    /**
     * Returns a string key for the node that is unique among all keys of PageHierarchyNodes in this database.
     * @throws WGAPIException
     */
    public String getNodeKey() throws WGAPIException;

}
