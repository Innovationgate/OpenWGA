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

import java.util.ArrayList;

import de.innovationgate.wga.server.api.tml.Context;

/**
 * A page of collection result documents
 */
public class CollectionPage extends ArrayList<Context> {
    
    private boolean _endReached = false;
    private int _endIndex = -1;

    /**
     * Sets the end index of the page
     */
    protected void setEndIndex(int endIndex) {
        _endIndex = endIndex;
    }

    /**
     * Denotes if there are retrievable result documents in the collection after the last document of this collection page
     */
    public boolean isEndReached() {
        return _endReached;
    }

    /**
     * Sets if the end was reached
     */
    protected void setEndReached(boolean endReached) {
        _endReached = endReached;
    }

    /**
     *  Denotes the index that the last document on this collection page had on the original collection
     *  The first document on a collection has the index 1. This index includes all returned documents, including unreadable, non-visible or otherwise filtered documents so you might not actually retrieve all documents on all indices.
     */
    public int getEndIndex() {
        return _endIndex;
    }
    
    /**
     * Denotes the index that the first document on this collection page had on the original collection
     * The first document on a collection has the index 1. This index includes all returned documents, including unreadable, non-visible or otherwise filtered documents so you might not actually retrieve all documents on all indices.
     */
    public int getStartIndex() {
        if (_endIndex != -1) {
            return _endIndex - (size()-1);
        }
        else {
            return -1;
        }
    }
    
    

}
