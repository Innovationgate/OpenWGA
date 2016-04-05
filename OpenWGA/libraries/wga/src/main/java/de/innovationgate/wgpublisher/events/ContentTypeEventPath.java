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

package de.innovationgate.wgpublisher.events;

import de.innovationgate.webgate.api.WGContentKey;

public class ContentTypeEventPath implements EventPath {
    
    public static final String EVENTTYPE = "content";
	
	private String _event;
	private String _db;
	private String _contenttype;
	private WGContentKey _contentkey;
	
	ContentTypeEventPath(String event, String db, String contenttype, WGContentKey contentkey) {
		_event = event;
		_db = db;
		_contenttype = contenttype;
		_contentkey = contentkey;
	}
	
	
	public EventPathEntry[] getEventHierarchy() {
	    EventPathEntry[] entryArray;
	    
	    // Event path for a specific content
	    if (_contentkey != null) {
			entryArray = new EventPathEntry[7];
			entryArray[0] = new EventPathEntry(EventPathEntry.ENTRYKEY_DB, _db);
			entryArray[1] = new EventPathEntry(EventPathEntry.ENTRYKEY_TYPE, EVENTTYPE);
			entryArray[2] = new EventPathEntry(EventPathEntry.ENTRYKEY_EVENT, _event);
			entryArray[3] = new EventPathEntry(EventPathEntry.ENTRYKEY_CONTENTTYPE, _contenttype);
			entryArray[4] = new EventPathEntry(EventPathEntry.ENTRYKEY_PAGEKEY, String.valueOf(_contentkey.getStructKey()));
			entryArray[5] = new EventPathEntry(EventPathEntry.ENTRYKEY_LANGUAGE, _contentkey.getLanguage());
			entryArray[6] = new EventPathEntry(EventPathEntry.ENTRYKEY_VERSION, String.valueOf(_contentkey.getVersion()));
	    }
	    
	    /// Event path for all contents of the given type
	    else {
            entryArray = new EventPathEntry[5];
            entryArray[0] = new EventPathEntry(EventPathEntry.ENTRYKEY_DB, _db);
            entryArray[1] = new EventPathEntry(EventPathEntry.ENTRYKEY_TYPE, "content");
            entryArray[2] = new EventPathEntry(EventPathEntry.ENTRYKEY_EVENT, _event);
            entryArray[3] = new EventPathEntry(EventPathEntry.ENTRYKEY_CONTENTTYPE, _contenttype);
            entryArray[4] = new EventPathEntry(EventPathEntry.ENTRYKEY_PAGEKEY, "*");
	    }
		return entryArray;
				
	}

}