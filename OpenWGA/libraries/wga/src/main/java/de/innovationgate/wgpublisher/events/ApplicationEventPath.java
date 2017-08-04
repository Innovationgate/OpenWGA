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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.WGUtils;

public class ApplicationEventPath implements EventPath {
    
    public static final String EVENTTYPE = "application";
    
    private String _db;
    private List<EventPathEntry> _path;

    public ApplicationEventPath(String db, List<EventPathEntry> path) {
        _db = db;
        _path = path;
    }
    
    public void setDbKey(String dbkey){
    	_db = dbkey;
    }
    
    @Override
    public EventPathEntry[] getEventHierarchy() {

        EventPathEntry[] entries = new EventPathEntry[2 + _path.size()];
        entries[0] = new EventPathEntry(EventPathEntry.ENTRYKEY_DB, _db);
        entries[1] = new EventPathEntry(EventPathEntry.ENTRYKEY_TYPE, EVENTTYPE);
        for (int idx = 0; idx < _path.size(); idx ++) {
            entries[idx +2] = _path.get(idx);
        }
        
        return entries;
        
        
    }
    
    public EventPathEntry[] getApplicationEventHierarchy() {

        EventPathEntry[] entries = new EventPathEntry[_path.size()];
        for (int idx = 0; idx < _path.size(); idx ++) {
            entries[idx] = _path.get(idx);
        }
        
        return entries;
        
        
    }
    
    public static List<EventPathEntry> parseQualifiers(List<? extends Object> qualifiers) {

        List<EventPathEntry> pathEntries = new ArrayList<EventPathEntry>();
        for (Object q : qualifiers) {
            
            if (q instanceof String) {
                
                String qStr = (String) q;
                int equalPos = qStr.indexOf("=");
                if (equalPos != -1) {
                    String name = qStr.substring(0, equalPos).trim();
                    String value = qStr.substring(equalPos+1).trim();
                    pathEntries.add(new EventPathEntry(name, value));
                }
                else {
                    pathEntries.add(new EventPathEntry(EventPathEntry.ENTRYKEY_QUALIFIER, qStr.trim()));   
                }
            
            }
            else if (q instanceof Map<?,?>) {
                
                Map<?,?> qMap = (Map<?,?>) q;
                if (qMap.size() != 1) {
                    throw new IllegalArgumentException("Only Maps/Lookup Tables/Objects with exactly one entry can be used as event path entry");
                }
                Map.Entry<?,?> qEntry = qMap.entrySet().iterator().next();
                pathEntries.add(new EventPathEntry(String.valueOf(qEntry.getKey()), String.valueOf(qEntry.getValue())));
                
            }
            else {
                throw new IllegalArgumentException("Invalid type to define an event path: " + q.getClass().getName());
            }
            
        }
        return pathEntries;
        
        
    }
    
    @Override
    public String toString() {

        List<String> elements = new ArrayList<>();
        for (EventPathEntry entry : _path) {
            elements.add(entry.toString());
        }
        return WGUtils.serializeCollection(elements, "/");
        
    }

}
