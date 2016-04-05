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
package de.innovationgate.webgate.api;

/**
 * Contains data of a content relation. The targeted content is the currently released content with the structkey of {@link #getTargetStructkey()} and the language of {@link #getTargetLanguage()}
 */
public class WGRelationData {
    
    private WGContentKey _parentContentKey;
    
    private Object _targetStructkey;

    private String _targetLanguage;
    private int _type;

    private String _name;
    private String _group;

    private WGDatabase _database;
    
    public WGRelationData(WGDatabase database, WGContentKey parentContentKey, String name, Object structkey, String language, int type, String group) {
        super();
        _database = database;
        _parentContentKey = parentContentKey;
        _name = name;
        _targetStructkey = structkey;
        _targetLanguage = language;
        _type = type;
        _group = group;
    }
    
    public WGRelationData(WGDatabase database, WGContentKey parentContentKey, String name, Object structkey, String language, int type) {
        this(database, parentContentKey, name, structkey, language, type, null);
    }
    
    /**
     * Returns the struct key of the targeted content
     */
    public Object getTargetStructkey() {
        return _targetStructkey;
    }
    
    /**
     * Returns the language name of the targeted content
     */
    public String getTargetLanguage() {
        return _targetLanguage;
    }
    /**
     * Returns the content key of the content that owns this relation
     */
    public WGContentKey getParentContentKey() {
        return _parentContentKey;
    }
    /**
     * Returns the type of relation, a constant of WGContent.RELATIONTYPE_...
     */
    public int getType() {
        return _type;
    }
    /**
     * Returns the name of the relation
     */
    public String getName() {
        return _name;
    }

    /**
     * Returns the group name of the relation, null if it does not belong to a group
     */
    public String getGroup() {
        return _group;
    }

    protected WGDatabase getDatabase() {
        return _database;
    }
    
    /**
     * Returns the content which owns this relation
     * @throws WGAPIException
     */
    public WGContent getParentContent() throws WGAPIException {
        return getDatabase().getContentByKey(getParentContentKey());
    }
    
    /**
     * Retrieves the current target content of this relation
     * @throws WGAPIException
     */
    public WGContent getTargetContent() throws WGAPIException {
        
        WGStructEntry entry = getDatabase().getStructEntryByKey(getTargetStructkey());
        if (entry == null) {
            return null;
        }
        return entry.getReleasedContent(getTargetLanguage());
        
    }

}
