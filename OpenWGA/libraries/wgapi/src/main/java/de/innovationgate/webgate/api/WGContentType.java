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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.fake.WGFakeTML;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.webgate.api.schemadef.WGContentTypeDefinition;
import de.innovationgate.webgate.api.utils.MasterSessionTask;

/**
 * Formerly referred to as "Doctype", this is a document describing a type of
 * content document. It decides, which design should be used to display it, who
 * may create content of that type and which workflow is needed, before a
 * content document of that type is released.
 */
public class WGContentType extends WGSchemaDocument implements PageHierarchyNode {  

    /**
     * Content type is allowed everywhere.
     */
    public static final String POSITIONING_EVERYWHERE = "all";

    /**
     * Content type is allowed in all root entries.
     */
    public static final String POSITIONING_ROOTENTRIES = "root";

    /**
     * Content type is allowed in all child entries
     */
    public static final String POSITIONING_CHILDENTRIES = "child";

    /**
     * Content type is allowed under special parent documents, specified as
     * allowed positions
     */
    public static final String POSITIONING_FIXEDPARENTS = "fixParentDocs";

    /**
     * Content type is allowed under parent documents with special content
     * types, specified as allowed positions
     */
    public static final String POSITIONING_FIXEDPARENTTYPES = "fixDocTypes";
    
    public static final String META_WORKFLOW = "WORKFLOW";
    public static final MetaInfo METAINFO_WORKFLOW = new MetaInfo(META_WORKFLOW, String.class, null);

    public static final String META_OUTER_LAYOUT = "OUTERLAYOUT";
    public static final MetaInfo METAINFO_OUTER_LAYOUT = new MetaInfo(META_OUTER_LAYOUT, String.class, null);

    public static final String META_INNER_LAYOUT = "INNERLAYOUT";
    public static final MetaInfo METAINFO_INNER_LAYOUT = new MetaInfo(META_INNER_LAYOUT, String.class, null);

    public static final String META_EDITORS = "EDITORS";
    public static final MetaInfo METAINFO_EDITORS = new MetaInfo(META_EDITORS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_EDITORS.setMultiple(true); };

    public static final String META_ALLOWED_POSITIONS = "ALLOWEDPOSITIONS";
    public static final MetaInfo METAINFO_ALLOWED_POSITIONS = new MetaInfo(META_ALLOWED_POSITIONS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_ALLOWED_POSITIONS.setMultiple(true); };

    public static final String META_POSITIONING = "POSITIONING";
    public static final MetaInfo METAINFO_POSITIONING = new MetaInfo(META_POSITIONING, String.class, POSITIONING_EVERYWHERE);
    static { 
        METAINFO_POSITIONING.addAllowedValue(POSITIONING_CHILDENTRIES);
        METAINFO_POSITIONING.addAllowedValue(POSITIONING_EVERYWHERE);
        METAINFO_POSITIONING.addAllowedValue(POSITIONING_FIXEDPARENTS);
        METAINFO_POSITIONING.addAllowedValue(POSITIONING_FIXEDPARENTTYPES);
        METAINFO_POSITIONING.addAllowedValue(POSITIONING_ROOTENTRIES);
    };
    
    public static final String META_CHILDPAGERESTRICTIONS = "CHILDPAGERESTRICTIONS";
    public static final MetaInfo METAINFO_CHILDPAGERESTRICTIONS = new MetaInfo(META_CHILDPAGERESTRICTIONS, String.class, PAGERESTRICTION_ANY);
    static {
        METAINFO_CHILDPAGERESTRICTIONS.setExtdata(true);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_ANY);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_NONE);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_FIXEDTYPES);
    }
    
    public static final String META_ALLOWED_CHILDTYPES = "ALLOWEDCHILDTYPES";
    public static final MetaInfo METAINFO_ALLOWED_CHILDTYPES = new MetaInfo(META_ALLOWED_CHILDTYPES, String.class, Collections.EMPTY_LIST);
    static {
        METAINFO_ALLOWED_CHILDTYPES.setMultiple(true);
        METAINFO_ALLOWED_CHILDTYPES.setExtdata(true);
    }
    

    public static final String META_EVENT_CREATECONTENT = "EVENTCREATECONTENT";
    public static final MetaInfo METAINFO_EVENT_CREATECONTENT = new MetaInfo(META_EVENT_CREATECONTENT, String.class, null);
    static {
        METAINFO_EVENT_CREATECONTENT.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    }

    public static final String META_EVENT_SAVECONTENT = "EVENTSAVECONTENT";
    public static final MetaInfo METAINFO_EVENT_SAVECONTENT = new MetaInfo(META_EVENT_SAVECONTENT, String.class, null);
    static {
        METAINFO_EVENT_SAVECONTENT.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    }

    public static final String META_EVENT_WORKFLOWMAIL = "EVENTWORKFLOWMAIL";
    public static final MetaInfo METAINFO_EVENT_WORKFLOWMAIL = new MetaInfo(META_EVENT_WORKFLOWMAIL, String.class, null);
    static {
        METAINFO_EVENT_WORKFLOWMAIL.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    

    public static final String META_EVENT_STATUSCHANGE = "EVENTSTATUSCHANGE";
    public static final MetaInfo METAINFO_EVENT_STATUSCHANGE = new MetaInfo(META_EVENT_STATUSCHANGE, String.class, null);
    static {
        METAINFO_EVENT_STATUSCHANGE.setExtdata(true);
    }


    public static final String META_NAMEALIASES = "NAMEALIASES";
    public static final MetaInfo METAINFO_NAMEALIASES = new MetaInfo(META_NAMEALIASES, String.class, Collections.EMPTY_LIST);
    static {
        METAINFO_NAMEALIASES.setMultiple(true);
        METAINFO_NAMEALIASES.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    };
    
    public static final String META_DESCRIPTIONALIASES = "DESCRIPTIONALIASES";
    public static final MetaInfo METAINFO_DESCRIPTIONALIASES = new MetaInfo(META_DESCRIPTIONALIASES, String.class, Collections.EMPTY_LIST);
    static {
        METAINFO_DESCRIPTIONALIASES.setMultiple(true);
        METAINFO_DESCRIPTIONALIASES.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    };

    public static final String META_PREFERREDPARENT = "PREFERREDPARENT";
    public static final MetaInfo METAINFO_PREFERREDPARENT = new MetaInfo(META_PREFERREDPARENT, Object.class, null);
    
    public static final String META_AUTHORING = "AUTHORING";
    public static final MetaInfo METAINFO_AUTHORING = new MetaInfo(META_AUTHORING, Boolean.class, Boolean.TRUE);
    static {
        METAINFO_AUTHORING.setExtdata(true);
        METAINFO_AUTHORING.setMinCsVersion(5.0);
    }
    
    /**
     * Constructor. Should not be used outside the WGAPI.
     * @throws WGAPIException 
     */
    public WGContentType(WGDatabase db, WGDocumentCore doc) throws WGAPIException {
        this(db, doc, new WGDocumentObjectFlags());
    }

    public WGContentType(WGDatabase wgDatabase, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(wgDatabase, doc, flags);
    }

    /*
     * @see WGDocument#dropCache()
     */
    public void dropCache() throws WGAPIException {
        super.dropCache();
        dropRelations();
    }

    protected void dropRelations() {
    }

    /**
     * Returns the name of this content type.
     * @throws WGAPIException 
     */
    public String getName() throws WGAPIException {
        return String.valueOf(this.getMetaData(META_NAME));
    }
    
    /**
     * Returns a content type name for the specified language.
     * This method first searches the aliases for a matching name.
     * If none is found the default name of the content type is returned.
     * @param lang
     * @return The content type name  in the given language or default if not available in this language
     * @throws WGAPIException
     */
    public String getNameForLanguage(String lang) throws WGAPIException {
        
        Iterator aliases = getNameAliases().iterator();
        while (aliases.hasNext()) {
            String alias = (String) aliases.next();
            if (alias.startsWith(lang + ":")) {
                return alias.substring(alias.indexOf(":") + 1).trim();
            }
        }
        
        return getName();
        
    }
    
    /**
     * Returns a content type description for the specified language.
     * This method first searches the aliases for a matching description.
     * If none is found the default description of the content type is returned.
     * @param lang
     * @return The content type description in the given language or default if not available in this language
     * @throws WGAPIException
     */
    public String getDescriptionForLanguage(String lang) throws WGAPIException {
        
        Iterator aliases = getDescriptionAliases().iterator();
        while (aliases.hasNext()) {
            String alias = (String) aliases.next();
            if (alias.startsWith(lang + ":")) {
                return alias.substring(alias.indexOf(":") + 1).trim();
            }
        }
        
        return getDescription();
        
    }

    /**
     * Returns the outer layout for this content type.
     * 
     * @param mediaKey
     *            The media key of the needed layout.
     * @return WGTMLModule
     * @throws WGAPIException 
     */
    public WGTMLModule getOuterLayout(String mediaKey) throws WGAPIException {

        String tmlName = (String) this.getMetaData(META_OUTER_LAYOUT);
        if (tmlName == null) {
            return null;
        }

        WGTMLModule layout = (WGTMLModule) this.db.getDesignObject(WGDocument.TYPE_TML, tmlName, mediaKey);
        
        if (layout != null) {
            return layout;
        }
        else {
            layout =(WGTMLModule) db.getOrCreateDesignDocumentObject(new WGFakeTML(db, String.valueOf(this.getMetaData(META_OUTER_LAYOUT)), mediaKey),  new WGDocumentObjectFlags().setDummy(true));
            return layout;
        }
    }
    
    /**
     * Returns the name of WebTML modules used as outer layouts for this content type
     * @throws WGAPIException
     */
    public String getOuterLayoutName() throws WGAPIException {
        return (String) this.getMetaData(META_OUTER_LAYOUT);
    }
    
    /**
     * Returns the name of WebTML modules used as inner layouts for this content type
     * @throws WGAPIException
     */
    public String getInnerLayoutName() throws WGAPIException {
        return (String) this.getMetaData(META_INNER_LAYOUT);
    }

    /**
     * Sets the outer layout for this content type. Only the name of the given
     * tml module is taken. The media key will not be fixed to the modules' one.
     * 
     * @param mod
     *            The module, whose name will be taken as outer layout.
     * @throws WGAPIException 
     */
    public boolean setOuterLayout(WGTMLModule mod) throws WGAPIException {

        if (mod == null) {
            return false;
        }

        String modName = mod.getName();
        return setOuterLayoutName(modName);

    }

    /**
     * Sets the outer layout for this content type by specifying the name without reference to the document
     * @param modName The name of the WebTML module representing outer layout 
     */
    public boolean setOuterLayoutName(String modName) throws WGAPIException {
        return setMetaData(META_OUTER_LAYOUT, modName);
    }
 
    /**
     * Returns the inner layout for this content type.
     * 
     * @param mediaKey
     *            The media key of the needed layout
     * @return WGTMLModule
     * @throws WGAPIException 
     */
    public WGTMLModule getInnerLayout(String mediaKey) throws WGAPIException {
        
        String tmlName = (String) this.getMetaData(META_INNER_LAYOUT);
        if (tmlName == null) {
            return null;
        }

        WGTMLModule layout = (WGTMLModule) this.db.getDesignObject(WGDocument.TYPE_TML, tmlName, mediaKey);

        if (layout != null) {
            return layout;
        }
        else {
            layout = (WGTMLModule) db.getOrCreateDesignDocumentObject(new WGFakeTML(db, String.valueOf(this.getMetaData(META_INNER_LAYOUT)), mediaKey), new WGDocumentObjectFlags().setDummy(true));
            return layout;
        }

    }

    /**
     * Sets the inner layout for this content type. Only the name of the given
     * tml module is taken. The media key will not be fixed to the modules' one.
     * 
     * @param mod
     *            The module, whose name will be taken as inner layout.
     * @throws WGAPIException 
     */
    public boolean setInnerLayout(WGTMLModule mod) throws WGAPIException {

        if (mod == null) {
            return false;
        }

        String modName = mod.getName();
        return setInnerLayoutName(modName);

    }

    /**
     * Sets the inner layout for this content type by specifying the name without reference to the document
     * @param modName Name of the WebTML module to use for inner layout
     */
    public boolean setInnerLayoutName(String modName) throws WGAPIException {
        return setMetaData(META_INNER_LAYOUT, modName);
    }

    /**
     * Returns a list of users allowed to edit content based on this content
     * type.
     * 
     * @return List
     * @throws WGAPIException 
     */
    public List getEditors() throws WGAPIException {
        return (List) this.getMetaData(WGContentType.META_EDITORS);  
    }
    
    /**
     * Old version of {@link #getEditors()}. Avoid in new developments.
     * @throws WGAPIException
     * @deprecated
     */
    public List getContentCreators() throws WGAPIException {
        return getEditors();
    }

    /**
     * Sets the list of allowed content creators for this content type.
     * 
     * @param cc
     * @throws WGAPIException 
     */
    public void setEditors(List cc) throws WGAPIException {
        setMetaData(META_EDITORS, cc);
    }
    
    /**
     * Old version of {@link #setEditors(List)}. Avoid in new developments.
     * @param cc
     * @throws WGAPIException
     * @deprecated
     */
    public boolean setContentCreators(List cc) throws WGAPIException {
        setEditors(cc);
        return true;
    }

    /**
     * Tests, if the user is allowed to create struct entries in this content
     * type for the given parent, according to this content type.
     * Note that this only checks the restrictions of this content type regarding its possible parent, not its restrictions about possible children.
     * 
     * @param parent
     *            A struct entry or an area, that should be the parent for the
     *            new entry
     * @throws WGAPIException 
     */
    public boolean mayCreateChildEntry(WGDocument parent) throws WGAPIException {

        if (parent == null) {
            return false;
        }

        if (!(parent instanceof WGStructEntry || parent instanceof WGArea)) {
            return false;
        }
        
        if (getDatabase().getSessionContext().getAccessLevel() == WGDatabase.ACCESSLEVEL_MANAGER) {
            return true;
        }

        // Positioning check: Disable if the content type is to be root document in a system area (#00001770)
        if (!(parent instanceof WGArea && ((WGArea) parent).isSystemArea())) {

            String positioning = getPositioning();
            if (positioning == null) {
                positioning = POSITIONING_EVERYWHERE;
            }
        
            if (positioning.equals(POSITIONING_CHILDENTRIES)) {
                if (!(parent instanceof WGStructEntry)) {
                    return false;
                }
            }
            else if (positioning.equals(POSITIONING_ROOTENTRIES)) {
                if (!(parent instanceof WGArea)) {
                    return false;
                }
            }
            else if (positioning.equals(POSITIONING_FIXEDPARENTS)) {
                if (parent instanceof WGArea) {
                    return false;
                }
                WGStructEntry entry = (WGStructEntry) parent;
                if (!getAllowedPositions().contains(entry.getStructKey().toString().toLowerCase())) {
                    return false;
                }
            }
            else if (positioning.equals(POSITIONING_FIXEDPARENTTYPES)) {
                if (parent instanceof WGArea) {
                    return false;
                }
                WGStructEntry entry = (WGStructEntry) parent;
                if (entry.getContentType() == null) {
                    return false;
                }
                
                List allowedPositions = getAllowedPositions();
                WGUtils.lowerCaseList(allowedPositions);
                
                if (!allowedPositions.contains(entry.getContentType().getName().toLowerCase())) {
                    return false;
                }
            }
        
        }


        return mayCreateContent();

    }

    /**
     * Tests if the current user may create contents for this content type.
     * 
     * @return boolean
     * @throws WGAPIException 
     */
    public boolean mayCreateContent() throws WGAPIException {

        if (getDatabase().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            return true;
        }

        // Test content creator users
        List users = (List) this.getEditors();
        if (!WGDatabase.anyoneAllowed(users)) {
            if (!this.db.isMemberOfUserList(users)) {
                return false;
            }
        }

        return true;

    }

    /**
     * Returns in which positions content of this type is allowed.
     * @throws WGAPIException 
     */
    public String getPositioning() throws WGAPIException {
       return (String) getMetaData(META_POSITIONING);        
    }

    /**
     * Sets the allowed positioning of content of this content type. Must be a
     * constant WGContentType.POSITIONING_...
     * 
     * @param pos
     * @throws WGAPIException 
     */
    public boolean setPositioning(String pos) throws WGAPIException {
        return setMetaData(META_POSITIONING, pos);
    }

    /**
     * Returns the list of allowed positions for content of this type. If
     * positioning is set to POSITIONING_FIXEDPARENTS, these are the struct keys
     * of the allowed parents. If positioning is set to
     * POSITIONING_FIXEDPARENTTYPES, these are the content types, that the
     * parents must be of.
     * @throws WGAPIException 
     */
    public List getAllowedPositions() throws WGAPIException {
        return (List) getMetaData(META_ALLOWED_POSITIONS);
    }

    /**
     * Sets the allowed positions for this content type. Only valid when
     * positioning is of Type WGContentType.POSITIONING_FIXED...
     * 
     * @param pos
     * @throws WGAPIException 
     */
    public boolean setAllowedPositions(List pos) throws WGAPIException {

        List positionKeys = new ArrayList();
        Iterator posIterator = pos.iterator();
        Object doc;
        while (posIterator.hasNext()) {
            doc = posIterator.next();
            if (doc == null) {
                continue;
            }

            if (doc instanceof WGStructEntry) {
                positionKeys.add(((WGStructEntry) doc).getStructKey());
            }
            else if (doc instanceof WGContentType) {
                positionKeys.add(((WGContentType) doc).getName());
            }
            else if (doc instanceof String) {
                positionKeys.add(doc);
            }

        }

        return setMetaData(META_ALLOWED_POSITIONS, positionKeys);

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDesignDocument#setName(String)
     */
    protected boolean setName(String name) throws WGAPIException {
        return setMetaData(META_NAME, name);
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
     */
    public WGDocument createClone(WGDatabase db) throws WGAPIException {

        WGContentType newContentType = db.createContentType(getName());
        try {
            pushData(newContentType);
    
            newContentType.saveWithGivenTimestamps(getCreated(), getLastModified());
            return newContentType;
        }
        catch (WGAPIException e) {
            throw e;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGContentType newContentType = (WGContentType) doc;
        newContentType.setDescription(getDescription());
        newContentType.setAllowedPositions(new ArrayList(getAllowedPositions()));
        newContentType.setEditors(new ArrayList(getEditors()));
        newContentType.setMetaData(META_INNER_LAYOUT, getMetaData(META_INNER_LAYOUT));
        newContentType.setMetaData(META_OUTER_LAYOUT, getMetaData(META_OUTER_LAYOUT));
        newContentType.setPositioning(getPositioning());
        newContentType.setWorkflow(getWorkflow());
        newContentType.setEventContentCreated(getEventContentCreated());
        newContentType.setEventContentSaved(getEventContentSaved());
        newContentType.setEventWorkflowMail(getEventWorkflowMail());
        newContentType.setNameAliases(new ArrayList(getNameAliases()));
        newContentType.setDescriptionAliases(new ArrayList(getDescriptionAliases()));
        
        /*if (getPreferredParent() != null) {
        	newContentType.setPreferredParent(newContentType.getDatabase().getStructEntryByKey(getPreferredParent().getStructKey()));
        }*/
        
        
        // Must be done directly via meta, bc. the target document may not be available
        if(getMetaData(META_PREFERREDPARENT) != null) {
            newContentType.setMetaData(META_PREFERREDPARENT, getMetaData(META_PREFERREDPARENT));
        }
        
        super.pushData(doc);
    }

    /**
     * Retrieves the name of the workflow for this content type.
     * @throws WGAPIException 
     */
    public String getWorkflow() throws WGAPIException {
        return (String) this.getMetaData(META_WORKFLOW);
    }

    /**
     * Sets the workflow name of this content type.
     * 
     * @param name
     * @throws WGAPIException 
     */
    public boolean setWorkflow(String name) throws WGAPIException {
        return this.setMetaData(META_WORKFLOW, name);
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocument#remove()
     */
    protected boolean remove(WGDocument deletionRoot) throws WGAPIException {
		return innerRemove(deletionRoot, true);
    }



    @Override
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException, WGIllegalStateException {
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_CONTENTTYPE)) {
            throw new WGIllegalStateException("Removing content types via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
        
        // check if contenttype is in use
        if (deepCheck) {
            if (getDatabase().getCore().isContentTypeUsed(this)) {
                throw new WGIllegalStateException("The content type '" + getName() + "' cannot be removed because there are existing struct entries of this type.", WGIllegalStateException.ERRORCODE_SCHEMA_DOCUMENT_IN_USE);
            }
        }
    }

    


    
    /**
     * Returns the name aliases for this content type in other languages. For
     * format see "setNameAliases"
     * @throws WGAPIException 
     */
    public List getNameAliases() throws WGAPIException {
        return (List) getMetaData(META_NAMEALIASES);
    }

    /**
     * Sets the name aliases in different languages. Aliases are specified as
     * list. Every list entry represents a different language. Format is:
     * languagecode:Name-in-Language-Version 
     * 
     * Example: 
     * en:My Content type 
     * de:Mein Seitentyp 
     * fr:French for "My Content Type" (Sorry, my french does not go
     * far beyond "Bonjour!")
     * 
     * @param aliases
     * @throws WGAPIException 
     */
    public boolean setNameAliases(List aliases) throws WGAPIException {
        return setMetaData(META_NAMEALIASES, aliases);
    }

    /**
     * Returns aliases for descriptions in different languages. For format see
     * "setNameAliases"
     * @throws WGAPIException 
     */
    public List getDescriptionAliases() throws WGAPIException {
        return (List) getMetaData(META_DESCRIPTIONALIASES);
    }

    /**
     * Sets the description aliases in different languages. For format see
     * "setNameAliases"
     * 
     * @param aliases
     * @throws WGAPIException 
     */
    public boolean setDescriptionAliases(List aliases) throws WGAPIException {
        return setMetaData(META_DESCRIPTIONALIASES, aliases);
    }

    /**
     * Retrieves the event script for event "contentCreated"
     * 
     * @return WGEventScript-Object for this event
     * @throws WGAPIException 
     */
    public WGEventScript getEventContentCreated() throws WGAPIException {
        String eventString = (String) getMetaData(META_EVENT_CREATECONTENT);
        if (eventString != null) {
            return new WGEventScript(eventString);
        }
        else {
            return null;
        }
    }
    
    /**
     * Retrieves the event script for event "statusChange"
     * 
     * @return WGEventScript-Object for this event
     * @throws WGAPIException 
     */
    public WGEventScript getEventStatusChange() throws WGAPIException {
        String eventString = (String) getMetaData(META_EVENT_STATUSCHANGE);
        if (eventString != null) {
            return new WGEventScript(eventString);
        }
        else {
            return null;
        }
    }
    
    /**
     * Retrieves the event script for event "contentSaved"
     * 
     * @return WGEventScript-Object for this event
     * @throws WGAPIException 
     */
    public WGEventScript getEventContentSaved() throws WGAPIException {
        String eventString = (String) getMetaData(META_EVENT_SAVECONTENT);
        if (eventString != null) {
            return new WGEventScript(eventString);
        }
        else {
            return null;
        }
    }

    /**
     * Retrieves the event script for event "workflowMail"
     * 
     * @return WGEventScript-Object for this event
     * @throws WGAPIException 
     */
    public WGEventScript getEventWorkflowMail() throws WGAPIException {
        String eventString = (String) getMetaData(META_EVENT_WORKFLOWMAIL);
        if (eventString != null) {
            return new WGEventScript(eventString);
        }
        else {
            return null;
        }
    }

    /**
     * Sets the event script for event "contentCreated"
     * 
     * @param script
     *            The script to execute for this event
     * @return true if storage succeeded, false otherwise
     * @throws WGAPIException 
     */
    public boolean setEventContentCreated(WGEventScript script) throws WGAPIException {

        if (script != null) {
            return setMetaData(META_EVENT_CREATECONTENT, script.toString());
        }
        else {
            return setMetaData(META_EVENT_CREATECONTENT, null);
        }
    }

    /**
     * Sets the event script for event "contentSaved"
     * 
     * @param script
     *            The script to execute for this event
     * @return true if storage succeeded, false otherwise
     * @throws WGAPIException 
     */
    public boolean setEventContentSaved(WGEventScript script) throws WGAPIException {

        if (script != null) {
            return setMetaData(META_EVENT_SAVECONTENT, script.toString());
        }
        else {
            return setMetaData(META_EVENT_SAVECONTENT, null);
        }
    }
    
    /**
     * Sets the event script for event "workflowMail"
     * 
     * @param script
     *            The script to execute for this event
     * @return true if storage succeeded, false otherwise
     * @throws WGAPIException 
     */
    public boolean setEventWorkflowMail(WGEventScript script) throws WGAPIException {

        if (script != null) {
            return setMetaData(META_EVENT_WORKFLOWMAIL, script.toString());
        }
        else {
            return setMetaData(META_EVENT_WORKFLOWMAIL, null);
        }
    }
    
    /**
     * Sets the event script for event "statusChange"
     * 
     * @param script
     *            The script to execute for this event
     * @return true if storage succeeded, false otherwise
     * @throws WGAPIException 
     */
    public boolean setEventStatusChange(WGEventScript script) throws WGAPIException {

        if (script != null) {
            return setMetaData(META_EVENT_STATUSCHANGE, script.toString());
        }
        else {
            return setMetaData(META_EVENT_STATUSCHANGE, null);
        }
    }

    /**
     * Returns the preferred parent for documents of this content type. This
     * information is used by WGA Content Clients, which cannot determine the
     * position for the new content (e.g. WGA Word Client). When these clients
     * use this content type, the new content is placed under the document
     * specified here.
     * @throws WGAPIException 
     */
    public WGStructEntry getPreferredParent() throws WGAPIException {
        String structKey = (String) getMetaData(META_PREFERREDPARENT);
        if (structKey != null) {
            return db.getStructEntryByKey(structKey);
        }
        else {
            return null;
        }
    }

    /**
     * Sets the preferred parent for documents of this content type. This
     * information is used by WGA Content Clients, which cannot determine the
     * position for the new content (e.g. WGA Word Client). When these clients
     * use this content type, the new content is placed under the document
     * specified here.
     * 
     * @param parent
     *            The parent struct entry to prefer
     * @throws WGAPIException 
     */
    public boolean setPreferredParent(WGStructEntry parent) throws WGAPIException {
        if (parent != null) {
            return setMetaData(META_PREFERREDPARENT, parent.getStructKey());
        }
        else {
            return setMetaData(META_PREFERREDPARENT, null);
        }
    }

    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException {
        super.performSaveCheck();
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_CONTENTTYPE)) {
            throw new WGIllegalStateException("Updating content types via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
    }

    public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
        return null;
    }
    
    @Override
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return new SkippingIteratorWrapper(Collections.EMPTY_LIST.iterator());
    }

    public Class getChildNodeType() {
        return null;
    }

    public String getNodeKey() throws WGAPIException {
        return getDocumentKey();
    }

    public String getNodeTitle(String language) throws WGAPIException {
        return getName();
    }

    public PageHierarchyNode getParentNode() throws WGAPIException {
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_CONTENTTYPE);
    }

    @Override
    public int getType() {
        return WGDocument.TYPE_CONTENTTYPE;
    }
    
    /**
     * Returns if this content type is available for authoring purposes
     * @throws WGAPIException
     */
    public boolean isAuthoring() throws WGAPIException {
        return (Boolean) getMetaData(META_AUTHORING);
    }
    
    /**
     * Sets if this content type should be available for authoring purposes. Set false to prohibit its use in authoring clients like Content Manager.
     * @param authoring
     * @throws WGAPIException
     */
    public void setAuthoring(boolean authoring) throws WGAPIException {
        setMetaData(META_AUTHORING, authoring);
    }
    
    /**
     * Returns the restrictions regarding childpages on pages of this content type. Returns constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public String getChildPageRestrictions() throws WGAPIException {
        return (String) getMetaData(META_CHILDPAGERESTRICTIONS);
    }
    
    /**
     * Sets the restrictions regarding childpages on pages of this content type. 
     * @param childPages Restriction string. Use constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public void setChildPageRestrictions(String childPages) throws WGAPIException {
        setMetaData(META_CHILDPAGERESTRICTIONS, childPages);
    }
    
    /**
     * Returns the allowed content types for child pages on pages of this type. Only effective when {@link #getChildPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @throws WGAPIException
     */
    public List<String> getAllowedChildTypes() throws WGAPIException {
        return (List<String>) getMetaData(META_ALLOWED_CHILDTYPES);
    }
    
    /**
     * Sets the allowed content types for child pages on pages of this type.  Only effective when {@link #getChildPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @param types Names of content types
     * @throws WGAPIException
     */
    public void setAllowedChildTypes(List<String> types) throws WGAPIException {
        setMetaData(META_ALLOWED_CHILDTYPES, types);
    }

}


