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

package de.innovationgate.wgpublisher.hdb;

import org.apache.commons.lang.StringUtils;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEvent;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseListenerV2;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * HDB listener for HDBModel documents
 */
public class HDBModelListener implements WGHierarchicalDatabaseListenerV2 {
	
	public static final String TMLSCRIPT_LISTENER_FOLDER = "hdbmodel";
    
    public String getName() {
        return "HDB Model Listener";
    }
    
    public void postMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable {

        if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            if (isDirectParentEvent(event)) {
        	    executeLocalEvent(event, HDBModel.EVENT_POST_MOVE_FROM);
         	}
        	
        	if (HDBModel.isContent(event.getListenerContent())) {
        	    executeParentEvent(event, HDBModel.EVENT_POST_MOVE_FROM);
        	}
        }
    	    	
    }

    public void postMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable {
    	
        if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            if (isDirectParentEvent(event)) {
        	    
        	    // Updating parent relations on this occasion
        	    HDBModel.updateParentRelations(event.getContent());
        	    event.getContent().save(); // Unfortunately we must re-save here...
        	    
        	    executeLocalEvent(event, HDBModel.EVENT_POST_MOVE_TO);
             	
         	}
        	
        	if (HDBModel.isContent(event.getListenerContent())) {
        	    executeParentEvent(event, HDBModel.EVENT_POST_MOVE_TO);
        	}
        }
        
    }

    public void preMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable {
    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_PRE_MOVE_FROM);
            }
        	
            if (isDirectParentEvent(event)) {
        	    executeLocalEvent(event, HDBModel.EVENT_PRE_MOVE_FROM);
         	}
        }
    }

    public void preMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable {
    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
    	
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_PRE_MOVE_TO);
            }
        	
            if (isDirectParentEvent(event)) {
        	    executeLocalEvent(event, HDBModel.EVENT_PRE_MOVE_TO);
         	}
        }
    }

    public void postCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable {

    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
    	HDBModelParams params = (HDBModelParams) event.getParameter();
    	HDBModel model = HDBModel.getModel(event.getDb().getWrappedDB());
    	if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
    	
            // Operations for the content itself (executed on parent level)
            if (isDirectParentEvent(event)) {

                // Sync content and struct titles
                syncPageTitles(event.getContent());
                
                // Init the submodel of a new content
                if (HDBModel.TYPE_CONTENT.equals(event.getContent().getItemText(HDBModel.ITEM_TYPE))) {
                    model.initSubmodel(event.getContent());
                }
                
                // If we can retrieve a web context: Throw portlet events
                TMLContext context = TMLContext.getThreadMainContext();
                if (context != null && context.iswebenvironment()) {
                    TMLPortlet portlet = context.getportlet();
                    if (portlet != null) {
                        PortletEvent portletEvent = context.createevent("hdbupdate");
                        portletEvent.addParameter("key", event.getContent().getContentKey().toString());
                        portletEvent.addParameter("type", "create");
                        portletEvent.addParameter("contentclass", event.getContent().getContentClass());
                        portlet.fireevent(portletEvent);
                        
                        portletEvent = context.createevent("hdbupdate-" + event.getContent().getContentClass());
                        portletEvent.addParameter("key", event.getContent().getContentKey().toString());
                        portletEvent.addParameter("type", "create");
                        portlet.fireevent(portletEvent);
                    }
                }
                                
                executeLocalEvent(event, HDBModel.EVENT_POST_CREATE);
            }
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_POST_CREATE);
            }
        
    	}
        
    }

    private void syncPageTitles(WGContent content) {

        try {
            WGStructEntry struct = content.getStructEntry();
            if (!WGUtils.nullSafeEquals(content.getTitle(), struct.getTitle()) && struct.maySave()) {
                struct.setTitle(content.getTitle());
                struct.save();
            }
        }
        catch (WGAPIException e) {
            HDBModel.LOG.error("Exception syncing page titles", e);
        }
        
        
    }

    private void executeParentEvent(WGHierarchicalDatabaseEvent event, String hdbEventType) throws WGException {
        HDBModelContentClassEvents customListener = retrieveContentClassEvents(event.getDb().getWrappedDB(), event.getListenerContent().getContentClass());
        HDBModelParams params = (HDBModelParams) event.getParameter();
        if (customListener != null) {
            HDBModelHDBEvent hdbModelEvent = new HDBModelHDBEvent(event, hdbEventType + createContentClassJsIdentifier(params.getContentClass()), event.getListenerContent(), event.getContent(), params.getRefDocumentKey(), false);
            try {
                customListener.callModuleEventFunction(hdbModelEvent);
            }
            catch (WGCancelledException e) {
                throw new WGHierarchicalDatabaseEventCanceledException(event, e.getMessage());
            }
        }
    }
    
    public static String createContentClassJsIdentifier(String contentClass) {
        
        
        // Remove chars not allowed in JS var names:
        StringBuffer extension = new StringBuffer();
        for (int idx=0; idx < contentClass.length(); idx++) {
            Character c = contentClass.charAt(idx);
            if (idx == 0) {
                if (Character.isLetter(c)) {
                    extension.append(c);
                }
            }
            else {
                if (Character.isLetter(c) || Character.isDigit(c)) {
                    extension.append(c);
                }
            }
        }
        
        // Capitalize
        return StringUtils.capitalize(extension.toString().toLowerCase());

        
        
    }

    public void postDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        
        if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
        
            // Operations for the content itself (executed on parent level)
            if (isDirectParentEvent(event)) {
                executeLocalEvent(event, HDBModel.EVENT_POST_DELETE);
                
             // If we can retrieve a web context: Throw portlet events
                TMLContext context = TMLContext.getThreadMainContext();
                if (context != null && context.iswebenvironment()) {
                    TMLPortlet portlet = context.getportlet();
                    if (portlet != null) {
                        PortletEvent portletEvent = context.createevent("hdbupdate");
                        portletEvent.addParameter("key", String.valueOf(params.getRefDocumentKey()));
                        portletEvent.addParameter("type", "delete");
                        portletEvent.addParameter("contentclass", params.getContentClass());
                        portlet.fireevent(portletEvent);
                        
                        portletEvent = context.createevent("hdbupdate-" + params.getContentClass());
                        portletEvent.addParameter("key", String.valueOf(params.getRefDocumentKey()));
                        portletEvent.addParameter("type", "delete");
                        portlet.fireevent(portletEvent);
                    }
                }
            }
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_POST_DELETE);
            }
        
        }
        
    }

    public void postUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
    	
    	HDBModelParams params = (HDBModelParams) event.getParameter();
    	HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
    	
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
        	// Execute event for triggering content itself
            if (isDirectParentEvent(event)) {
                
                // Sync content and struct titles
                syncPageTitles(event.getContent());
                
                // If we can retrieve a web context: Throw portlet events
                TMLContext context = TMLContext.getThreadMainContext();
                if (context != null && context.iswebenvironment()) {
                    TMLPortlet portlet = context.getportlet();;
                    if (portlet != null) {
                        PortletEvent portletEvent = context.createevent("hdbupdate");
                        portletEvent.addParameter("key", event.getContent().getContentKey().toString());
                        portletEvent.addParameter("type", "update");
                        portletEvent.addParameter("contentclass", event.getContent().getContentClass());
                        portlet.fireevent(portletEvent);
                        
                        portletEvent = context.createevent("hdbupdate-" + event.getContent().getContentClass());
                        portletEvent.addParameter("key", event.getContent().getContentKey().toString());
                        portletEvent.addParameter("type", "update");
                        portlet.fireevent(portletEvent);
                    }
                }
                
                executeLocalEvent(event, HDBModel.EVENT_POST_UPDATE);
                
        	}
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_POST_UPDATE);
            }
        }
    	
    }

    public void preCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
        
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        HDBModelParams params = (HDBModelParams) event.getParameter();
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_PRE_CREATE);
            }
            
            if (isDirectParentEvent(event)) {
                executeLocalEvent(event, HDBModel.EVENT_PRE_CREATE);
        	}
            
            Form form = params.getForm();
            if (form != null) {
                // validate form
                if (!form.validate()) {
                    //form.gettargetcontext().addwarning("Form validation during storeindocument() failed. Formdata not saved.", false);
                    throw new HDBModelFormValidationFailedException(event);
                }        
             }
        }
        
    }

    public void preDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable {
    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
    	
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {
            
            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_PRE_DELETE);
            }
            
            if (isDirectParentEvent(event)) {
        	    executeLocalEvent(event, HDBModel.EVENT_PRE_DELETE);
        	}
        }
    	
    }

    public void preUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        
    	if (!(event.getParameter() instanceof HDBModelParams)) {
            // Exit. This is no hdb model triggered action 
            return;
        }
    	
    	
        HDBModelParams params = (HDBModelParams) event.getParameter();
        HDBModel model = HDBModel.getModel(event.getListenerContent().getDatabase());
        
        if (HDBModel.TYPE_CONTENT.equals(params.getType())) {

            if (HDBModel.isContent(event.getListenerContent())) {
                executeParentEvent(event, HDBModel.EVENT_PRE_UPDATE);
            }
                    
        	// Operations for the parent of the created content only (should only be executed once)
            if (isDirectParentEvent(event)) {   
            	executeLocalEvent(event, HDBModel.EVENT_PRE_UPDATE);
            	

            	TMLForm form = (TMLForm) params.getForm();
                if (form != null) {
                    // validate form
                    if (!form.validate()) {
                        form.gettargetcontext().addwarning("Form validation during storeindocument() failed. Formdata not saved.", false);
                        throw new HDBModelFormValidationFailedException(event);
                    }
                    
                    // From here on all "pre" tests have been done. We now start performing automatic update functionality.
    
                    // Transfer form data
                    form.pushFields(event.getContent());
                    form.pushFiles(event.getContent());
                    form.retainFiles(event.getContent());
                }
                    
                // Execute custom process
                HDBModel.executeCustomProcess(event.getContent(), model.getCore(), params);
                
            }
        }
      
    }

    private void executeLocalEvent(WGHierarchicalDatabaseEvent event, String hdbEventType) throws WGException {
        
        HDBModel model = HDBModel.getModel(event.getDb().getWrappedDB());
        HDBModelParams param = (HDBModelParams) event.getParameter();
        
        HDBModelContentClassEvents customListener = retrieveContentClassEvents(event.getDb().getWrappedDB(), (event.getContent() != null ? event.getContent().getContentClass() : param.getContentClass()));
        if (customListener != null) {
            WGContent eventReceiver = (event.getContent() != null ? event.getContent() : event.getParentContent());
            try {
                customListener.callModuleEventFunction(new HDBModelHDBEvent(event, hdbEventType, eventReceiver, event.getContent(), param.getRefDocumentKey(), true));
            }
            catch (WGCancelledException e) {
                throw new WGHierarchicalDatabaseEventCanceledException(event, e.getMessage());
            }
        }
    }
    
    public static HDBModelContentClassEvents retrieveContentClassEvents(WGDatabase db, String contentClass) throws WGException {
        
        HDBModel model = HDBModel.getModel(db);
    	
    	// V2 object
        String moduleName = model.getScriptsPath() + ":" + contentClass + ".events";
        Design module = WGA.get(model.getCore()).design(db).resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, true);
        if (module != null) {
            return new HDBModelContentClassEvents(model.getCore(), contentClass, module, ObjectType.V2_ISOLATED);
        }
    	
    	// Legacy object
        moduleName = model.getScriptsPath() + ":" + contentClass;
    	module = WGA.get(model.getCore()).design(db).resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, true);
        if (module != null) {
        	return new HDBModelContentClassEvents(model.getCore(), contentClass, module, ObjectType.V1);
        }
        
        return null;
    }
    
    public boolean isDirectParentEvent(WGHierarchicalDatabaseEvent event) {
        
        return event.getListenerContent().equals(event.getParentContent());
        
    }
    


}
