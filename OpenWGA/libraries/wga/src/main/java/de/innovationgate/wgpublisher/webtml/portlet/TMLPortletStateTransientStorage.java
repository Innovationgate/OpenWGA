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

package de.innovationgate.wgpublisher.webtml.portlet;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import com.google.gson.Gson;
import com.google.gson.TypeAdapter;
import com.google.gson.TypeAdapterFactory;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import de.innovationgate.utils.GsonUtils;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGTransientPortlet;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.form.TMLForm.MultipartFormData;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class TMLPortletStateTransientStorage implements TMLPortletStateStorage {
    
    private Map<String, TMLPortletState> _states = new HashMap<String, TMLPortletState>();
    private Set<String> _statesSentByClient = new HashSet<String>();
    private Set<String> _processIdsSentByClient = new HashSet<String>();
    private Set<String> _disposedStates = new HashSet<String>();
    private List<String> _sentRawStates;
    private boolean _ajaxRequest;
    private HttpSession _session;
    private String _profileName;

    private PortletStateSerialisationService _serialisationService = new GsonPortletStateSerialisationService();
    

    
    public static class TMLPortletStateTypeAdapterFactory implements TypeAdapterFactory {

        @SuppressWarnings("unchecked")
        @Override
        public <T> TypeAdapter<T> create(Gson gson, TypeToken<T> type) {
            
            if (!TMLPortletState.class.isAssignableFrom(type.getRawType())) {
                return null;
            }

            final TypeAdapter<TMLPortletState> originalAdapter = gson.getDelegateAdapter(this, TypeToken.get(TMLPortletState.class));
            
            TypeAdapter<TMLPortletState> result = new TypeAdapter<TMLPortletState>() {

                @Override
                public TMLPortletState read(JsonReader in) throws IOException {
                    
                    in.beginObject();
                    String propName = in.nextName();
                    TMLPortletState state;
                    if (propName.equals("default")) {
                        in.beginArray();
                        DesignResourceReference design = new DesignResourceReference(in.nextString());
                        
                        String parent = GsonUtils.nextStringOrNull(in);
                        String key = in.nextString();
                        String name = GsonUtils.nextStringOrNull(in);
                        String process = in.nextString();
                        String profile = in.nextString();
                        String session = in.nextString();
                        in.endArray();
                        state = new TMLPortletState(key, name, parent, profile, process, session, design);
                    }
                    else if (propName.equals("data")) {
                        state = originalAdapter.read(in);
                    }
                    else {
                        throw new IOException("Invalid JSON portlet state data name: " + propName);
                    }
                    
                    in.endObject();
                    return state;

                }

                @Override
                public void write(JsonWriter out, TMLPortletState state) throws IOException {
                    
                   out.beginObject();
                   if (state.isDefaultState()) {
                       out.name("default");
                       out.beginArray();
                       out.value(state.getControllerDesign().toString());
                       out.value(state.getParentPortletKey());
                       out.value(state.getPortletKey());
                       out.value(state.getPortletName());
                       out.value(state.getProcessId());
                       out.value(state.getProfileName());
                       out.value(state.getSessionId());
                       out.endArray();
                       
                   }
                   else {
                       out.name("data");
                       originalAdapter.write(out, state);
                   }
                   out.endObject();
                    
                    
                }
                
            }.nullSafe();
            
            return (TypeAdapter<T>) result;
            
        }
        
    }
    
    public TMLPortletStateTransientStorage(WGACore core, WGUserProfile wgUserProfile, HttpServletRequest req, MultipartFormData formData, boolean ajaxRequest) throws UnsupportedEncodingException, WGAPIException {
        
        TMLPortletState state;
        _sentRawStates = null;
        _session = req.getSession();
        _profileName = wgUserProfile.getName();
        _ajaxRequest = ajaxRequest;
       
        // On AJAX requests we find sent states as array of request parameters
        String[] sentStateParameters = req.getParameterValues("$portletState");
        if (sentStateParameters != null) {
            _sentRawStates = Arrays.asList(sentStateParameters);
        }
        
        // On Non-AJAX request without form we find it inside one form field, given as request paramter
        if (_sentRawStates == null) {
            String states = req.getParameter("$portletStates");
            if (states != null) {
                _sentRawStates = WGUtils.deserializeCollection(states, "\n", true);
            }
        }
        
        // On Non-AJAX form post requests the portlet state is contained in a multipart formdata field
        if (_sentRawStates == null && formData != null && formData.getPortletStatesItem() != null) {
            _sentRawStates = WGUtils.deserializeCollection(formData.getPortletStatesItem().getString(core.getCharacterEncoding()), "\n", true);
        }
        
    }

    public void readStates(WGACore core) throws WGException {
        TMLPortletState state;
        if (_sentRawStates != null) {
            try {
                for (String stateStr : _sentRawStates) {
                    if (!WGUtils.isEmpty(stateStr)) {
                        state = deserializePortletState(stateStr);
                        if (state == null) { // Decoding error. Extract the portlet key so we at least know that this was sent and don't try to retrieve it again

                        	if(stateStr.indexOf("//")<0) {	// avoid StringIndexOutOfBoundsException
                        		//throw new WGException("Invalid state '" + stateStr + "' not containing //");
                        		core.getLog().warn("Ignoring invalid portlet state '" + stateStr + "' not containing //");
                        		continue;
                        	}
                        	
                            String portletKey = stateStr.substring(0, stateStr.indexOf("//"));
                            _statesSentByClient.add(portletKey);
                            continue;
                        }

                        _statesSentByClient.add(state.getPortletKey());
                        _processIdsSentByClient.add(state.getProcessId());
                        if (state.getProfileName().equals(_profileName) && state.getSessionId().equals(_session.getId())) {
                            state.setSentFromClient(true);
                            state.setTransmitToClient(!_ajaxRequest); // On non-ajax requests we send all sent states back to the client, because he might not have sessionStorage
                            _states.put(state.getPortletKey(), state);
                        }
                        
                    }
                }
            }
            catch (Exception e) {
                WGACore.INSTANCE.getLog().error("Exception reading portlet states", e);
                _states.clear();
                _statesSentByClient.clear();
                _processIdsSentByClient.clear();
            }
            
            _sentRawStates = null;
        }
        
        
    }

    @Override
    public TMLPortletState getState(TMLPortlet portlet) throws WGAPIException {
        
        TMLPortletState state = _states.get(portlet.getportletkey());
        if (state == null) {
            state = portlet.createState(this);
            state.setTransient(true);
            if (_statesSentByClient.contains(portlet.getportletkey())) {
                state.setSentFromClient(true);
            }
            if (!portlet.isroot()) {
                _states.put(portlet.getportletkey(), state);
            }
        }
        return state;
        
    }

    @Override
    public void disposeState(String appDb, String portletKey) {
        TMLPortletState state = _states.remove(portletKey);
        if (state != null && state.getProcessId() != null) {
            ProcessContextRegistration contexts = TMLContext.fetchProcessContextRegistration(_session);
            if (contexts != null) {
                contexts.removeProcessContext(state.getProcessId());
            }
        }
        _disposedStates.add(portletKey);
    }
    
    public Collection<TMLPortletState> getAllStates() {
        return Collections.unmodifiableCollection(_states.values());
    }
    
    public boolean isTransient() {
        return true;
    }

    public Set<String> getDisposedStates() {
        return _disposedStates;
    }

    @Override
    public void disposeChildState(String appDb, String portletKey, String childName) throws WGAPIException {
        String childPortletKey = WGTransientPortlet.generateKey(appDb, portletKey, childName);
        disposeState(appDb, childPortletKey);
    }

    public Set<String> getProcessIdsSentByClient() {
        return _processIdsSentByClient;
    }

    public TMLPortletState deserializePortletState(String str) throws IOException {
        try {
            // Cutoff portlet key
            str = str.substring(str.indexOf("//") + 2);
            
            // Try to find implementation id. If not found or different from current service we ignore the state.
            int secondDividerPos = str.indexOf("##");
            if (secondDividerPos == -1) {
                return null;
            }
            
            String serviceId = str.substring(0, secondDividerPos);
            String base64 = str.substring(secondDividerPos+2);
            if (!serviceId.equals(_serialisationService.getId())) {
                return null;
            }
            
            // Deserialize
            TMLPortletState state = _serialisationService.deserialize(base64);
            if (state != null) {
                state.afterTransientDeserialisation(WGA.get(), this);
            }
            return state;
        }
        catch (Exception e) {
            throw new IOException("Exception deserializing portlet state", e);
        }
        
    }
    
    public String serializePortletState(TMLPortletState state) throws IOException {
        try {
            return _serialisationService.getId() + "##" + _serialisationService.serialize(state);
        }
        catch (Exception e) {
            throw new IOException("Exception serializing portlet state", e);
        }
    
    }

    @Override
    public boolean isStoreSessionVarsAtState() {
        return false;
    }


}
