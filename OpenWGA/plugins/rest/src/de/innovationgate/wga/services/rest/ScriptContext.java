package de.innovationgate.wga.services.rest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.UriBuilder;

import de.innovationgate.wga.services.rest.v1.resources.ApiResource;

public class ScriptContext {
    
    public static class Header {
        public String getName() {
            return _name;
        }
        public String getValue() {
            return _value;
        }
        private String _name;
        private String _value;
        public Header(String name, String value) {
            super();
            _name = name;
            _value = value;
        }
        
        
    }
    
    private UriBuilder _uri;
    private ApiResource _controller;
    private int _responseCode = 200;
    private List<Header> _responseHeaders = new ArrayList<Header>();
    private String _selfUri;
    private Map<String,InputStream> _files;
    
    public List<Header> getResponseHeaders() {
        return _responseHeaders;
    }

    public ScriptContext(UriBuilder uri, ApiResource controller) {
        this(uri, controller, null);
    }
    
    public ScriptContext(UriBuilder uri, ApiResource controller, Map<String,InputStream> files) {
        _uri = uri;
        _controller = controller;
        _files = files != null ? files : new HashMap<String, InputStream>();
    }

    public String getUri() {
        return _uri.build().toString();
    }
    
    public UriBuilder getUriBuilder() {
        return _uri;
    }

    public ApiResource getController() {
        return _controller;
    }

    public int getResponseCode() {
        return _responseCode;
    }

    public void setResponseCode(int responseCode) {
        _responseCode = responseCode;
    }
    
    public void addResponseHeader(String name, String value) {
        _responseHeaders.add(new Header(name, value));
    }

    public String getSelfUri() {
        return _selfUri;
    }

    public void setSelfUri(String selfUri) {
        _selfUri = selfUri;
    }

    public Map<String, InputStream> getFiles() {
        return _files;
    }

}
