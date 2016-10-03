package de.innovationgate.wga.additional_script_langs.sass;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.jruby.Ruby;
import org.jruby.RubyHash;
import org.jruby.javasupport.JavaEmbedUtils;
import org.jruby.runtime.builtin.IRubyObject;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.ResourceRef;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;

public class Importer {
    
    public Importer(String path) throws WGException {
    }
    
    public Object find(String uri,RubyHash options) throws IOException, WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        PostProcessResult result = (PostProcessResult) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResult"));
        ResourceRef base_ref = (ResourceRef) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResourceRef"));
        
        ResourceRef ref = findResourceRef(base_ref, uri);
        
        // Return SASS engine with the given module code        
        if(ref!=null){
        	return importResource(ref, result, runtime, options);
        }
        else return null;
    }
    
    private Object importResource(ResourceRef ref, PostProcessResult result, Ruby runtime, RubyHash options) throws WGException, IOException{
        result.addIntegratedResource(ref.getDesignDocument());
        IRubyObject rubyClass = JavaEmbedUtils.newRuntimeAdapter().eval(runtime, "Sass::Engine");
        @SuppressWarnings("unchecked")
        Map<Object,Object> targetOptions = new HashMap<Object,Object>(options);
        targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"), ref.getDesign());
        targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "wgaResourceRef"), ref);
        targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "filename"), ref.toString());
        targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "original_filename"), ref.toString());
        Object[] parameters = {ref.getCode(), targetOptions};
       	return JavaEmbedUtils.invokeMethod(runtime, rubyClass, "new", parameters, IRubyObject.class);
    }

    private ResourceRef findResourceRef(ResourceRef parent, String uri) throws WGException, IOException{
        ResourceRef ref = new ResourceRef(parent, uri); 
    	if(ref.getType().equals(ResourceRef.TYPE_FILE)){
    		// type=file must have file extension. If not add .scss
    		if(!ref.getResourceName().contains(".")){
    			ref.setResourceName(ref.getResourceName() + ".scss");
    		}
    	}
        
        if(ref.getCode()==null){
            // Try _filename 
        	ref.setResourceName("_" + ref.getResourceName());
            if(ref.getCode()!=null)
            	return ref;
        	
        }
       	return ref;
    }
    
    public Date mtime(String uri,RubyHash options) throws WGException, IOException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        ResourceRef ref = (ResourceRef) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResourceRef"));
        
        if(ref!=null)
        	return ref.getDesignDocument().getLastModified();
        else return null;
    }
    
    public Object find_relative(String uri, String base, RubyHash options) throws WGException, IOException {
    	Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
    	ResourceRef base_ref = (ResourceRef) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResourceRef"));
        PostProcessResult result = (PostProcessResult) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResult"));
        if(base_ref!=null){
        	ResourceRef ref = findResourceRef(base_ref, uri);
        	if(ref.getDesignDocument()!=null)
        		return importResource(ref, result, runtime, options);
        }
        return null;
    }

}
