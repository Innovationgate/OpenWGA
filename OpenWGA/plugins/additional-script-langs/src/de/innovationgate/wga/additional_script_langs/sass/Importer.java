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
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;

public class Importer {
    
    class Module {
        
        private Design _design;
        private WGScriptModule _document;

        public Module(Design targetDesign, WGScriptModule module) {
            _design = targetDesign;
            _document = module;
        }

        public Design getDesign() {
            return _design;
        }

        public WGScriptModule getDocument() {
            return _document;
        }
    }

    @SuppressWarnings("unused")
    private WGA _wga;
    private String _path;
    
    public Importer(String path) throws WGException {
        _path = path;
    }
    
    public Object find(String uri,RubyHash options) throws IOException, WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        WGA wga = (WGA) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wga"));
        Design design = (Design) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"));
        PostProcessResult result = (PostProcessResult) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResult"));
        
        Module importerModule = findModule(design, uri);
        
        // Return SASS engine with the given module code
        if (importerModule != null) {
            result.addIntegratedResource(importerModule.getDocument());
            IRubyObject rubyClass = JavaEmbedUtils.newRuntimeAdapter().eval(runtime, "Sass::Engine");
            @SuppressWarnings("unchecked")
            Map<Object,Object> targetOptions = new HashMap<Object,Object>(options);
            targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"), importerModule.getDesign());
            targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "filename"), importerModule.getDesign().getBaseReference().toString());
            targetOptions.put(org.jruby.RubySymbol.newSymbol(runtime, "original_filename"), importerModule.getDesign().getBaseReference().toString());
            Object[] parameters = {importerModule.getDocument().getCode(), targetOptions};
            return JavaEmbedUtils.invokeMethod(runtime, rubyClass, "new", parameters, IRubyObject.class);
        }
        else {
            return null;
        }
        
    }

    private Module findModule(Design design, String uri) throws WGException {
        
                
        
        // Try full SCSS
        Design targetDesign = design.resolve(_path).resolve(enhanceURI(uri));
        WGScriptModule mod = targetDesign.getScriptModule(WGScriptModule.CODETYPE_CSS);
        
        // Try partial
        if (mod == null) {
            targetDesign = design.resolve(_path).resolve(enhanceURI("_" + uri));
            mod = targetDesign.getScriptModule(WGScriptModule.CODETYPE_CSS);
        }
        
        Module importerModule = null;
        if (mod != null) {
            importerModule = new Module(targetDesign, mod);
        }
        return importerModule;
    }
    
    private String enhanceURI(String uri) {
        if (!uri.startsWith("::") && !uri.contains("@") && !uri.contains("/")) {
            return "::" + uri;
        }
        else {
            return uri;
        }
    }

    public Date mtime(String uri,RubyHash options) throws WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        WGA wga = (WGA) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wga"));
        Design design = (Design) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"));
        PostProcessResult result = (PostProcessResult) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaResult"));

        Module importerModule = findModule(design, uri);
        if (importerModule != null) {
            return importerModule.getDocument().getLastModified();
        }
        else {
            return null;
        }
    }
    
    public Object find_relative(String uri, String base, RubyHash options) throws WGException, IOException {
        return find(uri, options);
        
    }

}
