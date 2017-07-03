package de.innovationgate.wga.additional_script_langs.sass;

import org.jruby.Ruby;
import org.jruby.RubyHash;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;

public class SassFunctions {
    
    public static String wga_css_url(String name, RubyHash options) throws WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        Design design = (Design) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"));
        try{
        	return design.resolve(name).scriptURL(WGScriptModule.CODETYPE_CSS);
        }
        catch(Exception e){
        	WGA wga = (WGA) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wga"));
        	wga.getLog().error("wga_css_url: css not found: " + name);
        	return "/* css not found: " + name + " */";        	
        }
    }
    
    public static String wga_file_url(String db, String container, String name, RubyHash options) throws WGException {
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        Design design = (Design) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"));
        try{
        	WGA wga = (WGA) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wga"));
        	return design.resolve(db, container).fileURL(name);
        }
        catch(Exception e){
        	WGA wga = (WGA) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wga"));
        	wga.getLog().error("wga_file_url: file not found: " + db + "/" + container + "/" + name + " ... refered from " + design.toString());
        	return "/* file not found: " + db + "/" + container + "/" + name + " */";        	
        }
    }

}
