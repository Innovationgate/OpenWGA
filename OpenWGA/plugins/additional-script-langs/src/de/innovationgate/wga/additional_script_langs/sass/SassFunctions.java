package de.innovationgate.wga.additional_script_langs.sass;

import org.jruby.Ruby;
import org.jruby.RubyHash;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.tml.Context;

public class SassFunctions {
    
    public static String wga_css_url(String name, RubyHash options) throws WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        Design design = (Design) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaDesign"));
        
        return design.resolve(name).scriptURL(WGScriptModule.CODETYPE_CSS);

    }
    
    public static String wga_file_url(String db, String container, String name, RubyHash options) throws WGException {
        
        Ruby runtime = org.jruby.Ruby.getGlobalRuntime();
        Context context = (Context) options.get(org.jruby.RubySymbol.newSymbol(runtime, "wgaContext"));
        
        return context.fileurl(db, container, name);

    }

}
