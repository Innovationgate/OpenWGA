package de.innovationgate.wga.additional_script_langs.sass;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.ResourceRef;

public class SassEngine {
    private static final Logger LOG = LoggerFactory.getLogger(SassEngine.class);
    private static final String RUBY_GEM_REQUIRE = "rubygems";
    private static final String SASS_PLUGIN_REQUIRE = "sass/plugin";
    private static final String SASS_ENGINE_REQUIRE = "sass/engine";

    private static final ScriptEngine rubyEngine = new ScriptEngineManager(SassEngine.class.getClassLoader()).getEngineByName("jruby");
    
    private final Set<String> requires;
    private WGA _wga;
    private Design _design;
    private Context _context;
    private PostProcessResult _ppr;
    private ResourceRef _ref;

    public SassEngine(WGA wga, Design design, Context cx, String uri, PostProcessResult result) throws WGException {
      System.setProperty("org.jruby.embed.compat.version", "JRuby1.9");
      requires = new LinkedHashSet<String>();
      requires.add(RUBY_GEM_REQUIRE);
      requires.add(SASS_PLUGIN_REQUIRE);
      requires.add(SASS_ENGINE_REQUIRE);
      _wga = wga;
      _design = design;    
      _context = cx;
      _ppr = result;
      _ref = new ResourceRef(_design, ResourceRef.TYPE_CSS);
    }
    
    /**
     * Adds a ruby require to the ruby script to be run by this RubySassEngine. It's safe to add the same require twice.
     *
     * @param require
     *          The name of the require, e.g. bourbon
     */
    public void addRequire(final String require) {
      if (require != null && require.trim().length() > 0) {
        requires.add(require.trim());
      }
    }

    /**
     * Transforms a sass content into css using Sass ruby engine. This method is synchronized because the engine itself is
     * not thread-safe.
     *
     * @param content
     *          the Sass content to process.
     * @throws Exception 
     */
    public String process(final String content) throws Exception {

    	if (StringUtils.isEmpty(content)) {
    		return StringUtils.EMPTY;
    	}
      
	    StringWriter sw = new StringWriter();
	    try {
	    	synchronized (rubyEngine) {
				
			      Bindings bindings = rubyEngine.createBindings();
			      bindings.put("wga", _wga);
			      bindings.put("wgaDesign", _design);
			      bindings.put("wgaContext", _context);
			      bindings.put("wgaResult", _ppr);
			      bindings.put("wgaResourceRef", _ref);
			      
			      rubyEngine.getContext().setErrorWriter(new PrintWriter(sw));
			      
			      return rubyEngine.eval(buildUpdateScript(content), bindings).toString();
	    	}
	    }
	    catch(ScriptException e){
	    	  StringReader reader = new StringReader(sw.toString());
	    	  List<String> errors = IOUtils.readLines(reader);
	    	  if(errors.size()>0)
	    		  _wga.getLog().error("SASS ERROR: " + errors.get(0));
		      throw new Exception("SASS Exception");
	    }
	    catch (Exception e) {
		      _wga.getLog().error("general SASS error", e);
		      throw e;
	    }
      
    }    

    private String buildUpdateScript(final String content) throws UnsupportedEncodingException, IOException {
      Validate.notNull(content);
      final StringWriter raw = new StringWriter();
      final PrintWriter script = new PrintWriter(raw);
      final StringBuilder sb = new StringBuilder();
      final StringBuilder cb = new StringBuilder();
      
      String style = WGACore.isDevelopmentModeEnabled() ? ":expanded" : ":compressed";
      String lineNumbers = WGACore.isDevelopmentModeEnabled() ? "true" : "false";
      
      Map<String,String> options = new HashMap<String,String>();
      options.put(":cache", "false");
      options.put(":syntax", ":scss");
      options.put(":trace_selectors", lineNumbers);
      options.put(":style", style);
      options.put(":filesystem_importer", "Java::DeInnovationgateWgaAdditional_script_langsSass::Importer");
      options.put(":filename", "\"" + _ref.toString() + "\"");
      options.put(":wga", "$wga");
      options.put(":wgaDesign", "$wgaDesign");
      options.put(":wgaContext", "$wgaContext");
      options.put(":wgaResult", "$wgaResult");
      options.put(":wgaResourceRef", "$wgaResourceRef");
      
      boolean firstOption = true;
      for (Map.Entry<String,String> option : options.entrySet()) {
          if (firstOption) {
              firstOption = false;
          }
          else {
              sb.append(", ");
          }
          sb.append(option.getKey()).append(" => ").append(option.getValue());
      }
      

      for (final String require : requires) {
        script.println("  require '" + require + "'                                   ");
      }

      script.print(WGUtils.readString(getClass().getClassLoader().getResourceAsStream(WGUtils.getPackagePath(SassEngine.class) + "/functions.rb"), "UTF-8"));      
      
      final int BACKSLASH = 0x5c;
      for (int i = 0; i < content.length(); i++) {
        final int code = content.codePointAt(i);
        if (code < 0x80) {
          // We leave only ASCII unchanged.
          if (code == BACKSLASH) {
            // escape backslash
            cb.append("\\");
          }
          cb.append(content.charAt(i));
        } else {
          // Non-ASCII String may cause invalid multibyte char (US-ASCII) error with Ruby 1.9
          // because Ruby 1.9 expects you to use ASCII characters in your source code.
          // Instead we use Unicode code point representation which is usable with
          // Ruby 1.9 and later. Inspired from
          // http://www.stefanwille.com/2010/08/ruby-on-rails-fix-for-invalid-multibyte-char-us-ascii/
          cb.append(String.format("\\u%04x", code));
        }
      }
      final String scriptAsString = String.format("result = Sass::Engine.new(\"%s\", {%s}).render",
          cb.toString().replace("\"", "\\\"").replace("#", "\\#"), // escape ", #
          sb.toString());
      LOG.debug("scriptAsString: {}", scriptAsString);
      script.println(scriptAsString);
      script.flush();
      return raw.toString();
    }
  }