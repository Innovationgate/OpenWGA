package de.innovationgate.wga.additional_script_langs.sass;

import java.util.List;
import java.util.Map;

import javax.json.JsonString;

import ro.isdc.wro.model.group.processor.Injector;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.additional_script_langs.CssDialectsPostProcessor;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;

public class SassPostProcessor extends CssDialectsPostProcessor {
    
    @Override
    protected String compileDialect(WGA wga, PostProcessData data, String code, Injector injector, PostProcessResult result) throws WGException, WGAPIException {
        StringBuilder completeCode = new StringBuilder();
        WGDesignDocument doc = data.getDocument();
        
        
        if (data.getCacheQualifier() != null) {
            @SuppressWarnings("unchecked")
            Map<Object,Object> variables = (Map<Object,Object>) data.getCacheQualifier();
            for (Map.Entry<Object,Object> varEntry : variables.entrySet()) {
                completeCode.append("$").append(String.valueOf(varEntry.getKey())).append(":");
                completeCode.append(formatValue(varEntry.getValue())).append(";");
                completeCode.append("\n");
            }
        }
        
        completeCode.append(code);
        
        Design design = wga.design(doc.getDatabase().getDbReference()).resolve(doc.getName());
        Context cx = wga.createTMLContext(data.getDocument().getDatabase(), design);
        SassEngine engine = new SassEngine(wga, design, cx, doc.getDesignReference().toString(), result);
        String out = engine.process(completeCode.toString());
        return out;
    }

    private String formatValue(Object value) {

        if (value instanceof List<?>) {
            return WGUtils.serializeCollection((List) value, " ", new ObjectFormatter() {
                @Override
                public String format(Object obj) throws FormattingException {
                    return formatValue(obj);
                }
            }, false);            
        }
        else if (value instanceof Map<?,?>) {
            return "(" + WGUtils.serializeCollection(((Map<?,?>) value).entrySet(), ", ", new ObjectFormatter() {
                @Override
                public String format(Object obj) throws FormattingException {
                    Map.Entry<?,?> entry = (Map.Entry<?,?>) obj; 
                    return formatValue(entry.getKey()) + ": " + entry.getValue();
                }
            }, false) + ")";
        }
        else if (value instanceof JsonString) {
        	return ((JsonString) value).getString();
        }
        
        return String.valueOf(value);
    }
    
    @Override
    protected boolean isMinimizingActive() {
        return false;
    }

}
