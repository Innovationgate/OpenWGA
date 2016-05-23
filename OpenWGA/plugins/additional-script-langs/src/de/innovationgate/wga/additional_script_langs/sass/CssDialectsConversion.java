package de.innovationgate.wga.additional_script_langs.sass;

import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.PreProcessData;
import de.innovationgate.wgpublisher.design.conversion.PreProcessResult;

public class CssDialectsConversion implements DesignResourceConversion {

    
    @Override
    public PreProcessResult preProcess(WGA wga, PreProcessData data, String code) {

        PreProcessResult result = new PreProcessResult();
        result.setCode(code);
        
        switch (data.getFile().getName().getExtension()) {
            
            case "scss": 
                result.setPostProcessor(SassPostProcessor.class);
                break;
            
        }
        
        return result;
    
    }

}
