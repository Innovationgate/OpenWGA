package de.innovationgate.wga.additional_script_langs.sass;

import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.PreProcessData;
import de.innovationgate.wgpublisher.design.conversion.PreProcessResult;

public class SassConversion implements DesignResourceConversion {

    
    @Override
    public PreProcessResult preProcess(WGA wga, PreProcessData data, String code) {

		// No pre-processing. Do nothing. Conversion is done in post-processing.

        PreProcessResult result = new PreProcessResult();
        result.setCode(code);
        result.setPostProcessor(SassPostProcessor.class);
        return result;
    
    }

}
