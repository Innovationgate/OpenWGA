package de.innovationgate.wga.additional_script_langs.coffeescript;

import java.io.StringReader;
import java.io.StringWriter;

import ro.isdc.wro.config.Context;
import ro.isdc.wro.config.jmx.WroConfiguration;
import ro.isdc.wro.extensions.processor.js.CoffeeScriptProcessor;
import ro.isdc.wro.model.group.processor.Injector;
import ro.isdc.wro.model.group.processor.InjectorBuilder;
import ro.isdc.wro.model.resource.Resource;
import ro.isdc.wro.model.resource.ResourceType;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.PreProcessData;
import de.innovationgate.wgpublisher.design.conversion.PreProcessResult;

public abstract class CoffeeScriptConversion implements DesignResourceConversion {

    @Override
    public PreProcessResult preProcess(WGA wga, PreProcessData data, String code) throws WGException {
        
        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(wga.server().getLibraryLoader());
        try {
            WroConfiguration config = new WroConfiguration();
            Context.set(Context.standaloneContext(), config);
            
            Injector injector = new InjectorBuilder().build();
            
            CoffeeScriptProcessor processor = new CoffeeScriptProcessor();
            injector.inject(processor);
            StringWriter minOut = new StringWriter();
            processor.process(Resource.create(data.getDesignReference().toString(), ResourceType.JS), new StringReader(code), minOut);
            
            PreProcessResult result = new PreProcessResult();
            result.setCode(minOut.toString());
            
            // Not necessary any more since #00004412 and the exported constructor functionality
            //result.getExtensionData().put(RhinoExpressionEngine.EXTDATA_OBJECTSTRATEGY, ObjectStrategy.MAINOBJECT_ON_THIS.toString());
            
            return result;
            
        }
        catch (Exception e) {
            throw new WGBackendException("Exception pre-processing " + data.getDocumentKey().toString(), e);
        }
        finally {
            Context.unset();
            Thread.currentThread().setContextClassLoader(oldLoader);
        }
    }

}
