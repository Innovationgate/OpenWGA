package de.innovationgate.wga.additional_script_langs.typescript;

import java.io.StringReader;
import java.io.StringWriter;

import ro.isdc.wro.config.Context;
import ro.isdc.wro.config.jmx.WroConfiguration;
import ro.isdc.wro.model.group.processor.Injector;
import ro.isdc.wro.model.group.processor.InjectorBuilder;
import ro.isdc.wro.model.resource.Resource;
import ro.isdc.wro.model.resource.ResourceType;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.PreProcessData;
import de.innovationgate.wgpublisher.design.conversion.PreProcessResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.ObjectStrategy;

public abstract class TypeScriptConversion implements DesignResourceConversion {

    @Override
    public PreProcessResult preProcess(WGA wga, PreProcessData data, String code) throws WGException {
        
        ClassLoader oldLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(wga.server().getLibraryLoader());
        
        try {
            WroConfiguration config = new WroConfiguration();
            Context.set(Context.standaloneContext(), config);
            
            Injector injector = new InjectorBuilder().build();
            
            Design design = data.getApp().design().resolve(data.getDocumentKey().getName());
            
            RhinoTypeScriptProcessor processor = new RhinoTypeScriptProcessor();
            injector.inject(processor);
            StringWriter minOut = new StringWriter();
            processor.process(Resource.create(data.getDesignReference().toString(), ResourceType.JS), new StringReader(code), minOut);
            
            PreProcessResult result = new PreProcessResult();
            result.setCode(minOut.toString());
            result.getExtensionData().put(RhinoExpressionEngine.EXTDATA_OBJECTSTRATEGY, ObjectStrategy.MAINOBJECT_ON_EXPORTS.toString());
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
