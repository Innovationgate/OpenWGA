package de.innovationgate.wgpublisher.modules.imagescaler;

import java.util.Locale;

import de.innovationgate.utils.imgscalr.ImgScalrScaler;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.ImageScalerModuleType;

public class ImgScalrScalerModuleDefinition implements ModuleDefinition {

    @Override
    public String getTitle(Locale locale) {
        return "ImgScalr Image Processing";
    }

    @Override
    public String getDescription(Locale locale) {
        return "Image processing based on Java library ImgScalr";
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return ImageScalerModuleType.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return ImgScalrScaler.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

}
