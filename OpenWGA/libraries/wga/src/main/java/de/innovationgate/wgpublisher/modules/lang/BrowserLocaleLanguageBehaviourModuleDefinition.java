package de.innovationgate.wgpublisher.modules.lang;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.LanguageBehaviourModuleType;
import de.innovationgate.wgpublisher.lang.BrowserLocaleLanguageBehaviour;

public class BrowserLocaleLanguageBehaviourModuleDefinition implements ModuleDefinition{

    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(BrowserLocaleLanguageBehaviourModuleDefinition.class) + "/lang", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("browserlacale.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return BrowserLocaleLanguageBehaviour.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return LanguageBehaviourModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
    	return null;
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("browserlacale.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }
}
