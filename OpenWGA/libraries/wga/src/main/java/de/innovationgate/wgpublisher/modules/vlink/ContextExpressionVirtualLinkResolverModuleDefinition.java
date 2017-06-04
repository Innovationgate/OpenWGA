package de.innovationgate.wgpublisher.modules.vlink;

import java.util.Locale;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.VirtualLinkResolverModuleType;
import de.innovationgate.wgpublisher.vlink.ContextExpressionVirtualLinkResolver;

public class ContextExpressionVirtualLinkResolverModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition{

	@Override
	public String getRegistrationKey() {
		return WGContent.VIRTUALLINKTYPE_CONTEXREXPRESSION;
	}

	@Override
	public String getTitle(Locale locale) {
		return "Context Expression Virtual Link Resolver";
	}

	@Override
	public String getDescription(Locale locale) {
		return "Virtual Link resolver for links to content documents in the same database, addressed by some context expression";
	}

	@Override
	public OptionDefinitionsMap getOptionDefinitions() {
		return null;
	}

	@Override
	public Class<? extends ModuleType> getModuleType() {
		return VirtualLinkResolverModuleType.class;
	}

	@Override
	public Class<? extends Object> getImplementationClass() {
		return ContextExpressionVirtualLinkResolver.class;
	}

	@Override
	public void testDependencies() throws ModuleDependencyException {
	}

	@Override
	public Object getProperties() {
		return null;
	}

}
