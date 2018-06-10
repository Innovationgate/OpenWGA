package de.innovationgate.wgpublisher.modules;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wga.modules.options.OptionValueValidationException;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.options.ValidationContext;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType.DatabaseType;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet;

public class AuthoringAppOptionType implements OptionType {

	public static final AuthoringAppOptionType INSTANCE = new AuthoringAppOptionType();

	private WGACore _core;
	
	private AuthoringAppOptionType(){
		/*
        super(null, null);
        addValue("plugin-contentmanager");
        addValue("plugin-cm-next");
        */
		try {
			_core = WGA.get().getCore();
		} catch (WGException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
	
	public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
		final WGAConfiguration _config = configCopy;
		return new OptionValueProvider(){

			@Override
			public List<String> getProvidedValues() {
				List<String> dbkeys = new ArrayList<String>();
				
				// Contentstores
	            Iterator<ContentDatabase> dbs = _config.getContentDatabases().iterator();
	            while (dbs.hasNext()) {
	                ContentDatabase contentDatabase = (ContentDatabase) dbs.next();
	                if (!contentDatabase.isEnabled()) {
	                    continue;
	                }
	                
	                if(contentDatabase.getPublisherOptions().containsKey(WGACore.DBATTRIB_AUTHORING_APP)){
	                	String authoringAppOption = contentDatabase.getPublisherOptions().get(WGACore.DBATTRIB_AUTHORING_APP);
	                	if(authoringAppOption!=null && WGUtils.stringToBoolean(authoringAppOption))
	                		dbkeys.add(contentDatabase.getKey());
	                }
	            }

	            // Plugins (more complicated then it SHOULD be bc. getActivePlugins() also contains disabled plugins)
	            WGAPluginSet plugins = _core.getPluginSet();
	            for(Entry<String, WGAPlugin> entry : plugins.getActivePluginsByUniqueName().entrySet()){
	            	String uniqueName = entry.getKey();
	            	WGAPlugin plugin = entry.getValue();
                	if(plugins.isPluginDeactivated(uniqueName))
                		continue;
                	PublisherOption authoringAppOption = plugin.getCsConfig().findPublisherOption(WGACore.DBATTRIB_AUTHORING_APP);
                	if(authoringAppOption!=null && WGUtils.stringToBoolean(authoringAppOption.getValue()))
                		dbkeys.add(plugin.buildDatabaseKey());
                }
	            
	            return dbkeys;

			}

			@Override
			public String getValueTitle(String value, Locale locale) {
				return value;
			}

			@Override
			public String getEmptyListMessage(Locale locale) {
				return "- no authoring App selected -";
			}
			
		};
	}

	@Override
	public Class<? extends Object> getDataTypeHint() {
		return String.class;
	}

	@Override
	public boolean isRestricted() {
		return true;
	}

	@Override
	public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(cx.getConfigCopy());
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The authoring app '" + value + "' in either unknown, not enabled or no content store");
        }
	}

	@Override
	public boolean isEmptyAllowed() {
		return true;
	}

	@Override
	public boolean isMultiValue() {
		return false;
	}

}
