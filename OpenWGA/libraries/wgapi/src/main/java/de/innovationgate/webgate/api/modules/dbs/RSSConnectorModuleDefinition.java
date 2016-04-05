/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.webgate.api.modules.dbs;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.rss2.SimpleRSS;
import de.innovationgate.webgate.api.servers.GenericServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer;
import de.innovationgate.webgate.api.servers.RSSDatabaseRetriever;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;

public class RSSConnectorModuleDefinition extends GenericContentDatabaseModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/rssconnector", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return SimpleRSS.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentDatabaseModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition proxy = new LocalizedOptionDefinition(SimpleRSS.COPTION_PROXY, StringOptionType.INSTANCE, _bundleLoader);
        proxy.setOptional(true);
        options.addOption(proxy);
        
        LocalizedOptionDefinition proxyDomain = new LocalizedOptionDefinition(SimpleRSS.COPTION_PROXY_DOMAIN, StringOptionType.INSTANCE, _bundleLoader);
        proxyDomain.setOptional(true);
        proxyDomain.setExpert(true);
        proxyDomain.addDependentOption(SimpleRSS.COPTION_PROXY);
        options.addOption(proxyDomain);
        
        LocalizedOptionDefinition proxyCredentials = new LocalizedOptionDefinition(SimpleRSS.COPTION_PROXY_CREDENTIALS, StringOptionType.INSTANCE, _bundleLoader);
        proxyCredentials.setOptional(true);
        proxyCredentials.addDependentOption(SimpleRSS.COPTION_PROXY);
        options.addOption(proxyCredentials);
        
        options.putAll(super.getOptionDefinitions());
        
        return options;
        
    }

    public Object getProperties() {
        DatabaseProperties props = new DatabaseProperties();
        RSSDatabaseRetriever sdr = new RSSDatabaseRetriever();
        props.addServerDatabaseRetriever(sdr);
        return props;
    }

    public String getTitle(Locale locale) {
        return "RSS Feed Connector";
    }

}
