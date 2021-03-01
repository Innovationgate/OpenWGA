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

package de.innovationgate.wgpublisher.modules;

import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wgpublisher.modules.auth.CSAuthModuleDefinition;
import de.innovationgate.wgpublisher.modules.auth.PluginAuthModuleDefinition;
import de.innovationgate.wgpublisher.modules.derivatecreators.ManualDerivateCreatorModuleDefinition;
import de.innovationgate.wgpublisher.modules.derivatecreators.ThumbnailDerivateCreatorModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.provider.DBDesignProviderModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.provider.FileSystemDesignProviderModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.provider.PluginDesignProviderModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.sources.DBDesignSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.sources.DefaultFileSystemDesignSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.sources.FileSystemDesignSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.design.sources.PluginDesignSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.encoders.ConciseEncoderModuleDefinition;
import de.innovationgate.wgpublisher.modules.encoders.OneLineEncoderModuleDefinition;
import de.innovationgate.wgpublisher.modules.fileannotators.WGADefaultFileAnnotatorModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.ContextDocumentFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.NewContentFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.NoneFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.PortletConfigFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.PortletSessionVarsFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.RequestFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.formsources.UserProfileFormSourceModuleDefinition;
import de.innovationgate.wgpublisher.modules.imagescaler.ImgScalrFactoryModuleDefinition;
import de.innovationgate.wgpublisher.modules.imagescaler.ImgScalrScalerModuleDefinition;
import de.innovationgate.wgpublisher.modules.joboptions.JobOptionsModuleDefinition;
import de.innovationgate.wgpublisher.modules.lang.BrowserLocaleLanguageBehaviourModuleDefinition;
import de.innovationgate.wgpublisher.modules.lang.DynamicLanguageBehaviourModuleDefinition;
import de.innovationgate.wgpublisher.modules.lang.OnlyDefaultLanguageBehaviourModuleDefinition;
import de.innovationgate.wgpublisher.modules.lang.StaticLanguageBehaviourModuleDefinition;
import de.innovationgate.wgpublisher.modules.mailoptions.MailOptionsCollector;
import de.innovationgate.wgpublisher.modules.poptions.BaseContentDatabasePublisherOptionsModuleDefinition;
import de.innovationgate.wgpublisher.modules.poptions.BaseContentStorePublisherOptionsModuleDefinition;
import de.innovationgate.wgpublisher.modules.poptions.ContentDatabasePublisherOptionsCollector;
import de.innovationgate.wgpublisher.modules.poptions.ContentStorePublisherOptionsCollector;
import de.innovationgate.wgpublisher.modules.schedulertasks.JavaTaskModuleDefinition;
import de.innovationgate.wgpublisher.modules.schedulertasks.ScriptTaskModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.BasicModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.BasicServicesModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.BasicVariousOptionsModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.CacheModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.LogModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.ServiceApisModuleDefinition;
import de.innovationgate.wgpublisher.modules.serveroptions.ServicesCollector;
import de.innovationgate.wgpublisher.modules.serveroptions.VariousOptionsCollector;
import de.innovationgate.wgpublisher.modules.servers.HsqlDefaultDatabaseServerModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.AdminNotificationApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.AppLogApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.HashedPassswordServiceApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.HashingServiceApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.ImageLinkReaderServiceModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.ImageScalerApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.ImageScalerFactoryApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.SessionManagerApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.SrcSetCreatorApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.serviceapis.TempDownloadApiModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.ContentVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.ContextExpressionVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.ExternalVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.FileVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.LegacyFileVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.LocalFileVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.NameVirtualLinkResolverModuleDefinition;
import de.innovationgate.wgpublisher.modules.vlink.PrimaryFileVirtualLinkResolverModuleDefinition;

public class WGARegistrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {

        // WGA Server options
        registry.addModuleDefinition(new BasicModuleDefinition());
        
        registry.addModuleDefinition(new ServicesCollector());
        registry.addModuleDefinition(new BasicServicesModuleDefinition());
        registry.addModuleDefinition(new ServiceApisModuleDefinition());
        
        registry.addModuleDefinition(new LogModuleDefinition());
        registry.addModuleDefinition(new CacheModuleDefinition());
        registry.addModuleDefinition(new VariousOptionsCollector());
        registry.addModuleDefinition(new BasicVariousOptionsModuleDefinition());
        
        
        // Mail options
        registry.addModuleDefinition(new MailOptionsCollector());

        // Design Sources
        registry.addModuleDefinition(new DBDesignSourceModuleDefinition());
        registry.addModuleDefinition(new PluginDesignSourceModuleDefinition());
        registry.addModuleDefinition(new FileSystemDesignSourceModuleDefinition());
        registry.addModuleDefinition(new DefaultFileSystemDesignSourceModuleDefinition());
        
        // Design Provider
        registry.addModuleDefinition(new DBDesignProviderModuleDefinition());
        registry.addModuleDefinition(new PluginDesignProviderModuleDefinition());
        registry.addModuleDefinition(new FileSystemDesignProviderModuleDefinition());
        
        // Publisher options
        registry.addModuleDefinition(new ContentDatabasePublisherOptionsCollector());
        registry.addModuleDefinition(new ContentStorePublisherOptionsCollector());
        registry.addModuleDefinition(new BaseContentDatabasePublisherOptionsModuleDefinition());
        registry.addModuleDefinition(new BaseContentStorePublisherOptionsModuleDefinition());

        // Servers
        registry.addModuleDefinition(new HsqlDefaultDatabaseServerModuleDefinition());
        
        // Auth Modules
        registry.addModuleDefinition(new CSAuthModuleDefinition());
        registry.addModuleDefinition(new PluginAuthModuleDefinition());
        
        // Scheduler Tasks
        registry.addModuleDefinition(new ScriptTaskModuleDefinition());
        registry.addModuleDefinition(new JavaTaskModuleDefinition());
        
        // Language behaviours
        registry.addModuleDefinition(new StaticLanguageBehaviourModuleDefinition());
        registry.addModuleDefinition(new DynamicLanguageBehaviourModuleDefinition());
        registry.addModuleDefinition(new OnlyDefaultLanguageBehaviourModuleDefinition());
        registry.addModuleDefinition(new BrowserLocaleLanguageBehaviourModuleDefinition());
        
        // WebTML form sources
        registry.addModuleDefinition(new NoneFormSourceModuleDefinition());
        registry.addModuleDefinition(new RequestFormSourceModuleDefinition());
        registry.addModuleDefinition(new NewContentFormSourceModuleDefinition());
        registry.addModuleDefinition(new ContextDocumentFormSourceModuleDefinition());
        registry.addModuleDefinition(new UserProfileFormSourceModuleDefinition());
        registry.addModuleDefinition(new PortletConfigFormSourceModuleDefinition());
        registry.addModuleDefinition(new PortletSessionVarsFormSourceModuleDefinition());
        
        // Service Apis
        registry.addModuleDefinition(new HashingServiceApiModuleDefinition());
        registry.addModuleDefinition(new ImageScalerApiModuleDefinition());
        registry.addModuleDefinition(new ImageScalerFactoryApiModuleDefinition());
        registry.addModuleDefinition(new TempDownloadApiModuleDefinition());
        registry.addModuleDefinition(new AdminNotificationApiModuleDefinition());
        registry.addModuleDefinition(new AppLogApiModuleDefinition());
        registry.addModuleDefinition(new ImageLinkReaderServiceModuleDefinition());
        registry.addModuleDefinition(new SrcSetCreatorApiModuleDefinition());
        registry.addModuleDefinition(new HashedPassswordServiceApiModuleDefinition());
        registry.addModuleDefinition(new SessionManagerApiModuleDefinition());
        
        // WebTML Encoders
        registry.addModuleDefinition(new OneLineEncoderModuleDefinition());
        registry.addModuleDefinition(new ConciseEncoderModuleDefinition());       
        
        // File derivate creators
        registry.addModuleDefinition(new ThumbnailDerivateCreatorModuleDefinition());
        registry.addModuleDefinition(new ManualDerivateCreatorModuleDefinition());
        
        // File annotators
        registry.addModuleDefinition(new WGADefaultFileAnnotatorModuleDefinition());
        
        // Generic Image scalers
        registry.addModuleDefinition(new ImgScalrScalerModuleDefinition());
        
        // Mimetype-specific image scaler factory
        registry.addModuleDefinition(new ImgScalrFactoryModuleDefinition());
        
        // Cluster service implementations
        registry.addModuleDefinition(new SingleNodeClusterServiceModuleDefinition());
        
        // Virtual Link Resolvers
        registry.addModuleDefinition(new ContentVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new FileVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new ExternalVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new LegacyFileVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new LocalFileVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new NameVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new PrimaryFileVirtualLinkResolverModuleDefinition());
        registry.addModuleDefinition(new ContextExpressionVirtualLinkResolverModuleDefinition());
        
        // Job options
        registry.addModuleDefinition(new JobOptionsModuleDefinition());
        
        // HTTP Session Managers
        registry.addModuleDefinition(new InMemoryHttpSessionManagerModuleDefinition());
    }
    
    

}

