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

package de.innovationgate.wgpublisher;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.license.LicenseException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.auth.AuthModuleFactory;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.wga.config.AuthenticationSource;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.Domain;
import de.innovationgate.wga.config.PersonalisationDatabase;
import de.innovationgate.wgpublisher.WGACore.UpdateConfigOccasion;
import de.innovationgate.wgpublisher.problems.DomainScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;

public class WGADomain {
	public static final String AUTHOPTION_DOMAIN = "wga.domain";
    /**
     * 
     */
    private final WGACore _wgaCore;
    private Domain _config;
	private AuthenticationModule _authModule;

    /**
     * @return Returns the name.
     */
    public String getName() {
        return _config.getName();
    }

    public WGADomain(WGACore wgaCore, String name) {
        _wgaCore = wgaCore;
        _config = new Domain(name);
        _config.setName(name);
    }

    public WGADomain(WGACore wgaCore, Domain config) {
    	_wgaCore = wgaCore;
        _config = config;
    }

    protected void init() {
    	if (_config != null) {
            // Authentication
            try {
                AuthModuleFactory authFactory = WGFactory.getAuthModuleFactory();
                AuthenticationSource authSource = _config.getAuthenticationSource();
                if (authSource != null) {
                    Map<String,String> options = new HashMap<String,String>();
                    options.putAll(authSource.getOptions());
                    options.put(AUTHOPTION_DOMAIN, getName());
                	_authModule = authFactory.getAuthModule(authSource.getImplClassName(), options, null);
                	_wgaCore.getLog().info("Domain uses authentication: " + getAuthModule().getAuthenticationSource());
                }
                else {
                    _wgaCore.getLog().info("Domain uses no authentication.");
                }
                
            }
            catch (de.innovationgate.webgate.api.auth.ConfigurationException e) {
                _wgaCore.getLog().error("Exception setting up authentication of domain '" + getName() + "'", e);
                if (e.getCause() instanceof LicenseException) {
                    _wgaCore.getProblemRegistry().addProblem(Problem.create(new WGACore.UpdateConfigOccasion(), new DomainScope(getName()), "updateConfigProblem.domainAuthLicenseError", ProblemSeverity.HIGH, e));
                }
                else {
                    _wgaCore.getProblemRegistry().addProblem(Problem.create(new WGACore.UpdateConfigOccasion(), new DomainScope(getName()), "updateConfigProblem.domainAuthFailed", ProblemSeverity.HIGH, e));
                }
            }
            
            // Personalisation
            WGDatabase db = null;
            PersonalisationDatabase persDBConfig = _config.getPersonalisation();
            if (persDBConfig != null && persDBConfig.isEnabled()) {
            	            
	            DatabaseServer serverConfig = (DatabaseServer) _wgaCore._wgaConfiguration.getByUid(persDBConfig.getDbServer());
	            if (serverConfig == null || serverConfig.isEnabled()) {

    	            try {
                        // Get or retrieve db
                        if (_wgaCore.personalisationdbs.containsKey(_config.getUid())) {
                            db = _wgaCore.personalisationdbs.get(_config.getUid());
                        }
                        else {
                            db = _wgaCore.retrievePersonalisationDB(_config);
                        }
                        _wgaCore.getLog().info("Domain uses domain-wide personalisation");
                    }
    	            catch (Problem p) {
    	                _wgaCore.getLog().error("Exception setting up personalisation of domain '" + getName() + "'", p);
    	                _wgaCore.getProblemRegistry().addProblem(p);
    	            }
                    catch (WGAServerException e) {
                        _wgaCore.getLog().error("Exception setting up personalisation of domain '" + getName() + "'", e);
                        _wgaCore.getProblemRegistry().addProblem(Problem.create(new WGACore.UpdateConfigOccasion(), new DomainScope(getName()), "updateConfigProblem.domainPersFailed", ProblemSeverity.HIGH, e));
                    }
    	            
	            }
            }

            // DB was retrieved or fetched, so it is active. Map it.
            if (db != null) {
                _wgaCore.personalisationdbs.put(_config.getUid(), db);
            }
            
            // Otherwise close existing instance
            else {
                _wgaCore.removePersonalisationDB(_config.getUid());
            }
            

            
    	}
    }

    /**
     * @return Returns the authModule.
     */
    public AuthenticationModule getAuthModule() {
        return _authModule;
    }
    










    @Override
	public String toString() {
		if (_config != null) {
			return _config.toString();
		} else {					
			return super.toString();
		}
	}
    
    public void destroy() {
       if (_authModule != null) {
           _authModule.destroy();
           _authModule = null;
       }
    }
    
    public String getUID() {
        return _config.getUid();
    }

    public Domain getConfig() {
        return _config;
    }
}