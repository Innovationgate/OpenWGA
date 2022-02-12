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
package de.innovationgate.wgpublisher.mail;

import java.util.Properties;

import javax.mail.Authenticator;
import javax.mail.Session;

import de.innovationgate.wga.config.MailConfiguration;

public class WGAMailConfiguration {
	
	private Authenticator _authenticator;
	private Properties _mailProperties;
	private MailConfiguration _config;
	
	private WGAMailConfiguration(MailConfiguration config) {
		_config = config;
	}	
	
	public static WGAMailConfiguration create(MailConfiguration mailConfig) {
		if (mailConfig.isConfigured()) {
			WGAMailConfiguration config = new WGAMailConfiguration(mailConfig);
			
			// init config
			config.init();
			
			return config;
		} else {
			return null;
		}
	}	
	
	/**
	 * creates a mail session based on the current configuration
	 * @return
	 */
	public Session createMailSession() {
		
		if (_mailProperties == null) {
			return null;
		}
				
		if (_authenticator == null) {
			return Session.getInstance(_mailProperties);	
		} else {
			return Session.getInstance(_mailProperties, _authenticator);
		}
		
	}
	
	private void init() {
		// init mail props and authenticator
		_mailProperties = new Properties();
		_mailProperties.put("mail.smtp.host", _config.getServer());
		_mailProperties.put("mail.smtp.connectiontimeout", "10000");
		_mailProperties.put("mail.smtp.timeout", "10000");
		
		String encryption = _config.getEncryption();
		if(encryption!=null){
			if(encryption.equals("tls")){
				_mailProperties.put("mail.smtp.port", "587");
				_mailProperties.put("mail.smtp.starttls.enable", "true");
			}
			else if(encryption.equals("ssl")){
				_mailProperties.put("mail.smtp.port", "465");
				_mailProperties.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
			}
		}
		
		if (_config.getUser() != null) {
			_mailProperties.put("mail.smtp.user", _config.getUser());
			if (_config.getPassword() != null) {
				_mailProperties.put("mail.smtp.auth", "true");
				_authenticator = new WGAMailUserPasswordAuthenticator(_config.getUser(), _config.getPassword());
			}
		}
		
		_mailProperties.putAll(_config.getOptions());
	}

	public Properties getMailProperties() {
		return _mailProperties;
	}

	public boolean isEnableAdminNotifications() {
		return _config.isEnableAdminNotifications();
	}

	public String getFromAddress() {
		return _config.getFromAddress();
	}
	
	public String getFromName() {
        return _config.getFromName();
    }

	public String getToAddress() {
		return _config.getToAddress();
	}		
}
