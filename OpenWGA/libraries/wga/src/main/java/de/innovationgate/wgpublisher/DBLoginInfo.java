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

import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.utils.Base64;

public class DBLoginInfo implements java.io.Serializable {
    
        public enum AuthType implements Serializable {
            PASSWORD("password"),CERTIFICATE("cert"),REQUEST("request");
            
            private String _strValue;
            
            private AuthType(String strValue) {
                _strValue = strValue;
            }
            
            @Override
            public String toString() {
                return _strValue;
            }
        }
        

        private static final long serialVersionUID = 3L;
        
        private String _userName;
		private Object _credentials;
		private String _accessFilter;
		private AuthType _authenticationType;
		
		private Map<String,String> _dbAccessFilters;
		
		public DBLoginInfo(String name, Object credentials, AuthType loginType) {
		    this(name, credentials, loginType, null, null);
		}
		
		public DBLoginInfo(String name, Object credentials, AuthType loginType, String accessFilter, Map<String,String> dbAccessFilters) {
			_userName = name;
			_credentials = credentials;
			_authenticationType = loginType;
			_accessFilter = accessFilter;
			_dbAccessFilters = (dbAccessFilters != null ? dbAccessFilters : new ConcurrentHashMap<String, String>());
		}
		
		public static DBLoginInfo createFromHttpCredentials(String usercredentials) throws IOException {
			
			int p = usercredentials.indexOf(" ");
			
			if (p == -1) {
				return null;
			}
			
			String userPass = usercredentials.substring(p).trim();
			userPass = new String(Base64.decode(userPass));

			p = userPass.indexOf(":");
			
			if (p== -1) {
				return null;
			}
			String userName = userPass.substring(0, p);
			String credentials = userPass.substring(p+1);
			
			return new DBLoginInfo(userName, credentials, AuthType.PASSWORD);
		}
		
	/**
	 * Gets the credentials
	 * @return Returns an Object
	 */
	public Object getCredentials() {
		return _credentials;
	}
    
    /**
     * gets the password if credentials is a String, null otherwise
     */
    public String getPassword() {
        if (_credentials instanceof String) {
            return (String) _credentials;
        }
        return null;
    }
	
	/**
	 * Gets the userName
	 * @return Returns a String
	 */
	public String getUserName() {
		return _userName;
	}
		/**
		 * @param string
		 */
		public void setCredentials(Object credentials) {
			_credentials = credentials;
		}

		/**
		 * @param string
		 */
		public void setUserName(String string) {
			_userName = string;
		}

        public String getAccessFilter() {
            return _accessFilter;
        }

        public void setAccessFilter(String accessFilter) {
            _accessFilter = accessFilter;
        }

        public Map<String, String> getDbAccessFilters() {
            return _dbAccessFilters;
        }

        public void setDbAccessFilters(Map<String, String> dbAccessFilters) {
            _dbAccessFilters = dbAccessFilters;
        }

        public AuthType getAuthenticationType() {
            return _authenticationType;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_accessFilter == null) ? 0 : _accessFilter.hashCode());
            result = prime * result + ((_authenticationType == null) ? 0 : _authenticationType.hashCode());
            result = prime * result + ((_credentials == null) ? 0 : _credentials.hashCode());
            result = prime * result + ((_dbAccessFilters == null) ? 0 : _dbAccessFilters.hashCode());
            result = prime * result + ((_userName == null) ? 0 : _userName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            DBLoginInfo other = (DBLoginInfo) obj;
            if (_accessFilter == null) {
                if (other._accessFilter != null)
                    return false;
            }
            else if (!_accessFilter.equals(other._accessFilter))
                return false;
            if (_authenticationType != other._authenticationType)
                return false;
            if (_credentials == null) {
                if (other._credentials != null)
                    return false;
            }
            else if (!_credentials.equals(other._credentials))
                return false;
            if (_dbAccessFilters == null) {
                if (other._dbAccessFilters != null)
                    return false;
            }
            else if (!_dbAccessFilters.equals(other._dbAccessFilters))
                return false;
            if (_userName == null) {
                if (other._userName != null)
                    return false;
            }
            else if (!_userName.equals(other._userName))
                return false;
            return true;
        }

}

