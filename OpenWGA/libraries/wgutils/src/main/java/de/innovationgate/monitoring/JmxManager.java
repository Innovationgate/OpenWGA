/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.monitoring;

import java.util.List;

import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

import org.apache.log4j.Logger;

public class JmxManager {
    
    public static final String SYSPROPERTY_DISABLE_JMX = "de.innovationgate.wga.jmx.disable";
    public static final boolean JMX_DISABLED = Boolean.valueOf(System.getProperty(SYSPROPERTY_DISABLE_JMX)).booleanValue();
    private static final String JMX_KEY_UNALLOWED_CHARS = ",=:*?";
    
    public static final Logger LOG = Logger.getLogger("wga.monitoring");
    
	private MBeanServer mbeanServer;
    private ObjectName _objectName;
    
    /**
     * Normalizes an input string to be a valid JMX Object Name Key. All unallowed characters are removed. 
     */
    public static String normalizeJmxKey(String input) {
        
        StringBuffer name = new StringBuffer();
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            
            // Unmodified chars
            if (Character.isLetterOrDigit(c)) {
                name.append(c);
            }
            else if (c == ' ') {
                name.append("_");
            }
            else if (JMX_KEY_UNALLOWED_CHARS.indexOf(c) == -1) {
                name.append(c);
            }
        }

        return name.toString();
        
    }
	
	public JmxManager( Object mbean, ObjectName objectName ) throws InstanceAlreadyExistsException, MBeanRegistrationException, NotCompliantMBeanException
	{
	    
	    if (JMX_DISABLED) {
	        LOG.info("WGA JMX metrics are disabled. Bypassing registration of JMX manager.");
	        return;
	    }
	    
        _objectName = objectName;
        
		//	get mbean server
		this.mbeanServer = retrieveMBeanServer();
        if (this.mbeanServer == null) {
            throw new UnsupportedOperationException("No JMX server available");
        }

        // Register the bean
        if (this.mbeanServer.isRegistered(objectName)) {
            try {
                this.mbeanServer.unregisterMBean(objectName);
            }
            catch (InstanceNotFoundException e) {
                LOG.error("Error unregistering JMX MBean for object name:" + objectName.toString(), e);
            }
        }
        this.mbeanServer.registerMBean( mbean, objectName );
    	
	}


    private MBeanServer retrieveMBeanServer() {
        
        List servers = MBeanServerFactory.findMBeanServer(null);
        if (servers.size() > 0) {
            return (MBeanServer) servers.get(0);
        }
        else {
            return null;
        }
    }

	
	public void unregister() {
       if (JMX_DISABLED) {
            return;
        }
	    
		try
		{
			this.mbeanServer.unregisterMBean( _objectName );
		}
		catch( MBeanRegistrationException e )
    	{
    		e.printStackTrace();
    	}
		catch( InstanceNotFoundException e )
    	{
    		e.printStackTrace();
    	}
	}


    public MBeanServer getMbeanServer() {
        return mbeanServer;
    }
}
