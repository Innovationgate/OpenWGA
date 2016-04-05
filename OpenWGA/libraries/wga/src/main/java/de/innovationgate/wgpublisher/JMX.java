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
import java.lang.management.ManagementFactory;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.NoSuchObjectException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.remote.JMXAuthenticator;
import javax.management.remote.JMXConnectorServer;
import javax.management.remote.JMXConnectorServerFactory;
import javax.management.remote.JMXPrincipal;
import javax.management.remote.JMXServiceURL;
import javax.management.remote.rmi.RMIConnectorServer;
import javax.rmi.ssl.SslRMIClientSocketFactory;
import javax.rmi.ssl.SslRMIServerSocketFactory;
import javax.security.auth.Subject;

import de.innovationgate.monitoring.JmxManager;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore.WGAConfigurationOptionReader;
import de.innovationgate.wgpublisher.monitoring.WGAInformation;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;

public class JMX {
    
    private WGACore _core;
    private Registry _integratedJmxRegistry;
    private JMXConnectorServer _integratedJmxServer;
    private JmxManager _wgaMonitor;
    private Boolean _enabled;
    private Boolean _sslEnabled;
    private Integer _registryPort;
    private Integer _jmxPort;
    private String _hostName;
    
    public static class JMXProblemOccasion implements ProblemOccasion, MessageVariableProvider {
        
        public static final JMXProblemOccasion INSTANCE = new JMXProblemOccasion();
        
        private JMXProblemOccasion() {
        }

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return JMX.class;
        }

        @Override
        public Problem.Vars getDefaultMessageVariables() {
            return null;
        }
        
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
        
        
    }
    
    protected JMX(WGACore core) {
        _core = core;
        
        // Try to instantiate JmxManager. We must test if jmx classes are
        // available
        try {
            Class.forName("java.lang.management.ManagementFactory");
            _wgaMonitor = new JmxManager(new WGAInformation(_core), new ObjectName("de.innovationgate.WGAMonitor:context=" + _core.getServletContext().getServletContextName() + ",name=Information"));
            if (JmxManager.JMX_DISABLED) {
                _core.getLog().warn("WGA JMX disabled via system property");
            }
        }
        catch (UnsupportedOperationException e) {
            _core.getLog().info("WGA JMX metrics disabled because no JMX MBean server available");
        }
        catch (ClassNotFoundException e) {
            _core.getLog().warn("WGA JMX metrics are not enabled since the neccessary API is not available");
        }
        catch (Throwable e) {
            _core.getLog().warn("Error enabling WGA JMX metrics: " + e.getClass().getName() + " - " + e.getMessage());
        }
        
    }
    
    protected void setup() {
        
        _core.getProblemRegistry().clearProblemOccasion(JMXProblemOccasion.INSTANCE);
        WGAConfigurationOptionReader reader = _core.getServicesServerOptionReader();
        String hostName = (String) reader.readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_HOST);
        if (hostName == null) {
            String rootURL = _core.getWgaConfiguration().getRootURL();
            if (!WGUtils.isEmpty(rootURL)) {
                try {
                    URL url  = new URL(rootURL);
                    hostName = url.getHost();
                }
                catch (MalformedURLException e) {
                    Problem problem = Problem.create(JMXProblemOccasion.INSTANCE, "JMXFailed.invalidRootURL", ProblemSeverity.HIGH, Problem.var("rooturl", rootURL));
                    _core.getProblemRegistry().addProblem(problem);
                    _core.getLog().error("Cannot use configured root URL to build JMX URL: " + rootURL + ". Specify correct root URL in OpenWGA configuration or specify an explicit host for the JMX server.", e);
                    return;
                }
            }
        }
        
        if (hostName == null) {
            hostName = "localhost";
        }
        
        Boolean enabled = ((Boolean) reader.readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_ENABLED)).booleanValue();
        Boolean sslEnabled = ((Boolean) reader.readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_SSL)).booleanValue();
        Integer registryPort = (Integer) reader.readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_REGISTRY);
        Integer servicePort = (Integer) reader.readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_SERVICE);
        
        // If nothing changed exit
        if (WGUtils.nullSafeEquals(enabled, _enabled) &&
            WGUtils.nullSafeEquals(sslEnabled, _sslEnabled) &&
            WGUtils.nullSafeEquals(registryPort, _registryPort) &&
            WGUtils.nullSafeEquals(servicePort, _jmxPort) &&
            WGUtils.nullSafeEquals(hostName, _hostName)) {
                return;
        }
        
        
        if (_integratedJmxRegistry != null) {
            _core.logCategoryInfo("JMX", 1);
            shutdown();
        }
        
        if (enabled == true) {
        
            try {
                _core.logCategoryInfo("JMX", 1);
                JMXServiceURL url = new JMXServiceURL("service:jmx:rmi://" + hostName + ":" + servicePort + "/jndi/rmi://" + hostName + ":" + registryPort + "/jmxrmi");
                _core.getLog().info("Starting integrated JMX server under URL: " + url.toString());
                
                // Create the custom JMX Server
                
                // Ensure cryptographically strong random number generator used
                // to choose the object number - see java.rmi.server.ObjID
                //
                System.setProperty("java.rmi.server.randomIDs", "true");
                
                // Set the configured hostname
                String rmiHost = System.getProperty("java.rmi.server.hostname", "localhost");
                if (!hostName.equals(rmiHost)) {
                    if (hostName.equals("localhost")) {
                        System.clearProperty("java.rmi.server.hostname");
                    }
                    else {
                        System.setProperty("java.rmi.server.hostname", hostName);
                    }
                }
    
                // Start an RMI registry
                _integratedJmxRegistry = LocateRegistry.createRegistry(registryPort);
    
                // Retrieve the PlatformMBeanServer.
                //
                MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
    
                // Environment map.
                //
                HashMap<String,Object> env = new HashMap<String,Object>();
    
                // Provide SSL-based RMI socket factories.
                //
                // The protocol and cipher suites to be enabled will be the ones
                // defined by the default JSSE implementation and only server
                // authentication will be required.
                //
                
                if (sslEnabled) {
                    SslRMIClientSocketFactory csf = new SslRMIClientSocketFactory();
                    SslRMIServerSocketFactory ssf = new SslRMIServerSocketFactory();
                    env.put(RMIConnectorServer.RMI_CLIENT_SOCKET_FACTORY_ATTRIBUTE, csf);
                    env.put(RMIConnectorServer.RMI_SERVER_SOCKET_FACTORY_ATTRIBUTE, ssf);
                }
                
                // Create custom authenticator
                JMXAuthenticator authenticator = new JMXAuthenticator() {
                    
                    @Override
                    public Subject authenticate(Object credentials) {
    
                     // Verify that credentials is of type String[].
                        //
                        if (!(credentials instanceof String[])) {
                            // Special case for null so we get a more informative message
                            if (credentials == null) {
                                throw new SecurityException("Credentials required");
                            }
                            throw new SecurityException("Credentials should be String[]");
                        }
    
                        // Verify that the array contains three elements (username/password/realm).
                        //
                        final String[] aCredentials = (String[]) credentials;
                        if (aCredentials.length != 2) {
                            throw new SecurityException("Credentials should have 2 elements");
                        }
    
                        // Perform authentication
                        String username = (String) aCredentials[0];
                        String password = (String) aCredentials[1];
    
                        if (_core.isAdminLogin(username, password)) {
                            return new Subject(true,
                                               Collections.singleton(new JMXPrincipal(username)),
                                               Collections.EMPTY_SET,
                                               Collections.EMPTY_SET);
                        } 
                        else {
                            throw new SecurityException("Invalid credentials");
                        }
                        
                        
                    }
                };
                env.put(JMXConnectorServer.AUTHENTICATOR, authenticator);
    
                // Create an RMI connector server.
                _integratedJmxServer = JMXConnectorServerFactory.newJMXConnectorServer(url, env, mbs);
                _integratedJmxServer.start();
                
            }
            catch (Exception e1) {
                _core.getProblemRegistry().addProblem(Problem.create(JMXProblemOccasion.INSTANCE, "JMXFailed.exception", ProblemSeverity.HIGH, e1));
                _core.getLog().error("Exception creating OpenWGA JMX Server", e1);
            }
        
        }
        
        // Store configuration so we can detect changes afterwards
        _enabled = enabled;
        _sslEnabled = sslEnabled;
        _registryPort = registryPort;
        _jmxPort = servicePort;
        _hostName = hostName;

        
    }

    protected void shutdown() {

        _core.getLog().info("Shutting down integrated JMX server");
        
        if (_integratedJmxServer != null) {
            try {
                _integratedJmxServer.stop();
            }
            catch (IOException e) {
                _core.getLog().error("Exception stopping integrated JMX server", e);
            }
            _integratedJmxServer = null;
        }
        
        if (_integratedJmxRegistry != null) {
            try {
                UnicastRemoteObject.unexportObject(_integratedJmxRegistry,true);
                System.gc();
                Thread.sleep(3000);
            }
            catch (Exception e) {
                _core.getLog().error("Exception stopping RMI registry of integrated JMX server", e);
            }
            _integratedJmxRegistry = null;
        }
        
        
    }

    public JmxManager getWgaMonitor() {
        return _wgaMonitor;
    }
    
    

}
