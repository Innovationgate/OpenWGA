/*
 * Created on Oct 31, 2006 from ow
 *
 */
package de.innovationgate.testclasses;

import java.io.File;
import java.io.FileNotFoundException;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.hsql.HsqlDatabaseServer;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.servers.HsqlDefaultDatabaseServer;

public class MyEventListener implements WGACoreEventListener {

    public void contentStoreConnected(WGACoreEvent event) {
        // Set attribute for unittest to test if the listener ran
        boolean result = (event.getCore().getContentdbs().get("designslave") != null);
        event.getCore().getServletContext().setAttribute("CoreEventListener:contentStoreConnected", new Boolean(result));        
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
        // Set attribute for unittest to test the listener ran
        event.getCore().getServletContext().setAttribute("CoreEventListener:contentStoreDisconnected", event.getDatabase().getDbReference());
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {
        
        // Set attribute for unittest to test if the listener ran
        boolean result = (event.getCore().getContentdbs().size() > 0);
        event.getCore().getServletContext().setAttribute("CoreEventListener:startupPostConnect", new Boolean(result));
        
    }

    public void startupPreConnect(WGACoreEvent event) {
        
        // Test java assertion status. If false we issue a false WGA assertion to notify
        boolean assertsEnabled = false;
        assert assertsEnabled = true; // Intentional side effect!!!
        event.getCore().getServletContext().setAttribute("CoreEventListener:javaAssertionsEnabled", new Boolean(assertsEnabled));
                       
               
        // Set attribute for unittest to test if the listener ran
        boolean result = (event.getCore().getContentdbs().size() == 0);
        event.getCore().getServletContext().setAttribute("CoreEventListener:startupPreConnect", new Boolean(result));
        Logger log = event.getCore().getLog();
        
        // Delete hsql database to test expanding of init.wgacs and reinit the target cs for replication tests
        File configDir = event.getCore().getConfigFile().getParentFile();
        HsqlDefaultDatabaseServer server = (HsqlDefaultDatabaseServer) event.getCore().getDatabaseServers().get(WGAConfiguration.SINGLETON_SERVER_PREFIX + HsqlDefaultDatabaseServer.class.getName());
        
        File hsqlDir = new File(configDir, "wgadata/#dbs" + HsqlDatabaseServer.V1_PATH_PREFIX);
        File[] files = hsqlDir.listFiles();
        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                File file = files[i];
                if (file.getName().startsWith("inittest.") ||
                    file.getName().startsWith("synctarget.") ||
                    file.getName().startsWith("synctarget_domino.") ||
                    file.getName().startsWith("testurls.") ||
                    file.getName().startsWith("design_overlay_base.") ||
                    file.getName().startsWith("perso.") ||
                    file.getName().startsWith("test4.") ||
                    file.getName().startsWith("test5.") ||
                    file.getName().startsWith("testhdbmodel.") ||
                    file.getName().startsWith("test-rest.")) {
                    log.info("Deleting file " + file.getName());
                    file.delete();
                }
            }
        }
        
        // Delete wga plugins dir contents to test plugin dir setup and default-plugins installation
        log.info("Deleting the plugins registration");
        File dataDir = event.getCore().getWgaDataDir();
        File pluginsDir = new File(dataDir, "plugins");
        File[] pluginFiles = pluginsDir.listFiles();
        if (pluginFiles != null) {
            for (int i = 0; i < pluginFiles.length; i++) {
                File file = pluginFiles[i];
                log.info("Deleting file " + file.getName());
                WGUtils.delTree(file);
            }
        }
        
        // Delete designs that should not be there in start
        File designOverlayInit = new File(configDir, "../designs/design_overlay_init");
        for (File file : designOverlayInit.listFiles()) {
            log.info("Deleting file " + file.getName());
            WGUtils.delTree(file);
        }
        
        // Remove applied patch level information so patcher runs on each start
        event.getCore().getWgaConfiguration().getServerOptions().remove("de.innovationgate.csmaintenance.AppliedPatchLevel");
    }

}
