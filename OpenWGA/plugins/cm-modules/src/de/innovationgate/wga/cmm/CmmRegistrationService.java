package de.innovationgate.wga.cmm;

import java.io.IOException;
import java.io.Reader;
import java.net.URLClassLoader;
import java.util.Collection;
import java.util.List;

import net.sf.json.JSONArray;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wga.cmm.definition.Definition;
import de.innovationgate.wga.cmm.modules.CmmModuleDefinition;
import de.innovationgate.wga.modules.CustomModuleRegistrationService;
import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class CmmRegistrationService implements CustomModuleRegistrationService {

    public static final String CMM_DEFINITIONS = "cmm.json";

    private void registerCMM(ModuleRegistry registry, String dbKey, Reader cmmDefReader) throws Exception {
        
        Collection<Definition> cmdefs = JSONArray.toCollection(JSONArray.fromObject(WGUtils.readString(cmmDefReader)), Definition.class);
        
        for (Definition def : cmdefs) {
            def.setDbkey(dbKey);
            registry.addModuleDefinition(new CmmModuleDefinition(def, dbKey));
        }
    }

    public void registerModules(ModuleRegistry registry) {
        
        WGACore core = (WGACore) registry.getContextObjects().get(WGACore.class);
        
        for (WGAPlugin plugin : core.getPluginSet().getActivePluginsByUniqueName().values()) {
            WGDatabase db = core.getContentdbs().get(plugin.buildDatabaseKey());
            if (db == null || !db.isReady()) {
                continue;
            }
            
            try {
                if (!db.isSessionOpen()) {
                    db.openSession();
                }
                
                WGFileContainer systemFC = db.getFileContainer("system");
                if (systemFC == null) {
                    continue;
                }
                
                if (systemFC.hasFile(CMM_DEFINITIONS)) {
                    registerCMM(registry, db.getDbReference(), systemFC.getFileText(CMM_DEFINITIONS));
                }
                
                
            }
            catch (Exception e) {
                core.getLog().error("Exception reading CMM definitions from database " + db.getDbReference(), e);
            }
        }
        
        
    }

    public void searchModuleDefinitions(ModuleRegistry registry, ClassLoader classLoader) throws IOException {
        registerModules(registry);
    }

    public void searchModuleDefinitions(ModuleRegistry registry, ClassLoader classLoader, Iterable<URLClassLoader> registrarCfgLoaders) throws IOException {
        registerModules(registry);
    }

}
