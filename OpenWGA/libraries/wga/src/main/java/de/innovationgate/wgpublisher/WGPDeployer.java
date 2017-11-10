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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletRequest;

import org.apache.commons.transaction.locking.GenericLockManager;
import org.apache.commons.transaction.locking.LockManager;
import org.apache.commons.transaction.locking.MultiLevelLock;
import org.apache.commons.transaction.util.Log4jLogger;
import org.apache.log4j.Logger;

import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.WGADesignManager;

public class WGPDeployer implements WGACoreEventListener {

    public class TMLTagsPreProcessor implements ReplaceProcessor {

        private int lineNumber = 0;

        /*
         * (Kein Javadoc)
         * 
         * @see
         * de.innovationgate.utils.ReplaceProcessor#replace(java.lang.String,
         * int, int, java.io.Writer)
         */
        public int replace(String text, int from, int to, Writer out) throws IOException {

            // Find end position of tagname
            int tagnameEndPos = text.length();
            int nextSpacePos = text.indexOf(" ", from);
            int closeTagPos = text.indexOf(">", from);
            int closeEndTagPos = text.indexOf("/>", from);

            if (nextSpacePos != -1 && nextSpacePos < tagnameEndPos) {
                tagnameEndPos = nextSpacePos;
            }

            if (closeTagPos != -1 && closeTagPos < tagnameEndPos) {
                tagnameEndPos = closeTagPos;
            }
            
            if (closeEndTagPos != -1 && closeEndTagPos < tagnameEndPos) {
                tagnameEndPos = closeEndTagPos;
            }


            // Look if we have the shortened tml:include syntax
            String tagname = text.substring(from, tagnameEndPos);
            String localName = tagname.substring(5);
            
            List<String> additionalAtts = new ArrayList<String>();
            
            // Include shortcuts
            if (localName.startsWith("[") && localName.endsWith("]")) {
                String designdbName = null;
                String refName = localName.substring(1, localName.length() - 1);
                int slashPos = refName.indexOf("/");
                if (slashPos != -1) {
                    designdbName = refName.substring(0, slashPos);
                    refName = refName.substring(slashPos + 1);
                }
                tagname = "<tml:include";
                additionalAtts.add(" ref=\"" + refName + "\"");
                if (designdbName != null) {
                    additionalAtts.add(" designdb=\"" + designdbName + "\"");
                }
            }
            
            // Range shortcuts with tag
            else if (localName.startsWith("{") && localName.endsWith("}")) {
                String tagName = localName.substring(1, localName.length() - 1);
                tagname = "<tml:range";
                additionalAtts.add(" wraphtmltag=\"" + tagName + "\"");
            }

            // Write tagname and additional attributes
            out.write(tagname);
            out.write(" sourceline=\"");
            out.write(String.valueOf(lineNumber));
            out.write("\"");
            
            for (String att : additionalAtts) {
                out.write(att);
            }

            return tagnameEndPos;

        }

        /**
         * @return
         */
        public int getLineNumber() {
            return lineNumber;
        }

        /**
         * @param i
         */
        public void setLineNumber(int i) {
            lineNumber = i;
        }

    }

    public class TMLCloseTagsPreProcessor implements ReplaceProcessor {

        public int replace(String text, int from, int to, Writer out) throws IOException {

            // Find end position of tagname
            int tagnameEndPos = text.indexOf(">", from);

            // if no end found (maybe syntax error) just put the source string
            // out
            if (tagnameEndPos == -1) {
                out.write("</tml:");
                return to;
            }

            // Look if we have the shortened tml:include syntax
            String tagname = text.substring(from, tagnameEndPos);
            String localName = tagname.substring(6);
            String refName = null;
            if (localName.startsWith("[") && localName.endsWith("]")) {
                tagname = "</tml:include";
            }
            else if (localName.startsWith("{") && localName.endsWith("}")) {
                tagname = "</tml:range";
            }

            // Write tagname and additional attributes
            out.write(tagname);
            return tagnameEndPos;
        }

    }

    class PPTagsPreProcessor implements ReplaceProcessor {

        private WGDatabase _db;
        private WGACore _core;
        private WGTMLModule _mod;
        private boolean _enabled = true;

        public PPTagsPreProcessor(WGACore core, WGTMLModule mod) {
            _core = core;
            _db = mod.getDatabase();
            _mod = mod;
        }

        /**
         * @see de.innovationgate.utils.ReplaceProcessor#replace(String, int, int, Writer)
         */
        public int replace(String text, int from, int to, Writer out) throws IOException {

            // First get all data
            int endPos = text.indexOf("%}", to);
            if (endPos == -1) {
                out.write(text);
                return text.length();
            }

            // If disabled, simply put code out
            if (_enabled == false) {
                out.write(text.substring(from, to));
                return to;
            }

            String call = text.substring(from + 2, endPos);
            int nameDelimiterPos = call.indexOf(":");
            String name = null;
            List<Object> params = new ArrayList<Object>();
            if (nameDelimiterPos != -1) {
                name = call.substring(0, nameDelimiterPos).trim().toLowerCase();
                params.addAll(WGUtils.deserializeCollection(call.substring(nameDelimiterPos + 1), ";"));
            }
            else {
                name = call;
            }

            // Preprocessor commands
            if (name.equals("disablePP")) {
                _enabled = false;
            }
            else if (name.equals("label")) {
                out.write("<tml:label key=\"" + params.get(0) + "\"/>");
            }
            else{
                out.write(text.substring(from, to));
                return to;            	
            }

            return endPos + 2;

        }

    }

    class PPPreProcessor implements ReplaceProcessor {

        private WGDatabase _db;
        private WGACore _core;
        private WGTMLModule _mod;

        public PPPreProcessor(WGACore core, WGTMLModule mod) {
            _core = core;
            _db = mod.getDatabase();
            _mod = mod;
        }

        /**
         * @see de.innovationgate.utils.ReplaceProcessor#replace(String, int, int, Writer)
         */
        public int replace(String text, int from, int to, Writer out) throws IOException {

            // First get all data
            int endPos = text.indexOf("}", to);
            if (endPos == -1) {
                out.write(text);
                return text.length();
            }

            String call = text.substring(from + 2, endPos).trim();
            int nameDelimiterPos = call.indexOf(" ");
            String name = null;
            List<String> params = new ArrayList<String>();
            if (nameDelimiterPos != -1) {
                name = call.substring(0, nameDelimiterPos).trim();
                params.addAll(WGUtils.deserializeCollection(call.substring(nameDelimiterPos + 1), " "));
            }
            else {
                name = call;
            }

            // Preprocessor commands
            if(name.startsWith(":")){
            	name=name.substring(1);
	            if (name.equals("label")) {
	                out.write("<tml:label key=\"" + params.get(0) + "\"/>");
	            }
	            else if (name.equals("each")) {
	                out.write("<tml:foreach item=\"" + params.get(0) + "\"");
	                params.remove(0);
	            	for(String p: params){
	            		out.write(" " + p);
	            	}
	            	out.write(">");
	            }
	            else if (name.equals("url")) {
	                out.write("<tml:url");
	            	for(String p: params){
	            		out.write(" " + p);
	            	}
	            	out.write("/>");
	            }
	            else if (name.equals("include")) {
	                out.write("<tml:include ref=\"" + params.get(0) + "\">");
	            }
	            else if (name.equals("context")) {
	                out.write("<tml:range context=\"" + params.get(0) + "\">");
	            }
	            else if (name.equals("encode")) {
	                out.write("<tml:range encode=\"" + params.get(0) + "\">");
	            }
	        }
            else if (name.equals("/each")) {
                out.write("</tml:foreach>");
            }
            else if (name.equals("/include")) {
                out.write("</tml:include>");
            }
            else if (name.equals("/encode") || name.equals("/context")) {
                out.write("</tml:range>");
            }
            else if (name.equals("import")) {
                String refStr = (String)params.get(0);
                try {
                	Design design = WGA.get(_core).design(_db).resolve(refStr);
                	WGTMLModule mod = design.getTMLModule("html");
                	if(mod!=null && mod.isPreprocess()){
                        PPPreProcessor preProcessor = new PPPreProcessor(_core, mod);
                        String code = WGUtils.strReplace(mod.getCode(), "@{", preProcessor, true);
                		out.write(code);
                	}
                	else LOG.error("unable to <@import " + refStr);
				} catch (WGException e) {
					LOG.error("unable to <@import " + refStr);
				}
                
            }
            else{
            	// interpret as item or meta
            	if(name.equals(name.toUpperCase()))
            		out.write("<tml:meta name=\"" + name + "\"");
            	else out.write("<tml:item name=\"" + name + "\"");
            	for(String p: params){
            		out.write(" " + p);
            	}
            	out.write("/>");
            }

            //LOG.info("preprocessed: " + name + ", params: " + params.toString());
            
            return endPos + 1;

        }

    }

    
    public static final String FOLDER_DYNAMIC_RESOURCES = "dynamic";

    private WGACore _core;
    
    private LockManager _lockManager = new GenericLockManager(1, new Log4jLogger(Logger.getLogger("wga.deployer.locking")));

    protected static final Logger LOG = Logger.getLogger("wga.deployer");

    public static final String REQATTRIB_TML_DEPLOYED = WGPDispatcher.class.getName() + ".tmlDeployed";

    private Map<String, DeployedLayout> _layoutMappings = new ConcurrentHashMap<String, DeployedLayout>();

    private Map<String, DeployedLayout> _layoutsByFileName = new ConcurrentHashMap<String, DeployedLayout>();

    private File _resourcesFolder;

    public WGPDeployer(WGACore core) {
        _core = core;
    }

    public void shutdown() {

        if (_resourcesFolder.exists()) {
            LOG.info("Deleting deployed TML modules");
            WGUtils.delTree(_resourcesFolder);
            _resourcesFolder = null;
        }

        _core.removeEventListener(this);
    }

    private synchronized DeployedLayout deployTML(WGTMLModule tmlLib) throws DeployerException {
    	
    	if (tmlLib == null) {
            throw new DeployerException("Tried to deploy tml module \"null\". Maybe a tml module is not accessible");
        }
        
        DesignReference ref;
        try {
            ref = WGADesignManager.createDesignReference(tmlLib);
        }
        catch (WGAPIException e1) {
            throw new DeployerException("Error retrieving design reference for "  + tmlLib.getDocumentKey(), e1);
        }
        String databaseKey = (String) tmlLib.getDatabase().getDbReference();
        String layoutKey = ref.toString();
        String mediaKey = null;
        String resourceName = null;
        
        try {
            
            if (tmlLib.isVariant()) {
                layoutKey = layoutKey + "//" + databaseKey;
            }
        
            mediaKey = tmlLib.getMediaKey().toLowerCase();
            if (_core.isMediaKeyDefined(mediaKey) == false) {
                _core.addMediaMapping(new MediaKey(mediaKey, "text/html", false, false), false);
            }
            String preprocessedCode = preprocessCode(tmlLib, tmlLib.getCode(), tmlLib.getCodeOffset());
            String completeCode = buildTMLCode(tmlLib, tmlLib.getName() + " (" + tmlLib.getMediaKey() + ")", layoutKey, preprocessedCode);

            // deploy
            resourceName = tmlLib.getName().toLowerCase();
            LOG.info("Deploying tml " + mediaKey + "/" + resourceName + " (" + ref.getDesignProviderReference().toString() + ")");
            return mapAndDeployLayout(tmlLib, layoutKey, completeCode);
        }
        catch (Exception e) {
            throw new DeployerException("Error deploying tml " + databaseKey + "/" + resourceName + " (" + mediaKey + ")", e);
        }
    }
    
    public synchronized DeployedLayout deployInlineTML(WGTMLModule tmlLib, String inlineName, String code, int codeOffset, ServletRequest req) throws DeployerException {

        if (tmlLib == null) {
            throw new DeployerException("Tried to deploy tml module \"null\". Maybe a tml module is not accessible");
        }
        
        DesignReference ref;
        try {
            ref = WGADesignManager.createDesignReference(tmlLib);
        }
        catch (WGAPIException e1) {
            throw new DeployerException("Error retrieving design reference for "  + tmlLib.getDocumentKey(), e1);
        }
        
        String databaseKey = (String) tmlLib.getDatabase().getDbReference();
        String resourceName = null;
        String mediaKey = null;
        
        try {
            String codeHash = String.valueOf(code.hashCode());
            String layoutKey = ref.toString() + "///" + codeHash;
            
            if (tmlLib.isVariant()) {
                layoutKey = layoutKey + "//" + databaseKey;
            }
            
            DeployedLayout existingLayout = _layoutMappings.get(layoutKey);
            if (existingLayout != null) {
                return existingLayout;
            }
            
            req.setAttribute(REQATTRIB_TML_DEPLOYED, true);
                    
            mediaKey = tmlLib.getMediaKey().toLowerCase();
            if (_core.isMediaKeyDefined(mediaKey) == false) {
                _core.addMediaMapping(new MediaKey(mediaKey, "text/html", false, false), false);
            }

            String completeCode = buildTMLCode(tmlLib, "Inline " + inlineName + " of " + tmlLib.getName() + " (" + tmlLib.getMediaKey() + ")", layoutKey, code);

            resourceName = tmlLib.getName().toLowerCase();
            LOG.info("Deploying inline " + inlineName + " of tml " + mediaKey + "/" + resourceName + " (" + ref.getDesignProviderReference().toString() + ")");
            return mapAndDeployLayout(tmlLib, layoutKey, completeCode);
        }
        catch (Exception e) {
            throw new DeployerException("Error deploying tml " + databaseKey + "/" + resourceName + " (" + mediaKey + ")", e);
        }
        
    }

    private DeployedLayout mapAndDeployLayout(WGTMLModule tmlLib, String layoutKey, String completeCode) throws WGAPIException, NoSuchAlgorithmException, IOException {
        
        DeployedLayout layout = _layoutMappings.get(layoutKey);
        if (layout != null) {
            layout.init(tmlLib, layoutKey, _resourcesFolder, _core.getCharacterEncoding());
        }
        else {
            layout = new DeployedLayout(tmlLib, layoutKey, _resourcesFolder, _core.getCharacterEncoding());
        }

        // deploy
        layout.deploy(completeCode);

        // Map deployed layout
        _layoutMappings.put(layoutKey, layout);
        _layoutsByFileName.put(layout.getFile().getName(), layout);
        _lastDeployments.put(tmlLib.getDatabase().getDbReference(), new Date());
        return layout;
    }

    private String buildTMLCode(WGTMLModule tmlLib, String resourceName, String layoutKey, String libCode) throws WGAPIException {
        // Build complete code
        StringBuffer tmlCode = new StringBuffer();
        tmlCode.append("<%@ taglib uri=\"http://www.innovationgate.de/wgpublisher/webtml/2.2\" prefix=\"tml\" %>");
        tmlCode.append("<%@ page ");

        // F000037B2
        if (_core.getCharacterEncoding() != null) {
            tmlCode.append(" pageEncoding=\"" + _core.getCharacterEncoding() + "\" ");
        }
        tmlCode.append(" buffer=\"" + _core.tmlBuffer + "kb\" autoFlush=\"true\" isThreadSafe=\"true\" session=\"true\" errorPage=\"../error.jsp\" %>");
        tmlCode.append("<%@ page import=\"de.innovationgate.wgpublisher.jsputils.*\"%>");
        tmlCode.append(_core.tmlHeader);
        
        tmlCode.append("<tml:root ref=\"" + layoutKey + "\" resource=\"" + WGUtils.strReplace(resourceName, "\"", "\\\"", true) + "\" modulename=\"" + tmlLib.getName() + "\" modulemediakey=\""
                + tmlLib.getMediaKey() + "\">");
        
        tmlCode.append(libCode);
        tmlCode.append("</tml:root>");
        String completeCode = tmlCode.toString();
        return completeCode;
    }

    public String filenameToTMLModuleName(String filename) {
        DeployedLayout layout = _layoutsByFileName.get(filename);
        if (layout != null) {
            return layout.getMainLibKey();
        }
        else {
            return null;
        }
    }

    public String locateTmlResource(WGTMLModule tmlLib, ServletRequest servletRequest) throws WGAPIException, DeployerException {

        DesignReference ref = WGADesignManager.createDesignReference(tmlLib);
                
        DeployedLayout layout = getDeployedLayout(tmlLib, ref);
        if (layout == null) {
            MultiLevelLock lock = _lockManager.atomicGetOrCreateLock(ref.toString());
            try {
                try {
                    lock.acquire(Thread.currentThread(), 1,  true, true, Long.MAX_VALUE);
                }
                catch (InterruptedException e) {
                }
                
                layout = getDeployedLayout(tmlLib, ref);
                if (layout == null) {
                    servletRequest.setAttribute(REQATTRIB_TML_DEPLOYED, true);
                    layout = deployTML(tmlLib);
                }
            }
            finally {
                lock.release(Thread.currentThread());
            }
        }

        return layout.getResourcePath();

    }

    private DeployedLayout getDeployedLayout(WGTMLModule mod, DesignReference ref) throws WGAPIException {
        
        String layoutKey = ref.toString();
        
        // On design variants we must append the database key to the layout key, since different database may use different data
        if (mod.isVariant()) {
            layoutKey = layoutKey + "//" + mod.getDatabase().getDbReference();
        }
        
        DeployedLayout layout = this._layoutMappings.get(layoutKey);
        if (layout == null) {
            return null;
        }

        if (layout.needsUpdate(mod)) {
            return null;
        }
        else {
            return layout;
        }
    }

    /**
     * @see WGDatabaseEventListener#isTemporary()
     */
    public boolean isTemporary() {
        return false;
    }

    public Map<String, DeployedLayout> getLayoutMappings() {
        return this._layoutMappings;
    }

    public void removeLayouts(WGDatabase db) {

        String key = (String) db.getAttribute(WGACore.DBATTRIB_DBKEY);
        Set<String> layoutsSet = this._layoutMappings.keySet();
        synchronized (this._layoutMappings) {
            Iterator<String> layouts = layoutsSet.iterator();
            while (layouts.hasNext()) {
                String layoutKey = layouts.next();
                if (layoutKey.startsWith(key + "/")) {
                    DeployedLayout layout = _layoutMappings.get(layoutKey);
                    if (layout != null) {
                        _layoutsByFileName.remove(layout.getFile().getName());
                    }
                    layouts.remove();
                }
            }
        }

    }

    public String preprocessCode(WGTMLModule mod, String code, int codeOffset) throws WGAPIException {

        // First pass. Process preprocessor tags
        try {
            PPTagsPreProcessor preProcessor = new PPTagsPreProcessor(_core, mod);
            code = WGUtils.strReplace(code, "{%", preProcessor, true);
        }
        catch (RuntimeException e) {
            LOG.error("Error preprocessing WebTML module " + mod.getDocumentKey(), e);
        }

        // Process <@ preprocessor if enabled
        if(mod.isPreprocess()){
	        try {
	        	//code = code.replaceAll("@@([\\w|\\$]+)", "<tml:item name=\"$1\"/>");
	            PPPreProcessor pppreProcessor = new PPPreProcessor(_core, mod);
	            code = WGUtils.strReplace(code, "@{", pppreProcessor, true);
	        }
	        catch (RuntimeException e) {
	            LOG.error("Error preprocessing WebTML module " + mod.getDocumentKey(), e);
	        }
        }
        
        // Second pass. Process WebTML tags
        LineNumberReader reader = new LineNumberReader(new StringReader(code));
        StringBuffer out = new StringBuffer();
        TMLTagsPreProcessor linePreProcessor = new TMLTagsPreProcessor();
        String line;
        boolean firstLine = true;
        try {
            while ((line = reader.readLine()) != null) {
                if (!firstLine) {
                    out.append("\n");
                }
                else {
                    firstLine = false;
                }
                linePreProcessor.setLineNumber(codeOffset + reader.getLineNumber());
                out.append(WGUtils.strReplace(line, "<tml:", linePreProcessor, true));
            }
            code = out.toString();
        }
        catch (IOException e) {
            LOG.error("Error adding line numbers to WebTML code", e);
        }

        // Third pass. Process WebTML close tags
        code = WGUtils.strReplace(code, "</tml:", new TMLCloseTagsPreProcessor(), true);

        return code;
    }

    public void startup() {

        // Create folder for dynamic resource
        _resourcesFolder = new File(_core.getServletContext().getRealPath("/"), FOLDER_DYNAMIC_RESOURCES);
        if (_resourcesFolder.exists()) {
            _core.getLog().info("Deleting deployed tmls from previous instance");
            WGUtils.delTree(_resourcesFolder);
        }
        _resourcesFolder.mkdir();

        // Register for core events
        _core.addEventListener(this);

    }

    private Map<String,Date> _lastDeployments = new HashMap<String,Date>();

    public Date getLastChangedOrDeployed(WGDatabase db) {

        String designDBKey = _core.getDesignDatabaseKey(db);

        Date lastDeployed = getLastDeployed(designDBKey);
        Date lastChanged = db.getLastChanged();
        if (lastDeployed == null || (lastChanged!=null && lastChanged.after(lastDeployed))) {
            return lastChanged;
        }
        else {
            return lastDeployed;
        }

    }

    public Date getLastDeployed(String dbKey) {
        return _lastDeployments.get(dbKey);
    }

    /**
     * 
     * @param domainConfig
     * @return
     * @throws IOException
     */
    public String deployErrorPage(String code) throws IOException {

        File errJsp = File.createTempFile("err", ".jsp", _resourcesFolder);
        FileWriter out = new FileWriter(errJsp);
        out.write(code);
        out.flush();
        out.close();

        return "/" + _resourcesFolder.getName() + "/" + errJsp.getName();

    }

    public void contentStoreConnected(WGACoreEvent event) {
    }

    public void contentStoreDisconnected(WGACoreEvent event) {

        WGDatabase db = event.getDatabase();
        String dbkey = (String) db.getAttribute(WGACore.DBATTRIB_DBKEY);
        _lastDeployments.remove(dbkey);

    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {
    }

    public void startupPreConnect(WGACoreEvent event) {
    }


    


}
