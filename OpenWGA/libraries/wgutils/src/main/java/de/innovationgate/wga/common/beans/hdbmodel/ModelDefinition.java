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

package de.innovationgate.wga.common.beans.hdbmodel;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.jxpath.JXPathContext;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;
import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.Version;
import org.simpleframework.xml.core.Commit;
import org.simpleframework.xml.core.Persister;
import org.simpleframework.xml.util.Dictionary;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import de.innovationgate.utils.WGUtils;

/**
 * The base object of HDBModel hierarchy definition
 */
@Root(name="hdb-model",strict=false)
public class ModelDefinition implements DocumentParent, CustomParamsCarrier {
    
    private static Persister _serializer = new Persister();
    
    private transient Map<Document,DocumentParent> _childToParent = new HashMap<Document, DocumentParent>();
    
    public static ModelDefinition read(InputStream in) throws Exception {
        
        // First read XML manually and validate against the DTD provided in this OpenWGA distribution (to prevent it being loaded from the internet, which might not work, see #00003612) 
        SAXReader reader = new SAXReader();
        reader.setIncludeExternalDTDDeclarations(true);
        reader.setIncludeInternalDTDDeclarations(true);
        reader.setEntityResolver(new EntityResolver() {

            @Override
            public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {

                if (systemId.equals("http://doc.openwga.com/hdb-model-definition-6.0.dtd")
                		|| systemId.equals("https://doc.openwga.com/hdb-model-definition-6.0.dtd")
                ) {
                    return new InputSource(ModelDefinition.class.getClassLoader().getResourceAsStream(WGUtils.getPackagePath(ModelDefinition.class) + "/hdb-model-definition-6.0.dtd"));
                } else {
                    // use the default behaviour
                    return null;
                }
                
            }
            
        });
        
        org.dom4j.Document domDoc =reader.read(in);
        
        // Remove doctype (we already have validated) and provide the resulting XML to the serializer
        domDoc.setDocType(null);
        StringWriter out = new StringWriter();
        XMLWriter writer = new XMLWriter(out);
        writer.write(domDoc);
        String xml = out.toString();
        
        
        return _serializer.read(ModelDefinition.class, new StringReader(xml));
    }
    
    @Version(name="version",revision=1.0)
    private double _version;
    
    @Attribute(name="definition",required=false)
    private String _definition;
    
    @Attribute(name="scripts", required=false)
    private String _scripts;
    
    @Attribute(name="versioning", required=false)
    private String _versioning;
    
    @ElementList(inline=true,required=false)
    private Dictionary<CustomParam> _customParams = new Dictionary<CustomParam>();

    
    public static Persister getSerializer() {
        return _serializer;
    }

    public static void setSerializer(Persister serializer) {
        _serializer = serializer;
    }

    public String getDefinition() {
        return _definition;
    }

    public void setDefinition(String redirect) {
        _definition = redirect;
    }

    public void setRootStorages(List<Storage> rootStorages) {
        _rootStorages = rootStorages;
    }

    @ElementList(inline=true,required=false)
    private List<Storage> _rootStorages = new ArrayList<Storage>();

    public List<Storage> getRootStorages() {
        return _rootStorages;
    }

    public List<Document> getChildDocuments() {
        List<Document> docs = new ArrayList<Document>();
        docs.addAll(getRootStorages());
        return docs;
    }
    
    @Commit
    public void commit(Map session) {
        _childToParent.putAll(session);
        
        for (Storage storage : getRootStorages()) {
            _childToParent.put(storage, this);
        }
    }
    
    public DocumentParent getDocumentParent(Document doc) {
        return _childToParent.get(doc);
    }

    public double getVersion() {
        return _version;
    }

    public void setVersion(double version) {
        _version = version;
    }

    public String getScripts() {
        return _scripts;
    }

    public void setScripts(String scripts) {
        _scripts = scripts;
    }

    public String getVersioning() {
        return _versioning;
    }

    public void setVersioning(String versioning) {
        _versioning = versioning;
    }

    public Dictionary<CustomParam> getCustomParams() {
        return _customParams;
    }

    public void setCustomParams(Dictionary<CustomParam> customParams) {
        _customParams = customParams;
    }
    
    public void accept(DocumentVisitor visitor) {
        for (Document doc : getChildDocuments()) {
            doc.accept(visitor);
        }
    }

 

}
