package de.innovationgate.wga.services.rest.v1.types;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.wga.services.rest.v1.resources.Resource;

@XmlRootElement
public class DocumentKey {
    
    @XmlElement
    public String type;
    
    @XmlElement
    public String name;
    
    @XmlElement
    public String pageKey;
    
    @XmlElement
    public String hdbmodelContentId;
    
    @XmlElement
    public Integer version;
    
    @XmlElement
    public String language;
    
    public DocumentKey() {
    }
    
    public DocumentKey(Resource<?> res, WGDatabase db, WGDocumentKey key) throws WGAPIException {
        
        this.type = res.getResourceType();
        switch (key.getDocType()) {
            
            case WGDocument.TYPE_AREA:
                this.name = key.getName();
                break;
                
                
            case WGDocument.TYPE_STRUCTENTRY:
                this.pageKey = key.getName();
                break;
                
            case WGDocument.TYPE_CONTENT:
                WGContentKey contentKey = WGContentKey.parse(key.getName(), db);
                this.pageKey = String.valueOf(contentKey.getStructKey());
                this.version = contentKey.getVersion();
                this.language = contentKey.getLanguage();
                break;
                
                
            default:
                 this.name = key.getName();
            
        }
        
        
    }

}
