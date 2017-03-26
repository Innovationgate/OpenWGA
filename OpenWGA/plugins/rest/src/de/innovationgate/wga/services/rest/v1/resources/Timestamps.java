package de.innovationgate.wga.services.rest.v1.resources;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlVariableNode;

@XmlRootElement
public class Timestamps {
    
    public Date created;
    public Date lastModified;
    public Date published;
    
    @XmlElementWrapper(name="published")
    @XmlVariableNode("language")
    public List<PagePublishedDate> pagePublished;
    
    public void addPagePublishedDates(Map<String,Date> dates) {
        
        this.pagePublished = new ArrayList<PagePublishedDate>();
        for (Map.Entry<String,Date> date : dates.entrySet()) {
            this.pagePublished.add(new PagePublishedDate(date.getKey(), date.getValue()));
        }
        
        
    }

}
