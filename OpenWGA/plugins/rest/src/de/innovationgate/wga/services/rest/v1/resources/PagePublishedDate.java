package de.innovationgate.wga.services.rest.v1.resources;

import java.util.Date;

import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlValue;

public class PagePublishedDate {
    
    public PagePublishedDate() {
    }
    
    public PagePublishedDate(String langParam, Date dateParam) {
        this.language = langParam;
        this.date = dateParam;
    }

    @XmlTransient
    public String language;
    
    @XmlValue
    public Date date;
}