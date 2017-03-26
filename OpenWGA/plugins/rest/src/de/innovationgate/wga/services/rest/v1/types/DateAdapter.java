package de.innovationgate.wga.services.rest.v1.types;

import java.text.DateFormat;
import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import de.innovationgate.utils.ISO8601DateFormat;

public class DateAdapter extends XmlAdapter<String, Date> {
    
    public static final DateFormat FORMAT = new ISO8601DateFormat();

    @Override
    public Date unmarshal(String v) throws Exception {
        if ("null".equals(v)) {
           return null;
        }
        else {
            return FORMAT.parse(v);
        }
    }

    @Override
    public String marshal(Date v) throws Exception {
        if (v == null) {
            return null;
        }
        else {
            return FORMAT.format(v);
        }
    }

}
