@XmlJavaTypeAdapters({
    @XmlJavaTypeAdapter(type=java.util.Date.class, 
        value=de.innovationgate.wga.services.rest.v1.types.DateAdapter.class)
})
package de.innovationgate.wga.services.rest.v1.resources.cms;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;

