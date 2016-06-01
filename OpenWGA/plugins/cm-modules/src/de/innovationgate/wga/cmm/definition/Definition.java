package de.innovationgate.wga.cmm.definition;

import org.simpleframework.xml.Attribute;

import de.innovationgate.wga.config.NotNull;

public class Definition {
    
    private String configtml;
    private String dbkey;

    public void setConfigtml(String tml) {
        this.configtml = tml;
    }

    public String getConfigtml() {
        return configtml;
    }

    public String getDbkey() {
        return dbkey;
    }

    public void setDbkey(String dbKey) {
        this.dbkey = dbKey;
    }

}
