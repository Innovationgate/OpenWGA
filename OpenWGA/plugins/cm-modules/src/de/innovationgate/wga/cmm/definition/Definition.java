package de.innovationgate.wga.cmm.definition;

public class Definition {
    
    private String _configtml;
    private String _dbkey;

    // default constructor needed by JSONArray
    public Definition(){}
    
    public Definition(String dbkey, String tml){
    	_dbkey = dbkey;
    	_configtml = tml;
    }
    
    public void setConfigtml(String tml) {
        _configtml = tml;
    }

    public String getConfigtml() {
        return _configtml;
    }

    public String getDbkey() {
        return _dbkey;
    }

    public void setDbkey(String dbKey) {
        _dbkey = dbKey;
    }

}
