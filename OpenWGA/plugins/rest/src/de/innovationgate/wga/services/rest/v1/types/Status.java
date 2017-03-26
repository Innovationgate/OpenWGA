package de.innovationgate.wga.services.rest.v1.types;

public enum Status {

    SUCCESS("SUCCESS"), ERROR("ERROR");
    
    
    private String _str;
    Status(String str) {
        _str = str;
    }
    
    @Override
    public String toString() {
        return _str;
    }
    
}
