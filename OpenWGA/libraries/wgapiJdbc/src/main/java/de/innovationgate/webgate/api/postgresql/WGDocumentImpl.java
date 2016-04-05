package de.innovationgate.webgate.api.postgresql;

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.jdbc.MainEntity;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;

public class WGDocumentImpl extends de.innovationgate.webgate.api.jdbc.WGDocumentImpl {

    protected WGDocumentImpl(WGDatabaseImpl parent, MainEntity entity, int type) {
        super(parent, entity, type);
    }
    
    @Override
    public boolean setItemValue(String strName, Object value) throws WGIllegalArgumentException, WGSystemException {
        
        try {
            return super.setItemValue(strName, stripNullBytes(value));
        }
        catch (WGIllegalArgumentException e) {
            throw e;
        }
        catch (WGSystemException e) {
            throw e;
        }
        catch (WGAPIException e) {
            throw new WGSystemException(e.getMessage(), e);
        }
    }
    
    @Override
    public boolean setMetaData(String name, Object value) throws WGAPIException {

        return super.setMetaData(name, stripNullBytes(value));
        
    }
    
    public Object stripNullBytes(Object value) {
        
        if (value instanceof String) {
            return ((String) value).replace('\0', ' ');
        }
        else if (value instanceof List) {
            List values = (List) value;
            List newValues = new ArrayList();
            for (Object singleValue : values) {
                newValues.add(stripNullBytes(singleValue));
            }
            return newValues;
        }
        else {
            return value;
        }
        
    }

}
