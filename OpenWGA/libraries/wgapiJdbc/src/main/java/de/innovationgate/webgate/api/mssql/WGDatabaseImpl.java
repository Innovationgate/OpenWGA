/*
 * Created on Jan 10, 2006 from ow
 *
 */
package de.innovationgate.webgate.api.mssql;


import java.util.Map;

import org.hibernate.dialect.SQLServerDialect;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGUserAccess;

public class WGDatabaseImpl extends de.innovationgate.webgate.api.jdbc.WGDatabaseImpl {

    public static final String SOCKET_TIMEOUT_DEFAULT = "120000";
    public static final String CONNECT_TIMEOUT_DEFAULT = "60000";


    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {
        
        // Build creations options
        Map creationOptions = db.getCreationOptions();
        
        // Hibernate configuration
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dialect", SQLServerDialect.class.getName());
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.validationQuery", "select 1");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.testOnBorrow", "true");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.dbcp.maxWait", "30000");
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.connectTimeout", CONNECT_TIMEOUT_DEFAULT);
        WGDatabase.putDefaultOption(creationOptions, "hibernate.connection.socketTimeout", SOCKET_TIMEOUT_DEFAULT);
        
        // Recommended to disable query paging on MSSQL
        // WGDatabase.putDefaultOption(creationOptions, WGDatabase.COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING, "true");
        
        return super.open(db, path, user, pwd, prepareOnly);
    }
    
    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
     */
    public String getTypeName() {
        return "jdbc/wgacontentstore/mssql";
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGDatabaseImpl#hasFeature(java.lang.String)
     */
    public boolean hasFeature(String feature) {

        if (feature.equals(WGDatabase.FEATURE_CREATEABLE)) {
            return false;
        }

        if (WGDatabase.FEATURE_MILLISECOND_PRECISION.equals(feature)) {
            return true;
        }

        return super.hasFeature(feature);
    }
    
}
