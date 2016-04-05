/*
 * Created on 16.06.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.mssql;

import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCSource;
import de.innovationgate.webgate.api.templates.ContentSourceSpecs;

public class MSSQLEnhancedQuerySource extends JDBCSource {

    @Override
    protected String getJDBCDriver(WGDatabase db) {
        return de.innovationgate.webgate.api.mssql.WGDatabaseImpl.DRIVER;
    }

    @Override
    public ContentSourceSpecs init(WGDatabase db, String path) throws WGInvalidDatabaseException {
        
        
        // Build creations options
        Map creationOptions = db.getCreationOptions();
        
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.validationQuery", "select 1");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.validationQueryTimeout", "5");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.testOnBorrow", "true");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.maxWait", "30000");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.connection.connectTimeout", WGDatabaseImpl.CONNECT_TIMEOUT_DEFAULT);
        WGDatabase.putDefaultOption(creationOptions, "jdbc.connection.socketTimeout", WGDatabaseImpl.SOCKET_TIMEOUT_DEFAULT);

        
        return super.init(db, path);
    }

}
