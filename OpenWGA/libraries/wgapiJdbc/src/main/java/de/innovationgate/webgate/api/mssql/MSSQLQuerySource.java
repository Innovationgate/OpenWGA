/*
 * Created on 12.06.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.mssql;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.query.jdbc.WGDatabaseImpl;

public class MSSQLQuerySource extends WGDatabaseImpl {

    @Override
    protected String getJDBCDriver() {
        return de.innovationgate.webgate.api.mssql.WGDatabaseImpl.DRIVER;
    }

    @Override
    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGInvalidDatabaseException, WGBackendException {
        return super.open(db, path, user, pwd, prepareOnly);
    }

}
