package de.innovationgate.webgate.api.mariadb;

import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;

public class MariaDbDatabaseServer extends MySqlDatabaseServer{

	public static final String JDBC_BASE_PATH = "jdbc:mariadb://";

	@Override
    public String getJdbcBasePath() {
    	return JDBC_BASE_PATH;
    }

}
