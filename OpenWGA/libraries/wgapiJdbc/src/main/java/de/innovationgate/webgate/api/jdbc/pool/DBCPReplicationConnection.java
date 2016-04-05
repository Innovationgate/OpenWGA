/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.webgate.api.jdbc.pool;

import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.PreparedStatement;
import java.sql.SQLClientInfoException;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Struct;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.Executor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class DBCPReplicationConnection implements Connection {
	
	private Connection _masterConnection;
	private Connection _slaveConnection;
	private Connection _currentConnection;
	
	private static final Log log = LogFactory.getLog("wga.dbcppool");
	
	// flag to determine if this wrapper connection is readonly
	// this information is redundant bc. the underlying connection is set readonly
	// if some drivers cannot handle the readonly flag it is saver to deny commits
	// on readonly connections directly in this wrapper
	boolean _readonly = true;

	public DBCPReplicationConnection(Connection master, Connection slave) {
		_masterConnection = master;
		_slaveConnection = slave;
		
		if (_masterConnection != null) {
			try {
				_masterConnection.setReadOnly(true);
				_masterConnection.setAutoCommit(false);
			} catch (SQLException e) {
				log.warn("Unable to set inital readonly flag on masterConnection.", e);
			}
		}
		
		if (_slaveConnection != null) {			
			try {
				_slaveConnection.setReadOnly(true);
				_slaveConnection.setAutoCommit(false);
			} catch (SQLException e) {
				log.warn("Unable to set initial readonly flag on slaveConnection.", e);
			}
		}
		
		if (_masterConnection != null && _slaveConnection != null) {
			// randomize currentConnection
			if (new Random().nextBoolean()) {
				_currentConnection = _masterConnection;
			} else {
				_currentConnection = _slaveConnection;
			}
		} else if (_masterConnection != null) {
			_currentConnection = _masterConnection;
		} else {
			_currentConnection = _slaveConnection;
		}
	}
	
	public void clearWarnings() throws SQLException {
		_currentConnection.clearWarnings();
	}
	
	public void close() throws SQLException {
		if (_masterConnection != null) {
			_masterConnection.close();
		}
		if (_slaveConnection != null) {
			_slaveConnection.close();
		}
	}
	
	public void commit() throws SQLException {
		if (_readonly) {
			throw new SQLException("Datamodification is not allowed on a readonly connection.");
		} else {
			_currentConnection.commit();
		}
	}
	
	public Statement createStatement() throws SQLException {
		return _currentConnection.createStatement();
	}
	
	public Statement createStatement(int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
		return _currentConnection.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
	}
	
	public Statement createStatement(int resultSetType, int resultSetConcurrency) throws SQLException {
		return _currentConnection.createStatement(resultSetType, resultSetConcurrency);
	}
	
	public boolean getAutoCommit() throws SQLException {
		return _currentConnection.getAutoCommit();
	}
	public String getCatalog() throws SQLException {
		return _currentConnection.getCatalog();
	}
	
	public int getHoldability() throws SQLException {
		return _currentConnection.getHoldability();
	}
	
	public DatabaseMetaData getMetaData() throws SQLException {
		return _currentConnection.getMetaData();
	}
	
	public int getTransactionIsolation() throws SQLException {
		return _currentConnection.getTransactionIsolation();
	}
	
	public Map getTypeMap() throws SQLException {
		return _currentConnection.getTypeMap();
	}
	
	public SQLWarning getWarnings() throws SQLException {
		return _currentConnection.getWarnings();
	}
	public boolean isClosed() throws SQLException {
		return _currentConnection.isClosed();
	}
	
	public boolean isReadOnly() throws SQLException {		
		return _readonly;
	}
	
	public String nativeSQL(String sql) throws SQLException {
		return _currentConnection.nativeSQL(sql);
	}
	
	public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
		return _currentConnection.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
	}
	
	public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
		return _currentConnection.prepareCall(sql, resultSetType, resultSetConcurrency);
	}
	
	public CallableStatement prepareCall(String sql) throws SQLException {
		return _currentConnection.prepareCall(sql);
	}
	
	public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
		return _currentConnection.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
	}
	
	public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
		return _currentConnection.prepareStatement(sql, resultSetType, resultSetConcurrency);
	}
	
	public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {
		return _currentConnection.prepareStatement(sql, autoGeneratedKeys);
	}
	
	public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {
		return _currentConnection.prepareStatement(sql, columnIndexes);
	}
	
	public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {
		return _currentConnection.prepareStatement(sql, columnNames);
	}
	
	public PreparedStatement prepareStatement(String sql) throws SQLException {
		return _currentConnection.prepareStatement(sql);
	}
	
	public void releaseSavepoint(Savepoint savepoint) throws SQLException {
		_currentConnection.releaseSavepoint(savepoint);
	}
	
	public void rollback() throws SQLException {
		_currentConnection.rollback();
	}
	
	public void rollback(Savepoint savepoint) throws SQLException {
		_currentConnection.rollback(savepoint);
	}
	
	/**
	 * setting autocommit will be ignored - always false
	 */
	public void setAutoCommit(boolean autoCommit) throws SQLException {
		// ignore
	}
	
	public void setCatalog(String catalog) throws SQLException {
		_currentConnection.setCatalog(catalog);
	}
	
	public void setHoldability(int holdability) throws SQLException {
		_currentConnection.setHoldability(holdability);
	}
	
	public void setReadOnly(boolean readOnly) throws SQLException {
		_readonly = readOnly;
		if (readOnly) {
			if (_slaveConnection != null && _masterConnection != null) {
				// randomize current connection between master and slave
				if (new Random().nextBoolean()) {
					_currentConnection = _masterConnection;
				} else {
					_currentConnection = _slaveConnection;
				}				
			} else if (_masterConnection != null) {
				_currentConnection = _masterConnection;
			} else {
				_currentConnection = _slaveConnection;
			}
			try {
				_currentConnection.setReadOnly(true);
			} catch (SQLException e) {
				log.warn("Unable to set readonly flag to '" + readOnly + "' on db connection.", e);
			}
		} else {
			if (_masterConnection != null) {
				// switch to master connection and make it read/write
				_currentConnection = _masterConnection;
				try {
					_currentConnection.setReadOnly(false);
				} catch (SQLException e) {
					log.warn("Unable to set readonly flag to '" + readOnly + "' on db connection.", e);
				}
			} else {
				_currentConnection = _slaveConnection;
				log.warn("No master connection avaiable. Master may be down. Connection will fall back to slave and is readonly.");
				try {
					_currentConnection.setReadOnly(true);
				} catch (SQLException e) {
					log.warn("Unable to set readonly flag to '" + readOnly + "' on db connection.", e);
				}
			}
		}
	}
	
	public Savepoint setSavepoint() throws SQLException {
		return _currentConnection.setSavepoint();
	}
	
	public Savepoint setSavepoint(String name) throws SQLException {
		return _currentConnection.setSavepoint(name);
	}
	
	public void setTransactionIsolation(int level) throws SQLException {
		_currentConnection.setTransactionIsolation(level);
	}

    @Override
    public boolean isWrapperFor(Class<?> arg0) throws SQLException {
        return (arg0.isAssignableFrom(_currentConnection.getClass()));
    }

    @Override
    public <T> T unwrap(Class<T> arg0) throws SQLException {
        
        if (arg0.isAssignableFrom(Connection.class)) {
            return (T) _currentConnection;
        }
        else {
            return null;
        }
    }

    @Override
    public Array createArrayOf(String typeName, Object[] elements) throws SQLException {
        return _currentConnection.createArrayOf(typeName, elements);
    }

    @Override
    public Blob createBlob() throws SQLException {
        return _currentConnection.createBlob();
    }

    @Override
    public Clob createClob() throws SQLException {
        return _currentConnection.createClob();
    }

    @Override
    public NClob createNClob() throws SQLException {
        return _currentConnection.createNClob();
    }

    @Override
    public SQLXML createSQLXML() throws SQLException {
        return _currentConnection.createSQLXML();
    }

    @Override
    public Struct createStruct(String typeName, Object[] attributes) throws SQLException {
        return _currentConnection.createStruct(typeName, attributes);
    }

    @Override
    public Properties getClientInfo() throws SQLException {
        return _currentConnection.getClientInfo();
    }

    @Override
    public String getClientInfo(String name) throws SQLException {
        return _currentConnection.getClientInfo(name);
    }

    @Override
    public boolean isValid(int timeout) throws SQLException {
        return _currentConnection.isValid(timeout);
    }

    @Override
    public void setClientInfo(Properties properties) throws SQLClientInfoException {
        _currentConnection.setClientInfo(properties);
    }

    @Override
    public void setClientInfo(String name, String value) throws SQLClientInfoException {
        _currentConnection.setClientInfo(name, value);
        
    }

    @Override
    public void setTypeMap(Map<String, Class<?>> map) throws SQLException {
        _currentConnection.setTypeMap(map);
    }

    @Override
    public void setSchema(String schema) throws SQLException {
        _currentConnection.setSchema(schema);
        
    }

    @Override
    public String getSchema() throws SQLException {
        return _currentConnection.getSchema();
    }

    @Override
    public void abort(Executor executor) throws SQLException {
        _currentConnection.abort(executor);        
    }

    @Override
    public void setNetworkTimeout(Executor executor, int milliseconds) throws SQLException {
        _currentConnection.setNetworkTimeout(executor, milliseconds);
    }

    @Override
    public int getNetworkTimeout() throws SQLException {
        return _currentConnection.getNetworkTimeout();
    }
}
