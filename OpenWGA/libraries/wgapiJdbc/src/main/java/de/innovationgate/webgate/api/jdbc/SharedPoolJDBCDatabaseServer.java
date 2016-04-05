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

package de.innovationgate.webgate.api.jdbc;

import org.hibernate.SessionBuilder;
import org.hibernate.SessionFactory;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.jdbc.connections.spi.MultiTenantConnectionProvider;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl.CSVersion;
import de.innovationgate.wga.modules.options.OptionConversionException;

/**
 * An instance for {@link JDBCDatabaseServer} which offer a shared connection pool/session factory for the current content stores version
 */
public interface SharedPoolJDBCDatabaseServer {
    
    /**
     * Determines if a pool is available for the given content store version
     * @param csVersion the content Store version
     */
    public boolean isPoolAvailable(CSVersion csVersion);
    
    /**
     * Creates a connection provider for the given catalog name, served from the shared connection pool
     * @param catalogName The catalog name
     * @throws OptionConversionException 
     * @throws WGInvalidDatabaseException 
     */
    public ConnectionProvider createPoolConnectionProvider(String catalogName) throws WGAPIException, OptionConversionException;

}
