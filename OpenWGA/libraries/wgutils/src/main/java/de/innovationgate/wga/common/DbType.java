/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.common;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JList;

/**
 * Bean object to represent a database type shown in WGA Manager
 *
 */
public class DbType implements Comparable {
		
		/**
		 * General type representing content databases
		 */
		public static final int GENTYPE_CONTENT = 1;
		/**
		 * General type representing personalisation databases
		 */
		public static final int GENTYPE_PERSO = 2;
		/**
		 * General type representing application logs
		 */
		public static final int GENTYPE_LOG = 3;
        
        private static Map contentByClass = new HashMap();
        private static Map persByClass = new HashMap();
        private static Map logByClass = new HashMap();
        
        private static void createContentDBTypes() {
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.domino.remote.WGDatabaseImpl", "WGA Content Store for Lotus Domino", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.query.domino.remote.WGDatabaseImpl", "Lotus Domino Database", true, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.jdbc.WGDatabaseImpl", "WGA Content Store for JDBC", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.mysql.WGDatabaseImpl", "WGA Content Store for MySQL", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.firebird.WGDatabaseImpl", "WGA Content Store for FirebirdSQL", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.oracle.WGDatabaseImpl", "WGA Content Store for Oracle", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.hsql.WGDatabaseImpl", "WGA Content Store on embedded HSQL-Server", true, true);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.query.jdbc.WGDatabaseImpl", "JDBC-Database (Query only)", false, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.jdbc.custom.JDBCSource", "JDBC-Database with enhanced Access", false, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.xfiles.WGDatabaseImpl", "WGA Content Store for XML-Files", false, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.query.rss.WGDatabaseImpl", "RSS-Feed Connector (old version)", false, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.rss2.SimpleRSS", "RSS-Feed Connector", false, false);
            new DbType(DbType.GENTYPE_CONTENT, "de.innovationgate.webgate.api.simple.BeanAdapter", "Bean-Adapter", false, false);
        }
        
        private static void createPersonalisationDBTypes() {
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.pers.domino.remote.WGDatabaseImpl", "WGA Personalisation for Lotus Domino");
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.jdbc.WGDatabaseImpl", "WGA Personalisation for JDBC");
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.mysql.WGDatabaseImpl", "WGA Personalisation for MySQL");
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.firebird.WGDatabaseImpl", "WGA Personalisation for FirebirdSQL");
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.oracle.WGDatabaseImpl", "WGA Personalisation for Oracle");
            new DbType(DbType.GENTYPE_PERSO, "de.innovationgate.webgate.api.hsql.WGDatabaseImpl", "WGA Personalisation on embedded HSQL-Server");
        }
        
        private static void createLogDBTypes() {
            new DbType(DbType.GENTYPE_LOG, "de.innovationgate.wgpublisher.log.WGALogger2JDBC", "WGA Log for JDBC");
            new DbType(DbType.GENTYPE_LOG, "de.innovationgate.wgpublisher.log.WGALogger2Firebird", "WGA Log for Firebird");
            new DbType(DbType.GENTYPE_LOG, "de.innovationgate.wgpublisher.log.WGALogger2DB2", "WGA Log for DB2");
            new DbType(DbType.GENTYPE_LOG, "de.innovationgate.wgpublisher.log.WGALogger2Text", "WGA Log for Textfiles");
        }
        
        static {
            createContentDBTypes();
            createPersonalisationDBTypes();
            createLogDBTypes();
        }
		
		private int generalType;
		private String implClass;
		private String title;
		private boolean authConfigurable;
		
        private boolean fullContentStore;
        
        /**
         * Constructor
         * @param genType The general type, a constant GENTYPE_...
         * @param implClass The implementation class
         * @param title The title
         * @param authConfigurable true if the authentication on this type is configurable
         * @param fullContentStore true if the type is a full featured content store
         */
        public DbType(int genType, String implClass, String title, boolean authConfigurable, boolean fullContentStore) {
			this.generalType = genType;
			this.implClass = implClass;
			this.title = title;
			this.authConfigurable = authConfigurable;
			this.fullContentStore = fullContentStore;
            
            if (genType == GENTYPE_CONTENT) {
                contentByClass.put(implClass, this);
            }
            else if (genType == GENTYPE_PERSO) {
                persByClass.put(implClass, this);
            }
            else if (genType == GENTYPE_LOG) {
                logByClass.put(implClass, this);
            }

		}
        
        /**
         * Constructor
         * @param genType The general type, a constant GENTYPE_...
         * @param implClass The implementation class
         * @param title The title
         */
        public DbType(int genType, String implClass, String title) {
            this(genType, implClass, title, false, false);
        }
		
		/**
		 * Returns the db type that matches an implementation class
		 * @param genType The general type, a constant GENTYPE_...
		 * @param implClass The implementation class
		 */
		public static DbType getByImplClass(int genType, String implClass) {
            
            if (genType == GENTYPE_CONTENT) {
                return (DbType) contentByClass.get(implClass);
            }
            else if (genType == GENTYPE_PERSO) {
                return (DbType) persByClass.get(implClass);
            }
            else if (genType == GENTYPE_LOG) {
                return (DbType) logByClass.get(implClass);
            }
            else {
                return null;
            }
		}
        
		/**
         * Feeds a swing list with all database types of a general type
         * @param genType The general type, a constant GENTYPE_...
         * @param list The list to fill
         */
        public static void feedSwingList(int genType, JList list) {
            
            Vector types = new Vector(getAllTypes(genType));
            list.setListData(types);
            
        }
        
        /**
         * Feeds a swing combo box with all database types of a general type
         * @param genType The general type, a constant GENTYPE_...
         * @param cbo The combo box to fill
         */
        public static void feedSwingCombo(int genType, JComboBox cbo) {
            
            Vector typesList = new Vector(getAllTypes(genType));
            Collections.sort(typesList);
            Iterator types = typesList.iterator();
            while (types.hasNext()) {
                cbo.addItem(types.next());
            }
                
            
        }
        
        /**
         * Returns all registered database types for a given general type
         * @param genType The general type, a constant GENTYPE_...
         * @return A list of DbType objects for all registered database types of the general type
         */
        public static List getAllTypes(int genType) {
            if (genType == GENTYPE_CONTENT) {
                return new ArrayList(contentByClass.values());
            }
            else if (genType == GENTYPE_PERSO) {
                return new ArrayList(persByClass.values());
            }
            else if (genType == GENTYPE_LOG) {
                return new ArrayList(logByClass.values());
            }
            else {
                return null;
            }
        }
		
		/**
		 * Returns the implClass.
		 * @return String
		 */
		public String getImplClass() {
			return implClass;
		}

		/**
		 * Returns the title.
		 * @return String
		 */
		public String getTitle() {
			return title;
		}
	

		/**
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return this.title;
		}

		/**
		 * Returns if the authentication of this database type is configurable
		 */
		public boolean isAuthConfigurable() {
			return authConfigurable;
		}

		/**
		 * Returns the type of database as constant GENTYPE_....
		 */
		public int getGeneralType() {
			return generalType;
		}

        /**
         * Returns if the database is a full featured content store
         */
        public boolean isFullContentStore() {
            return fullContentStore;
        }

        public int compareTo(Object arg0) {
            return getTitle().compareTo(((DbType) arg0).getTitle());
        }
	}
