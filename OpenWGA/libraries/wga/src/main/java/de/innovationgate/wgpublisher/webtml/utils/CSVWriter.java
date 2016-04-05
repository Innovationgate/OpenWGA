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
package de.innovationgate.wgpublisher.webtml.utils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;

public class CSVWriter {
	
	public static boolean writeCSV( String filePath, TMLForm form, boolean headers, String delimiter ) {

		try {
			File testFile 			= new File( filePath );
			FileWriter fileWriter	= null;
			List fieldList			= form.getfieldnames();
			String entryDelimiter	= delimiter;
			
			if( entryDelimiter == null || entryDelimiter.equals("") ) {
				entryDelimiter = ";";
			}
			
			Collections.sort( fieldList );
			
			// if header == true then write header fields first.
			if( headers == true && !testFile.exists() ) {
				fileWriter	= new FileWriter( filePath, true );
				Iterator fieldListIter = fieldList.iterator();
				
				while( fieldListIter.hasNext() ) {
					fileWriter.write( (String) fieldListIter.next() );
					if( fieldListIter.hasNext() ) {
						fileWriter.write( entryDelimiter + " " );
					}
				}
				fileWriter.write("\n");
			}
			else {
				fileWriter	= new FileWriter( filePath, true );
			}
			
			Iterator fieldIter	= fieldList.iterator();
			
			// write formdata to comma separated list, multiple value fields will be separated by "|"
			while( fieldIter.hasNext() ) {
				String fieldName = (String) fieldIter.next();
				if( fieldName!= null && !fieldName.equals("") ) {
					String fieldValue	= "";
					fieldValue			= ((List) form.fieldlist( fieldName )).toString()
						.substring(1, ((List) form.fieldlist( fieldName )).toString().length() - 1);
						
					// test, if field value is a multi value field, if so then substitude "," with "|"
					if( fieldValue.indexOf(",") != -1 ) {
						fileWriter.write( WGUtils.strReplace( fieldValue, ",", "|", true ) );
					}
					else {
						fileWriter.write( fieldValue );
					}
					
					// only add a "," if its not the last field in list.
					if( fieldIter.hasNext() ) {
						fileWriter.write( entryDelimiter + " " );
					}	
				}
			}
			fileWriter.write("\n");
			fileWriter.close();
			return true;
		}
		catch (IOException e) {
			e.printStackTrace();
			WGFactory.getLogger().error( "Error: CSV file couldn't be generated at given location!" );
			return false;
		}
		
	}



}
