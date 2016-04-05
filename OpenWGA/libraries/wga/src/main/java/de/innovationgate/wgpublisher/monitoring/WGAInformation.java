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
package de.innovationgate.wgpublisher.monitoring;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;

import javax.servlet.ServletException;

import org.dom4j.DocumentException;

import de.innovationgate.wgpublisher.WGACore;

public class WGAInformation implements WGAInformationMBean
{
	private WGACore core;
	
	
	public WGAInformation( WGACore core )
	{
		this.core = core;
	}
	
	public String getApplicationServer()
	{
		return this.core.getServletContext().getServerInfo();
	}

	public String getBruteForceLoginBlockerStatus()
	{
		int blockedSize = core.getBruteForceLoginBlocker().getBlockedLogins().size();
		
		if( blockedSize > 0 )
		{
			Integer size = new Integer( blockedSize );
			return size.toString() + " Logins blocked";
		}
		else
			return "All logins are available";
	}

	public String getBuildSignature()
	{
		return WGACore.getBuildSignature();
	}

	public String getConfigFile()
	{
		return this.core.getConfigFilePath();
	}

	public String getDefaultSystemLocale()
	{
		return java.util.Locale.getDefault().toString();
	}

	public String getInstanceActiveSince()
	{
		return this.core.getInstanceActiveSince().toString();
	}

	public String getLogOutputDir()
	{
		return this.core.getLoggingDir() != null ? this.core.getLoggingDir().getPath() : "(none)";
	}

	public String getLuceneIndexerStatus()
	{
		return this.core.getLuceneManager() == null ? "Not activated" : this.core.getLuceneManager().isIndexerRunning() ? "Indexing content" : "Idle";
	}

	public String getOperatingSystem()
	{
		return System.getProperty("os.name") + " Version " + System.getProperty("os.version") + " (" + System.getProperty("os.arch") + ")";
	}

	public String getOperatingSystemUser()
	{
		return System.getProperty( "user.name" );
	}

	public String getRelease()
	{
		return WGACore.getReleaseString();
	}

	public String getTempFilesDir()
	{
		return this.core.getWgaTempDir().getPath();
	}


	public String restartApplication()
	{
		ClassLoader oldcl = Thread.currentThread().getContextClassLoader();
		ClassLoader newcl = this.core.getClass().getClassLoader();
		
		if( newcl != null )
			Thread.currentThread().setContextClassLoader( newcl );
		
		try
		{
			this.core.shutdown();
			this.core.startup();
			return "Successfully restarted application.";
		}
		catch( ServletException e )
		{
			return "Application could not be restarted.";
		}
		finally
		{
			Thread.currentThread().setContextClassLoader( oldcl );
		}
	}
	
	public String clearLog()
	{
		this.core.initLoggingFile();
		return "Successfully cleared application log.";
	}
	
	public String cacheDump()
	{
		try
		{
			String fileName = this.core.tmlCacheDump();
			return "WebTML cache dump file written to: " + fileName;
		}
		catch( java.io.IOException e )
		{
			return "ERROR CREATING CACHE DUMP: " + e.getMessage();
		}
	}
	
	public String reloadConfig()
	{
		try
		{
			this.core.updateConfig();
			return "Reloaded configuration";
		}
		catch( Exception e )
		{
			return "Configuration could not be reloaded.";		
		}
	}
	
	public String rebuildLuceneIndex()
	{
		if( this.core.getLuceneManager() != null )
		{
   			try
   			{
				this.core.getLuceneManager().rebuildIndex();
				return "Rebuilding lucene index, check application log for details.";
			}
   			catch( Exception e )
   			{
				this.core.getLog().error(e);	//	?
				return "Rebuilding lucene index failed. Exception: '" + e.getClass().getName() + "' " + e.getMessage();
			}				
		}
		else
		{
			return "Lucene fulltext index is disabled.";
		}	
	}

	public int getCoreStatus() {
		int status = -1;
		if (core != null) {
			status = core.getStatus();
		}
		return status;
	} 
}
