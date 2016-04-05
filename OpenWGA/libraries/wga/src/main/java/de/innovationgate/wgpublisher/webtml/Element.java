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
package de.innovationgate.wgpublisher.webtml;
import java.net.MalformedURLException;
import java.net.URLClassLoader;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.ElementImpl;
import de.innovationgate.wgpublisher.webtml.utils.ElementImplContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Element extends Base implements PreferredOptionReceiverTag {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    // Attributes
	private String name;
	private String classname;
	private String source;
	
	
	public static class Status extends BaseTagStatus { 
    	private ElementImpl elementImpl;
    	protected ElementImplContext elementImplContext = null;
    	public Body bodyTag = null;
    	
    	@Override
    	public Object getTagInfo(String name) throws WGAPIException {

    	    Object info = elementImpl.tagInfo(name);
    	    if (info != null) {
    	        return info;
    	    }
    	    
    	    return super.getTagInfo(name);
    	}
    	
	   protected void maybeCallBegin() {
	        try {
	            if (elementImplContext == null) {
	                elementImplContext = new ElementImplContext(new ElementOptionWrapper(tagOptions), tmlContext, this);
	                elementImpl.begin(elementImplContext);
	            }
	        }
	        catch (Exception exc) {
	            tmlContext.addwarning("Error executing element: " + exc.getMessage());
	            log.error("Error executing element", exc);
	        }
	        catch (Error err) {
	            tmlContext.addwarning("Error executing element: " + err.getMessage());
	            log.error("Error executing element", err);
	        }
	    }
	   
	   protected boolean callBeforeBody() {
	        try {
	            maybeCallBegin();
	            return elementImpl.beforeBody(elementImplContext);
	        }
	        catch (Exception exc) {
	            tmlContext.addwarning("Error executing element: " + exc.getMessage());
	            log.error("Error executing element", exc);
	            return false;
	        }
	        catch (Error err) {
	            tmlContext.addwarning("Error executing element: " + err.getMessage());
	            log.error("Error executing element", err);
	            return false;
	        }
	    }

    protected boolean callAfterBody() {
    	try {
    		return elementImpl.afterBody(elementImplContext);
    	}
    	catch (Exception exc) {
    		tmlContext.addwarning("Error executing element: " + exc.getMessage());
    		log.error("Error executing element", exc);
    		return false;
    	}
    	catch (Error err) {
    		tmlContext.addwarning("Error executing element: " + err.getMessage());
    		log.error("Error executing element", err);
    		return false;
    	}
    }
	}
	
	@Override
	public BaseTagStatus createTagStatus() {
	    return new Status();
	}
	
	public Status getStatus() {
	    return (Status) super.getStatus();
	}


    
	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException {
		
	    Status status = (Status) getStatus();
		status.elementImpl = null;
		status.elementImplContext = null;
		status.bodyTag = null;

		// Be sure, there is an name supplied
		String classname = this.getClassname();
		String name = this.getName();
		if (name == null && classname == null) {
			this.addWarning("No name or class of element implementation given", true);
			return;
		}
		
		if (name != null) {
			classname = this.getCore().getElementClassForName(name);
			if (classname == null || classname.equals("")) {
				this.addWarning("No element defined with name: " + name, true);
				return;
			}
		}

		// Get Element loader
		Class elementImplClass = null;
		ClassLoader elementLoader = WGACore.getLibraryLoader();

		// If source given, build a new class loader
		String source = this.getSource();
		if (source != null) {
			java.net.URL[] urls = new java.net.URL[1];
			try {
				urls[0] = new java.net.URL(this.getSource());
			}
			catch (MalformedURLException exc) {
				this.addWarning("Malformed url: " + this.getSource(), true);
				return;
			}
			elementLoader = new URLClassLoader(urls, elementLoader);
		}
		
		// try to load class
		try {
			elementImplClass = elementLoader.loadClass(classname);
		}
		catch (ClassNotFoundException exc) {
		}
		catch (Exception exc) {
			log.error("Error retrieving element implementation", exc);
		}
		catch (NoClassDefFoundError err) {
			log.error("Error retrieving element implementation", err);
		}

		if (elementImplClass == null) {
			this.addWarning("Unable to find element implementation class: " + classname, true);
			return;
		}

		// Test if it fits the ElementImpl-Interface
		if (!ElementImpl.class.isAssignableFrom(elementImplClass)) {
			this.addWarning("Class " + classname + " does not implement necessary interface " + ElementImpl.class.getName(), true);
			return;
		}

		// Instatiate element implementation
		try {
			status.elementImpl = (ElementImpl) elementImplClass.newInstance();
		}
		catch (InstantiationException exc) {
			this.addWarning(
				"Class " + elementImplClass.getName() + " cannot be instantiated. Possible reasons: no default constructor, no instantiable type (e.g. an abstract class)",
				true);
			this.setCancelTag(true);
			return;
		}
		catch (IllegalAccessException exc) {
			this.addWarning("Class " + elementImplClass.getName() + " cannot be instantiated because of access restrictions.", true);
			this.setCancelTag(true);
			return;
		}
		catch (Exception exc) {
			log.error("Error instantiating element implementation", exc);
			this.addWarning("Class " + elementImplClass.getName() + " threw exception when instantiating: " + exc.getLocalizedMessage());
			this.setCancelTag(true);
			return;
		}
		catch (Error err) {
			log.error("Error instantiating element implementation", err);
			this.addWarning("Class " + elementImplClass.getName() + " threw error when instantiating: " + err.getLocalizedMessage());
			this.setCancelTag(true);
			return;
		}

	}

	/**
	 * Gets the source
	 * @return Returns a String
	 */
	public String getSource() {
		return this.getTagAttributeValue("source", source, null);
	}
	/**
	 * Sets the source
	 * @param source The source to set
	 */
	public void setSource(String source) {
		this.source = source;
	}

	

	/**
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws TMLException, WGAPIException {
		
		Status status = (Status) getStatus();
		status.maybeCallBegin();
		try {
			status.elementImpl.end(status.elementImplContext);
		}
		catch (Exception exc) {
			log.error("Error executing element", exc);
		}
		catch (Error err) {
			log.error("Error executing element", err);
		}

		this.appendResult(status.elementImplContext.getResult().toString());
	}

	/**
	 * Gets the bodyTag
	 * @return Returns a Body
	 */
	public Body getBodyTag() {
	    Status status = (Status) getStatus();
		return status.bodyTag;
	}

	/**
	 * Gets the classname
	 * @return Returns a String
	 */
	public String getClassname() {
		return this.getTagAttributeValue("classname", classname, null);
	}
	/**
	 * Sets the classname
	 * @param classname The classname to set
	 */
	public void setClassname(String classname) {
		this.classname = classname;
	}    
}
