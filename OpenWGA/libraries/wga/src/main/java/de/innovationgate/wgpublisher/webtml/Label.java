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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Label extends Base {	
	
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String container;
	private String key;
	private String bundleName;
    private String param1;
    private String param2;
    private String param3;
    private String param4;
    private String param5;
    private String language;
    private String designdb;
	
 

	/**
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {

        String result = "";		
		
        String requestType = (String) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_REQUESTTYPE);
        if (WGPDispatcher.REQUESTTYPE_STATICTML.equals(requestType)) {
            result = getSystemLabel();
        }
        else {
            result = getWebTMLLabel();
        }
        
		
		this.setResult(result);

	}

    public String getWebTMLLabel() throws WGException {

        Design design;
        if(getDesigndb()!=null)
        	design = WGA.get(getTMLContext()).design(getDesigndb());
        else design = WGA.get(getTMLContext()).design();

    	HashMap<String,Object> config = new HashMap<String,Object>();
    	config.put("container", getContainer());
    	config.put("file", getFile());
    	config.put("params", collectParams());

        String language = getLanguage();
        if (language != null) {
        	config.put("language", language);
        }
        
        return design.label(getKey(), config);
        
    }

    private List<String> collectParams() {
        
        List<String> params = new ArrayList<String>();
        params.add(getParam1());
        params.add(getParam2());
        params.add(getParam3());
        params.add(getParam4());
        params.add(getParam5());
        return params;
    }
    

    private String getSystemLabel() {
        
        String labelKey = this.getKey();
        String systemBundleName = this.getBundle();
        String result = getTMLContext().systemLabel(systemBundleName, labelKey);
        return result;
    }

    public String getKey() {		
		return this.getTagAttributeValue("key", this.key, null);
	}
	
	public void setKey(String key) {
		this.key = key;
	}
	
	/**
	 * @return
	 */
	public String getBundle() {
		return this.getTagAttributeValue("bundle", this.bundleName, "common");		
	}

	/**
	 * @param string
	 */
	public void setBundle(String string) {
		bundleName = string;
	}
    
    public String getFile() {
        return this.getTagAttributeValue("file", this.bundleName, null);
    }
    
    public void setFile(String string) {
        setBundle(string);
    }

    /**
     * @return Returns the container.
     */
    public String getContainer() {
        return getTagAttributeValue("container", container, null);
    }

    /**
     * @param container The container to set.
     */
    public void setContainer(String container) {
        this.container = container;
    }

    /**
     * @return Returns the param1.
     */
    public String getParam1() {
        return getTagAttributeValue("param1", param1, "");
    }

    /**
     * @param param1 The param1 to set.
     */
    public void setParam1(String param1) {
        this.param1 = param1;
    }

    /**
     * @return Returns the param2.
     */
    public String getParam2() {
        return getTagAttributeValue("param2", param2, "");
    }

    /**
     * @param param2 The param2 to set.
     */
    public void setParam2(String param2) {
        this.param2 = param2;
    }

    /**
     * @return Returns the param3.
     */
    public String getParam3() {
        return getTagAttributeValue("param3", param3, "");
    }

    /**
     * @param param3 The param3 to set.
     */
    public void setParam3(String param3) {
        this.param3 = param3;
    }

    /**
     * @return Returns the param4.
     */
    public String getParam4() {
        return getTagAttributeValue("param4", param4, "");
    }

    /**
     * @param param4 The param4 to set.
     */
    public void setParam4(String param4) {
        this.param4 = param4;
    }

    /**
     * @return Returns the param5.
     */
    public String getParam5() {
        return getTagAttributeValue("param5", param5, "");
    }

    /**
     * @param param5 The param5 to set.
     */
    public void setParam5(String param5) {
        this.param5 = param5;
    }

    public String getLanguage() {
        return getTagAttributeValue("language", language, null);
    }

    public void setLanguage(String language) {
        this.language = language;
    }

	/**
	 * Returns the designdb.
	 * @return String
	 */
	public String getDesigndb() {
		return this.getTagAttributeValue("designdb", designdb, null);
	}

	/**
	 * Sets the designdb.
	 * @param designdb The designdb to set
	 */
	public void setDesigndb(String designdb) {
		this.designdb = designdb;
	}

}

