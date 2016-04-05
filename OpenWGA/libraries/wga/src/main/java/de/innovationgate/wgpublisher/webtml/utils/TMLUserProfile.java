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

import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.server.api.tml.UserProfile;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;

@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE,delegate=UserProfile.class)
public class TMLUserProfile implements TMLObject, UserProfile {
	
	public static final int RC_OK = 0;
	public static final int RC_NO_DOMAIN = 1;
	public static final int RC_PROFILE_EXISTS = 2;
	public static final int RC_WRONG_PERSMODE = 3;
	public static final int RC_NOT_CREATABLE = 4;
	public static final int RC_NOT_PERSONALIZED = 5;
	public static final int RC_WRONG_PASSWORD = 6;
	public static final int RC_NO_PROFILE = 7;
	public static final int RC_NO_SESSION = 8;
	public static final int RC_METHOD_UNAVAILABLE = 9;
		
	private WGUserProfile _profile;
    private boolean _savedOnEnd = true;
    private WGACore _core;
    private int _persMode;
    private String _profileSessionId;
    private Version _complianceVersion;
	
	public TMLUserProfile(WGUserProfile profile, WGACore core, int persMode, HttpSession session, Version complianceVersion) {
		_profile = profile;
		_core = core;
		_persMode = persMode;
		_complianceVersion = complianceVersion;
		try {
            _profileSessionId = generateProfileSessionId(session, profile);
        }
        catch (Exception e) {
        }
	}
	
	public void setpassword(String pwd) throws WGAPIException {
		_profile.setPassword(pwd);
	} 
 
	private Object retrieveitem(String name) throws WGAPIException {
        if (_profile.hasItem(name)) {
            return _profile.getItemValue(name);
        }
        else {
            // Symbolizes that the item does not exist
            return new NullPlaceHolder();
        }
	} 
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#removeitem(java.lang.String)
     */
	@Override
    @CodeCompletion
    public void removeitem(String name) throws WGAPIException {
        _profile.removeItem(name);
    }
	
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#item(java.lang.String)
     */
    @Override
    @CodeCompletion
	public Object item(String name) throws WGAPIException {
        
		Object result = TMLContext.flattenList(retrieveitem(name), !_complianceVersion.isAtLeast(7,2));
        if (result instanceof NullPlaceHolder) {
            return _profile.getDatabase().getNoItemBehaviour().getForTMLItem();
        }
        else {
            return result;
        }
        
	}
	
	public Object retrievemeta(String name) throws WGAPIException {
		return _profile.getMetaData(name);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#meta(java.lang.String)
     */
	@Override
    @CodeCompletion
	public Object meta(String name) throws WGAPIException {
		return TMLContext.flattenList(retrievemeta(name), !_complianceVersion.isAtLeast(7,2));
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#setitem(java.lang.String, java.lang.Object)
     */
	@Override
    @CodeCompletion
	public boolean setitem(String name, Object value) throws WGAPIException {
		return _profile.setItemValue(name, value);
	}

	public boolean save(boolean force) throws WGAPIException {
        
        if (_savedOnEnd && !force) {
            return true;
        }
        else {
            return _profile.save();
        }
		
	}
	
	@CodeCompletion
	public boolean save() throws WGAPIException {
	    return save(false);
	}
	
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#optinstorage()
     */
	@Override
    @CodeCompletion
	public boolean optinstorage() throws WGAPIException {
	    return save(true);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#isstorageoptedin()
     */
	@Override
    @CodeCompletion
	public boolean isstorageoptedin() throws WGAPIException {
	    
	    if (getprofile().isSaved()) {
	        return true;
	    }
	    
	    boolean optIn = (Boolean) _core.readPublisherOptionOrDefault(getprofile().getDatabase(), WGACore.DBATTRIB_PERSMODE_OPT_IN);
	    if (!optIn) {
	        return true;
	    }
	    
	    return false;
	}

	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#getprofile()
     */
	@Override
    @CodeCompletion
	public WGUserProfile getprofile() {
		return _profile;
	}

	/**
	 * @see de.innovationgate.wgpublisher.webtml.utils.TMLObject#getAPIObject()
	 */
	public WGDocument getapiobject() {
		return this._profile;
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#itemlist(java.lang.String)
     */
	@Override
    @CodeCompletion
	public List<Object> itemlist(String name) throws WGAPIException {
		
        Object result = retrieveitem(name);
        if (result instanceof NullPlaceHolder) {
            return _profile.getDatabase().getNoItemBehaviour().getForTMLItemList();
        }
        else {
            return TMLContext.toList(result);
        }
		
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#metalist(java.lang.String)
     */
	@Override
    @CodeCompletion
	public List<Object> metalist(String name) throws WGAPIException {
		
		return TMLContext.toList(retrievemeta(name));
		
		
	}
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.UserProfile#hasitem(java.lang.String)
     */
	@Override
    @CodeCompletion
    public boolean hasitem(String name) throws WGAPIException {
        return _profile.hasItem(name);        
    }

    /**
     * @return Returns the savedOnEnd.
     */
    public boolean isSavedOnEnd() {
        return _savedOnEnd;
    }

    /**
     * @param savedOnEnd The savedOnEnd to set.
     */
    public void setSavedOnEnd(boolean savedOnEnd) {
        this._savedOnEnd = savedOnEnd;
    }

    public static final void prepareNewProfile(WGUserProfile profile, HttpServletRequest request) throws WGAPIException {
        
        // Set WGA Version under which this profile was created. Helps to determine
        // differing storage strategies used for profiles of different versions
        String version = WGAVersion.toCsConfigVersion().toString();
        profile.setItemValue(WGUserProfile.ITEM_WGACREATIONVERSION, version);
        
        // Fields only written once at profile creation, because they are not threadsafe writable
        if (request != null) {
            Enumeration<Locale> langsIt = request.getLocales();
            List<String> langs = new ArrayList<String>();
            while (langsIt.hasMoreElements()) {
                langs.add(langsIt.nextElement().getLanguage());
            }
            profile.setLanguages(langs);
        }
    }
    
    public boolean isdummy() {
        return _profile.isDummy();
    }

    public int getPersMode() {
        return _persMode;
    }

    public String getProfileSessionId() {
        return _profileSessionId;
    }

    public static String generateProfileSessionId(HttpSession session, WGUserProfile userProfile) throws NoSuchAlgorithmException, UnsupportedEncodingException {

        StringBuffer idString = new StringBuffer();
        idString.append(session.getId());
        idString.append(String.valueOf(System.identityHashCode(userProfile)));
        return WGUtils.createMD5HEX(idString.toString().getBytes("UTF-8"));      
        
        
    }

    public Version getComplianceVersion() {
        return _complianceVersion;
    }
    


}
