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

package de.innovationgate.wga.common.beans.csconfig.v1;

public class PublisherOption {
	
    
    public static final String OPTION_HOME_PAGE = "HomePage";
    public static final String OPTION_DESIGN_ENCODING = "DesignEncoding";
    public static final String OPTION_MULTI_LANGUAGE_CONTENT = "MultiLanguageContent";
    public static final String OPTION_LOGIN_PAGE = "LoginPage";
	public static final String OPTION_EXPRESSION_DEFAULT = "ExpressionDefault";
	public static final String OPTION_DEFAULT_MEDIA_KEY = "DefaultMediaKey";
	public static final String OPTION_DEFAULT_ITEM_ENCODING = "DefaultItemEncoding";
	public static final String OPTION_DIRECT_ACCESS_DEFAULT = "DirectAccessDefault";
	public static final String OPTION_USES_HDB = "isHDB";
    public static final String OPTION_ADMIN_APP = "AdminApp";
    public static final String OPTION_BROWSING_SECURITY = "BrowsingSecurity";
    public static final String OPTION_OVERLAY_SUPPORT = "OverlaySupport";
    
    public static final String OVERLAY_SUPPORT_NONE = "none";
    public static final String OVERLAY_SUPPORT_OPTIONAL = "optional";
    public static final String OVERLAY_SUPPORT_MANDATORY = "mandatory";

    public static final String OPTION_OVERLAY_THEME = "OverlayTheme";
    
    private String name;
    private String value;
    
    public PublisherOption() {
    }
    
    public PublisherOption(String name, String value) {
        super();
        this.name = name;
        this.value = value;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public String getValue() {
        return value;
    }
    public void setValue(String value) {
        this.value = value;
    }

}
