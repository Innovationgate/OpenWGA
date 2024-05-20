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

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wga.server.api.Nav;
import de.innovationgate.wga.server.api.CollectionResult;
import de.innovationgate.wga.server.api.NavigatorResult;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.webtml.utils.ResultIterator;
import de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Navigator extends ForEach {
	
    private static final long serialVersionUID = 1L;
    public static final String TAGINFO_RESULTCOUNT = "resultcount";
	private String maxLevel;
	private String allareas;
	private String alllanguages;
	private String role;
	private String allowdefaultlang;
	private String exclude;
	private String contentclass;
	private String relation;
	private String relationgroup;
	private String order;
    
    public String getOrder() {
        return getTagAttributeValue("order", order, null);
    }

    public void setOrder(String order) {
        this.order = order;
    }

	
	public static class Status extends ForEach.Status implements ResultSetTagStatus {
	    private CollectionResult navigatorResult = null;
	    protected String navType;
	    
	    
	    @Override
	    public Object getTagInfo(String name) throws WGAPIException {

	        if (name.equals(TAGINFO_RESULTCOUNT)) {
	            return results();
	        }
	        
	        return super.getTagInfo(name);
	    }
	    
	    public TMLContext[] createContextList(WGContentList contentList) throws WGAPIException {

	        TMLContext[] contextList = new TMLContext[contentList.size()];
	        for (int idx = 0; idx < contentList.size(); idx++) {
	            contextList[idx] = tmlContext.getTMLContextForDocument((WGContent) contentList.get(idx));
	        }
	        return contextList;

	    }
	    
	    @Override
	    public void initAttributeDelegates(Base tag) {
	        super.initAttributeDelegates(tag);
	        
	        Navigator navTag = (Navigator) tag;
	        this.forEachType = "content";
	        this.navType = navTag.getType();
	    }
	    
	    public String getResultLanguage() {
	        return MULTILANGUAGE_RESULT;
	    }
	    
	    public ResultIterator<Object> getResultIterator() throws TMLException {
	        if (navigatorResult != null) {
	            SkippingIterator<Context> iterator = navigatorResult.iterator();
	            return new ResultIterator<Object>(new ContentUnwrappingIterator(iterator), new ResultIterator.ResultCount() {
                    @Override
                    public int results() {
                        return Status.this.results();
                    }
                });
	        }
	        else {
	            throw new TMLException("Tag has no result list", true);
	        }
	    } 

	    public int results() {
            try {
                if (this.navigatorResult != null) {
                    return navigatorResult.getSize();
                }
                else {
                    return 0;
                }
            }
            catch (WGException e) {
                tmlContext.getlog().error("Exception retrieving navigator result size", e);
                return 0;
            }
	    }
	    
	    public void addResultSet(WGResultSet resultSet, String language) {}
	}
	
	@Override
	public BaseTagStatus createTagStatus() {
	    Status status = new Status();
	    return status;
	}
	
	
	public void tmlStartTag() throws WGException {
		
	    Status status = (Status) getStatus();
		
		if (this.allowdefaultlang != null) {
		    addWarning("Attribute \"allowdefaultlang\" is deprecated and inoperable since OpenWGA 5.1");
		}
		
		WGContent relContent = this.getTMLContext().content();

		String type = status.navType;
       
		Map<String,Object> atts = new java.util.HashMap<String,Object>();
		atts.put(Nav.NAVATT_ALLAREAS, getAllareas());
		atts.put(Nav.NAVATT_ALLLANGUAGES, getAlllanguages());
		atts.put(Nav.NAVATT_MAXLEVEL, getMaxlevel());
		atts.put(Nav.NAVATT_ROLE, getRole());
		atts.put(Nav.NAVATT_ONLYPUBLISHED, getOnlypublished());
		atts.put(Nav.NAVATT_CONTENTCLASS, getContentclass());
		atts.put(Nav.NAVATT_RELATION, getRelation());
		atts.put(Nav.NAVATT_RELATIONGROUP, getRelationgroup());
		atts.put(Nav.NAVATT_PAGESIZE, getPageSize());
		atts.put(Nav.NAVATT_ORDER, getOrder());
		
		NavigatorResult navResult = WGA.get(getTMLContext().context(relContent)).nav().navigate(type, atts);
		status.onlyPublished = navResult.isOnlyPublished();
        status.navigatorResult = navResult;
		status.forEachType = "content";
		
		String exclude = getExclude();
        if (exclude != null) {
            final TMLContext excludeContext = getTMLContext().context(exclude, false);
            if (excludeContext != null) {
                status.navigatorResult = status.navigatorResult.filter(new CollectionResult.Filter() {
                    @Override
                    public boolean passesFilter(Context context) throws WGException {
                        return !context.content().getContentKey().equals(excludeContext.content().getContentKey());
                    }
                });
            }
            else {
                addWarning("Unresolvable exclude context: " + exclude, false);
            }
        }
		
		super.tmlStartTag();
		
	}

	/**
	 * Gets the maxLevel
	 * @return Returns a String
	 */
	public String getMaxlevel() {
		return this.getTagAttributeValue("maxlevel", maxLevel, null);
	}
	/**
	 * Sets the maxLevel
	 * @param maxLevel The maxLevel to set
	 */
	public void setMaxlevel(String maxLevel) {
		this.maxLevel = maxLevel;
	}

	/**
	 * Gets the allareas
	 * @return Returns a String
	 */
	public String getAllareas() {
		return this.getTagAttributeValue("allareas", allareas, "false");
	}
	/**
	 * Sets the allareas
	 * @param allareas The allareas to set
	 */
	public void setAllareas(String allareas) {
		this.allareas = allareas;
	}

	/**
	 * Gets the alllanguages
	 * @return Returns a String
	 */
	public String getAlllanguages() {
		return this.getTagAttributeValue("alllanguages", alllanguages, "false");
	}
	/**
	 * Sets the alllanguages
	 * @param alllanguages The alllanguages to set
	 */
	public void setAlllanguages(String alllanguages) {
		this.alllanguages = alllanguages;
	}







	/**
	 * Returns the role.
	 * @return String
	 */
	public String getRole() {
		return this.getTagAttributeValue("role", role, null);
	}

	/**
	 * Sets the role.
	 * @param role The role to set
	 */
	public void setRole(String role) {
		this.role = role;
	}

    /**
     * @return Returns the allowdefaultlang.
     */
    public String getAllowdefaultlang() {
        return getTagAttributeValue("allowdefaultlang", allowdefaultlang, "false");
    }
    /**
     * @param allowdefaultlang The allowdefaultlang to set.
     */
    public void setAllowdefaultlang(String allowdefaultlang) {
        this.allowdefaultlang = allowdefaultlang;
    }



    public String getExclude() {
        return getTagAttributeValue("exclude", exclude, null);
    }

    public void setExclude(String includecurrent) {
        this.exclude = includecurrent;
    }


    public String getContentclass() {
        return getTagAttributeValue("contentclass", contentclass, null);
    }


    public void setContentclass(String contentclass) {
        this.contentclass = contentclass;
    }


    public String getRelation() {
        return getTagAttributeValue("relation", relation,  null);
    }


    public void setRelation(String relation) {
        this.relation = relation;
    }


    public String getRelationgroup() {
        return getTagAttributeValue("relationgroup",  relationgroup, null);
    }


    public void setRelationgroup(String relationgroup) {
        this.relationgroup = relationgroup;
    }


}
