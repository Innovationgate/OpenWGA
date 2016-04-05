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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.servlet.http.HttpSession;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.IterationTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class ConditionBase extends Base {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    // condition attributes
	private String role = null;
	private String condition = null;
	private String isSelected = null;
	private String isRoot = null;
	private String isCurrentDocument = null;
	private String hasChildren = null;
	private String hasSiblings = null;
	private String hasOption = null;
	private String hasoptions = null;
	private String hasUrlParameter = null;
	private String hasNextPage = null;
	private String hasPreviousPage = null;
	private String iscontextvalid = null;
	private String istagidvalid = null;
	private String isbrowserinterface = null;
	private String iseditmode = null;
	private String isnewsession = null;
	private String istrue = null;
	private String isfalse = null;
	private String isdefined = null;
	private String platform = null;
	private String doctype = null;
	private String xplanguage = null;
	private String portletmode = null;
    private String isempty = null;
    private String isfilled = null;
    private String hasroles = null;
    private String hasgroups = null;
    private String hasprofile = null;
	private String language = null;
	private String isanonymous = null;
	
	private String isfirstloop = null;
	private String islastloop = null;
	

	protected boolean testCondition() {

		boolean result = innerTestCondition();
		org.dom4j.Element debugNode = getStatus().debugNode;
		if (debugNode != null) {
			debugNode.addAttribute("conditionis", String.valueOf(result));
		}
		return result;
		
	}

	protected boolean innerTestCondition() {

        TMLContext context = this.getTMLContext();
        context.setrole(this.getRole());
	    try {
			
			WGContent content = context.content();
			
			String attValue;
			attValue = this.getIscontextvalid();
            if (attValue != null) {
                boolean iscontextvalid = !this.isChildContextErrornous();
                if (this.stringToBoolean(attValue) == false) {
                    iscontextvalid = !iscontextvalid;
                }
                if (iscontextvalid == false) {
                    return false;
                }
            }
            
            attValue = this.getIstagidvalid();
            if (attValue != null) {
                boolean istagidvalid = getTMLContext().istagidvalid(attValue);
                if (istagidvalid == false) {
                    return false;
                }
            }
			
			attValue = this.getIstrue();
			if (attValue != null) {
				if (!context.istrue(attValue)) {
					return false;
				}
			}
		
			attValue = this.getIsfalse();
			if (attValue != null) {
				if (!context.isfalse(attValue)) {
					return false;
				}
			}
			
			attValue = this.getIsdefined();
			if (attValue != null) {
				if (!context.isdefined(attValue)) {
					return false;
				}
			}
			
			attValue = this.getCondition();
			if (attValue != null) {
				ExpressionEngine engine = ExpressionEngineFactory.getEngine(this.getConditionlanguage());
				if (engine == null) {
					this.addWarning("Unknown expression language: " + this.getConditionlanguage(), true);
					return false;
				}
				
				
				Map objects = new HashMap();
				objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "Condition on " + getTagDescription());
				ExpressionResult result = engine.evaluateExpression(attValue, this.getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, objects);
				if (result.isError()) {
				    addExpressionWarning(attValue,result);
					return false;
				}
				else if (result.isFalse()) {
					return false;
				}
			}
		
			attValue = this.getIsselected();
			if (attValue != null) {
				boolean isSelected = context.isselected();
				if (this.stringToBoolean(attValue) == false) {
					isSelected = !isSelected;
				}
				if (isSelected == false) {
					return false;
				}
			}
			
			attValue = this.getHaschildren();
			if (attValue != null) {
				boolean hasChildren = context.haschildren();
				if (this.stringToBoolean(attValue) == false) {
					hasChildren = !hasChildren;
				}
				if (hasChildren == false) {
					return false;
				}				
			}
			
			attValue = this.getHassiblings();
			if (attValue != null) {
				boolean hasSiblings = context.hassiblings();
				if (this.stringToBoolean(attValue) == false) {
					hasSiblings = !hasSiblings;
				}
				if (hasSiblings == false) {
					return false;
				}				
			}

			attValue = this.getHasoption();
			if (attValue != null) {
				if(!context.hasoption(attValue))
					return false;
			}
			
			attValue = this.getHasoptions();
			if (attValue != null) {
			    for (String option : WGUtils.deserializeCollection(attValue, ",", true)) {
			        if(!context.hasoption(option)) {
	                    return false;
			        }
			    }
			}

			attValue = this.getIsroot();
			if (attValue != null) {
				boolean isRoot = context.isroot();
				if (this.stringToBoolean(attValue) == false) {
					isRoot = !isRoot;
				}
				if (isRoot == false) {
					return false;
				}
			}
			
			attValue = this.getIscurrentdocument();
			if (attValue != null) {
				boolean isCurrentDocument = context.ismaindocument();
				if (this.stringToBoolean(attValue) == false) {
					isCurrentDocument = !isCurrentDocument;
				}
				if (isCurrentDocument == false) {
					return false;
				}
			}
			
			attValue = this.getHasurlparameter();
			if (attValue != null) {
				String urlParam = this.pageContext.getRequest().getParameter(attValue);
				boolean hasUrlParameter = (urlParam != null ? true : false);
				if (hasUrlParameter == false) {
					return false;
				}
			}
			
			attValue = this.getHasnextpage();
			if (attValue != null) {
				IterationTagStatus iterationTag = (IterationTagStatus) this.getTagStatusById(attValue, IterationTagStatus.class);
				if (iterationTag == null) {
					this.addWarning("Could not find iteration tag: " + this.getHasnextpage());
					return false;
				}
				if (!iterationTag.hasNextPage()) {
					return false;
				}
			}
		
			attValue = this.getHaspreviouspage();
			if (attValue != null) {
			    IterationTagStatus iterationTag = (IterationTagStatus) this.getTagStatusById(attValue, IterationTagStatus.class);
				if (iterationTag == null) {
					this.addWarning("Could not find iteration tag: " + attValue);
					return false;
				}
				if (!iterationTag.hasPreviousPage()) {
					return false;
				}
			}
			
			attValue = this.getPlatform();
			if (attValue != null) {
				if (!attValue.equalsIgnoreCase("wgpublisher")) {
					return false;
				}
			}
			
			attValue = this.getDoctype();
			if (attValue != null) {
				if (content.getStructEntry() == null || content.getStructEntry().getContentType() == null) {
					return false;
				}
				
				if (!attValue.equalsIgnoreCase(content.getStructEntry().getContentType().getName())) {
					return false;
				}
			}
			
			attValue = this.getIsbrowserinterface();
			if (attValue != null) {
				boolean isBI = context.isbrowserinterface();
				if (this.stringToBoolean(attValue) == false) {
					isBI = !isBI;
				}
				if (isBI == false) {
					return false;
				}
			}
			
			attValue = this.getIseditmode();
			if (attValue != null) {
				boolean isEditmode = (context.isbrowserinterface() && content.getStatus().equals(WGContent.STATUS_DRAFT));
				if (this.stringToBoolean(attValue) == false) {
					isEditmode = !isEditmode;
				}
				if (isEditmode == false) {
					return false;
				}
			}
			
			attValue = this.getIsnewsession();
			if (attValue != null) {
				boolean isNewSession = context.isnewsession();
				if (this.stringToBoolean(attValue) == false) {
					isNewSession = !isNewSession;
				}
				if (isNewSession == false) {
					return false;
				}
			}
			
			attValue = this.getPortletmode();
			if (attValue != null) {
				List modesToTest = WGUtils.deserializeCollection(attValue.toLowerCase(), ",", true);
				HttpSession session = this.pageContext.getSession();
				TMLPortlet portlet = context.getportlet();
                if (portlet == null) {
                    addWarning("Portlet mode was tested although no portlet was registered", false);
                    return false;
                }
                
                String portletMode	= portlet.getmode();
 
				if (!modesToTest.contains(portletMode.toLowerCase())) {
					return false;
				}
			}
			
			attValue = this.getHasprofile();
			if (attValue != null) {
				
				boolean hasProfile = context.hasprofile();
				if (this.stringToBoolean(attValue) == false) {
					hasProfile = !hasProfile;
				}
				if (hasProfile == false) {
					return false;
				}
				
			}
			
			attValue = this.getLanguage();
			if (attValue != null) {
				
				String prefLanguage = getTMLContext().getpreferredlanguage();
				if (prefLanguage == null) {
					prefLanguage = (String) getTMLContext().meta("language");
				}
				if (!prefLanguage.equals("attValue")) {
					return false;
				}
								
			}
            
            attValue = this.getIsempty();
            if (attValue != null) {
                if (!getTMLContext().isempty(attValue)) {
                    return false;
                }
            }

            attValue = this.getIsfilled();
            if (attValue != null) {
                
                // Bypass this test in edit mode (as it most likely is used to hide fields that are not filled)
                boolean isEditmode = (context.isbrowserinterface() && content.getStatus().equals(WGContent.STATUS_DRAFT));
                if (!isEditmode) {
                    if (getTMLContext().isempty(attValue)) {
                        return false;
                    }
                }
            }
            
            attValue = this.getIsfirstloop();
            if (attValue != null) {
            	boolean firstLoop = false;
            	if (WGUtils.isBooleanValue(attValue)) {
            		firstLoop = getTMLContext().isfirstloop();
            		if (this.stringToBoolean(attValue) == false) {
						firstLoop = !firstLoop;
					}
            	} else {
            		firstLoop = getTMLContext().isfirstloop(attValue);
            	}
            	if (firstLoop == false) {
            		return false;
            	}
            }
            
            attValue = this.getIslastloop();
            if (attValue != null) {
            	boolean lastLoop = false;
            	if (WGUtils.isBooleanValue(attValue)) {
            		lastLoop = getTMLContext().islastloop();
            		if (this.stringToBoolean(attValue) == false) {
						lastLoop = !lastLoop;
					}
            	} else {
            		lastLoop = getTMLContext().islastloop(attValue);
            	}
            	if (lastLoop == false) {
            		return false;
            	}
            }
            
            attValue = this.getHasroles();
            if (attValue != null) {
                
                List<String> roles = WGUtils.deserializeCollection(attValue, ",", true);
                for (String role : roles) {
                    if (!getTMLContext().hasrole(role)) {
                        return false;
                    }
                }
                
            }
            
            attValue = this.getHasgroups();
            if (attValue != null) {
                
                List<String> groups = WGUtils.deserializeCollection(attValue, ",", true);
                for (String group : groups) {
                    if (!getTMLContext().hasgroup(group)) {
                        return false;
                    }
                }
                
            }
            
            attValue = this.getIsanonymous();
            if (attValue != null) {
                
                boolean expected = stringToBoolean(attValue);
                if (getTMLContext().isanonymous() != expected) {
                    return false;
                }
                
            }
            
            
			return true;
		}
		catch (Exception exc) {
			log.error("Error evaluating expression", exc);
			return false;
		}
		catch (Error err) {
			log.error("Error evaluating expression", err);
			return false;
		}
		finally {
		    context.setrole(null);
		}
	}



	/**
	 * Gets the condition
	 * @return Returns a String
	 */
	public String getCondition() {
		return this.getTagAttributeValue("condition", this.condition, null);
	}
	/**
	 * Sets the condition
	 * @param condition The condition to set
	 */
	public void setCondition(String condition) {
		this.condition = condition;
	}

	/**
	 * Gets the isSelected
	 * @return Returns a String
	 */
	public String getIsselected() {
		return this.getTagAttributeValue("isselected", this.isSelected, null);
	}
	/**
	 * Sets the isSelected
	 * @param isSelected The isSelected to set
	 */
	public void setIsselected(String isSelected) {
		this.isSelected = isSelected;
	}

	/**
	 * Gets the hasChildren
	 * @return Returns a String
	 */
	public String getHaschildren() {
		return this.getTagAttributeValue("haschildren", this.hasChildren, null);
	}
	/**
	 * Sets the hasChildren
	 * @param hasChildren The hasChildren to set
	 */
	public void setHaschildren(String hasChildren) {
		this.hasChildren = hasChildren;
	}

	/**
	 * Gets the hasSiblings
	 * @return Returns a String
	 */
	public String getHassiblings() {
		return this.getTagAttributeValue("hassiblings", this.hasSiblings, null);
	}
	/**
	 * Sets the hasSiblings
	 * @param hasSiblings The hasSiblings to set
	 */
	public void setHassiblings(String hasSiblings) {
		this.hasSiblings = hasSiblings;
	}

	/**
	 * Gets the hasOption
	 * @return Returns a String
	 */
	public String getHasoption() {
		return this.getTagAttributeValue("hasoption", this.hasOption, null);
	}
	/**
	 * Sets the hasOption
	 * @param hasOption The value to set
	 */
	public void setHasoption(String hasOption) {
		this.hasOption = hasOption;
	}

	/**
	 * Gets the platform
	 * @return Returns a String
	 */
	public String getPlatform() {
		return this.getTagAttributeValue("platform", this.platform, null);
	}
	/**
	 * Sets the platform
	 * @param platform The platform to set
	 */
	public void setPlatform(String platform) {
		this.platform = platform;
	}

	/**
	 * Gets the isRoot
	 * @return Returns a String
	 */
	public String getIsroot() {
		return this.getTagAttributeValue("isroot", this.isRoot, null);
	}
	/**
	 * Sets the isRoot
	 * @param isRoot The isRoot to set
	 */
	public void setIsroot(String isRoot) {
		this.isRoot = isRoot;
	}

	/**
	 * Gets the isCurrentDocument
	 * @return Returns a String
	 */
	public String getIscurrentdocument() {
		return this.getTagAttributeValue("iscurrentdocument", this.isCurrentDocument, null);
	}
	/**
	 * Sets the isCurrentDocument
	 * @param isCurrentDocument The isCurrentDocument to set
	 */
	public void setIscurrentdocument(String isCurrentDocument) {
		this.isCurrentDocument = isCurrentDocument;
	}

	/**
	 * Gets the hasUrlParameter
	 * @return Returns a String
	 */
	public String getHasurlparameter() {
		return this.getTagAttributeValue("hasurlparameter", hasUrlParameter, null);
	}
	/**
	 * Sets the hasUrlParameter
	 * @param hasUrlParameter The hasUrlParameter to set
	 */
	public void setHasurlparameter(String hasUrlParameter) {
		this.hasUrlParameter = hasUrlParameter;
	}

	/**
	 * Gets the hasNextPage
	 * @return Returns a String
	 */
	public String getHasnextpage() {
		return this.getTagAttributeValue("hasnextpage", hasNextPage, null);
	}
	/**
	 * Sets the hasNextPage
	 * @param hasNextPage The hasNextPage to set
	 */
	public void setHasnextpage(String hasNextPage) {
		this.hasNextPage = hasNextPage;
	}

	/**
	 * Gets the hasPreviousPage
	 * @return Returns a String
	 */
	public String getHaspreviouspage() {
		return this.getTagAttributeValue("haspreviouspage", hasPreviousPage, null);
	}
	/**
	 * Sets the hasPreviousPage
	 * @param hasPreviousPage The hasPreviousPage to set
	 */
	public void setHaspreviouspage(String hasPreviousPage) {
		this.hasPreviousPage = hasPreviousPage;
	}

	/**
	 * Gets the iscontextvalid
	 * @return Returns a String
	 */
	public String getIscontextvalid() {
		return this.getTagAttributeValue("iscontextvalid", iscontextvalid, null);
	}
	/**
	 * Sets the iscontextvalid
	 * @param iscontextvalid The iscontextvalid to set
	 */
	public void setIscontextvalid(String iscontextvalid) {
		this.iscontextvalid = iscontextvalid;
	}

	/**
	 * Gets the role
	 * @return Returns a String
	 */
	public String getRole() {
		return this.getTagAttributeValue("role", role, null);
	}
	/**
	 * Sets the role
	 * @param role The role to set
	 */
	public void setRole(String role) {
		this.role = role;
	}

	/**
	 * Gets the istagidvalid
	 * @return Returns a String
	 */
	public String getIstagidvalid() {
		return this.getTagAttributeValue("istagidvalid", istagidvalid, null);
	}
	/**
	 * Sets the istagidvalid
	 * @param istagidvalid The istagidvalid to set
	 */
	public void setIstagidvalid(String istagidvalid) {
		this.istagidvalid = istagidvalid;
	}

	/**
	 * Gets the doctype
	 * @return Returns a String
	 */
	public String getDoctype() {
		return this.getTagAttributeValue("doctype", doctype, null);
	}
	
	public String getContenttype() {
		return getDoctype();
	}
	
	public void setContenttype(String ct) {
		setDoctype(ct);
	}
	/**
	 * Sets the doctype
	 * @param doctype The doctype to set
	 */
	public void setDoctype(String doctype) {
		this.doctype = doctype;
	}

	/**
	 * Gets the xplanguage
	 * @return Returns a String
	 */
	public String getConditionlanguage() {
		return this.getTagAttributeValue("xplanguage", xplanguage, this.getDefaultExpressionLanguage());
	}
	
	public String getXplanguage() {
		return this.getConditionlanguage();
	}
	/**
	 * Sets the xplanguage
	 * @param xplanguage The xplanguage to set
	 */
	public void setConditionlanguage(String xplanguage) {
		this.xplanguage = xplanguage;
	}
	
	public void setXplanguage(String xplanguage) {
		this.setConditionlanguage(xplanguage);
	}

	/**
	 * Gets the isbrowserinterface
	 * @return Returns a String
	 */
	public String getIsbrowserinterface() {
		return isbrowserinterface;
	}
	/**
	 * Sets the isbrowserinterface
	 * @param isbrowserinterface The isbrowserinterface to set
	 */
	public void setIsbrowserinterface(String isbrowserinterface) {
		this.isbrowserinterface = isbrowserinterface;
	}

	/**
	 * Gets the iseditmode
	 * @return Returns a String
	 */
	public String getIseditmode() {
		return this.getTagAttributeValue("iseditmode", iseditmode, null);
	}
	/**
	 * Sets the iseditmode
	 * @param iseditmode The iseditmode to set
	 */
	public void setIseditmode(String iseditmode) {
		this.iseditmode = iseditmode;
	}

	/**
	 * Gets the isnewsession
	 * @return Returns a String
	 */
	public String getIsnewsession() {
		return this.getTagAttributeValue("isnewsession", isnewsession, null);
	}
	/**
	 * Sets the isnewsession
	 * @param isnewsession The isnewsession to set
	 */
	public void setIsnewsession(String isnewsession) {
		this.isnewsession = isnewsession;
	}

	/**
	 * Returns the isfalse.
	 * @return String
	 */
	public String getIsfalse() {
		return this.getTagAttributeValue("isfalse", isfalse, null);
	}

	/**
	 * Returns the istrue.
	 * @return String
	 */
	public String getIstrue() {
		return this.getTagAttributeValue("istrue", istrue, null);
	}

	/**
	 * Sets the isfalse.
	 * @param isfalse The isfalse to set
	 */
	public void setIsfalse(String isfalse) {
		this.isfalse = isfalse;
	}

	/**
	 * Sets the istrue.
	 * @param istrue The istrue to set
	 */
	public void setIstrue(String istrue) {
		this.istrue = istrue;
	}

	/**
	 * Returns the portletmode.
	 * @return String
	 */
	public String getPortletmode() {
		return this.getTagAttributeValue("portletmode", portletmode, null);
	}

	/**
	 * Sets the portletmode.
	 * @param portletmode The portletmode to set
	 */
	public void setPortletmode(String portletmode) {
		this.portletmode = portletmode;
	}

	/**
	 * Returns the hasprofile.
	 * @return String
	 */
	public String getHasprofile() {
		return this.getTagAttributeValue("hasprofile", this.hasprofile, null);
	}

	/**
	 * Sets the hasprofile.
	 * @param hasprofile The hasprofile to set
	 */
	public void setHasprofile(String hasprofile) {
		this.hasprofile = hasprofile;
	}

	/**
	 * @return
	 */
	public String getIsdefined() {
		return this.getTagAttributeValue("isdefined", isdefined, null);
	}

	/**
	 * @param string
	 */
	public void setIsdefined(String string) {
		isdefined = string;
	}

	/**
	 * @return
	 */
	public String getLanguage() {
		return getTagAttributeValue("language", language, null);
	}

	/**
	 * @param string
	 */
	public void setLanguage(String string) {
		language = string;
	}



    /**
     * @return the isempty
     */
    public String getIsempty() {
        return getTagAttributeValue("isempty", isempty, null);
    }



    /**
     * @param isempty the isempty to set
     */
    public void setIsempty(String isempty) {
        this.isempty = isempty;
    }



    /**
     * @return the isfilled
     */
    public String getIsfilled() {
        return getTagAttributeValue("isfilled", isfilled, null);
    }



    /**
     * @param isfilled the isfilled to set
     */
    public void setIsfilled(String isfilled) {
        this.isfilled = isfilled;
    }

    
	public String getIsfirstloop() {
		return getTagAttributeValue("isfirstloop", isfirstloop, null);
	}


	public void setIsfirstloop(String isfirstloop) {
		this.isfirstloop = isfirstloop;
	}



	public String getIslastloop() {
		return getTagAttributeValue("islastloop", islastloop, null);
	}


	public void setIslastloop(String islastloop) {
		this.islastloop = islastloop;
	}
	
	public String getHasroles() {
	   return getTagAttributeValue("hasroles", hasroles, null);
	}

    public void setHasroles(String hasroles) {
        this.hasroles = hasroles;
    }

    public String getHasgroups() {
        return getTagAttributeValue("hasgroups", hasgroups, null);
    }

    public void setHasgroups(String hasgroups) {
        this.hasgroups = hasgroups;
    }

    public String getIsanonymous() {
        return getTagAttributeValue("isanonymous", isanonymous, null);
    }

    public void setIsanonymous(String isauthenticated) {
        this.isanonymous = isauthenticated;
    }

    public String getHasoptions() {
        return getTagAttributeValue("hasoptions", hasoptions, null);
    }

    public void setHasoptions(String hasoptions) {
        this.hasoptions = hasoptions;
    }
}
