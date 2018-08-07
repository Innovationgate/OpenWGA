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

package de.innovationgate.wga.server.api;

import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wga.server.api.tml.Portlet;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.expressions.tmlscript.FunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class WebTMLFunctionArgumentSubstitutor implements FunctionArgumentSubstitutor {

    private WGA _wga;

    public WebTMLFunctionArgumentSubstitutor(WGA wga) {
        _wga = Unlocker.unlock(wga);
    }

    @Override
    public Object getArgumentValue(String argumentName) throws WGException {

        if (argumentName.startsWith("$")) {
            return returnDollarArgument(argumentName);
        }
        
        return null;
        
        
    }

    private Object returnDollarArgument(String argumentName) throws WGException, WGAPIException {
        if (argumentName.equals("$cx")) {
            if (_wga.isTMLContextAvailable()) {
                return _wga.tmlcontext();
            }
        }
        else if (argumentName.equals("$mainCx")) {
            if (_wga.isTMLContextAvailable()) {
                return ((TMLContext) _wga.tmlcontext()).getmaincontext();
            }
        }
        else if (argumentName.equals("$form")) {
            if (_wga.isTMLContextAvailable()) {
                return _wga.tmlcontext().gettmlform();
            }
        }
        else if (argumentName.startsWith("$field_")) {
            if (_wga.isTMLContextAvailable()) {
                Form form = _wga.tmlcontext().gettmlform();
                if (form != null) {
                    return form.field(argumentName.substring(7));
                }
            }
        }
        else if (argumentName.startsWith("$fieldList_")) {
            if (_wga.isTMLContextAvailable()) {
                Form form = _wga.tmlcontext().gettmlform();
                if (form != null) {
                    return form.fieldlist(argumentName.substring(11));
                }
            }
        }
        else if (argumentName.startsWith("$item_")) {
            if (_wga.isTMLContextAvailable()) {
                return _wga.tmlcontext().item(argumentName.substring(6));
            }
        }
        else if (argumentName.equals("$request")) {
            if (_wga.isTMLContextAvailable()) {
                return _wga.tmlcontext().getrequest();
            }
        }
        else if (argumentName.startsWith("$request_")) {
            if (_wga.isTMLContextAvailable()) {
                String requestMeta = argumentName.substring(9);
                return _wga.tmlcontext().meta("request", requestMeta);
            }
        }
        
        else if (argumentName.startsWith("$pageVar_")) {
            if (_wga.tmlPage().isAvailable()) {
                return _wga.tmlPage().getVar(argumentName.substring(9));
            }
        }
        else if (argumentName.startsWith("$itemList_")) {
            if (_wga.isTMLContextAvailable()) {
                return _wga.tmlcontext().itemlist(argumentName.substring(10));
            }
        }
        else if (argumentName.equals("$portlet")) {
            if (_wga.isTMLContextAvailable()) {
                return  _wga.tmlcontext().getportlet();
            }
        }
        else if (argumentName.startsWith("$portlet_")) {
            if (_wga.isTMLContextAvailable()) {
                return  _wga.tmlcontext().getportlet().item(argumentName.substring(9));
            }
        }
        else if (argumentName.equals("$pc")) {
            if (_wga.isTMLContextAvailable()) {
            	return  _wga.tmlcontext().getportlet().getcontroller();
            }
        }
        else if (argumentName.startsWith("$pMode") || argumentName.startsWith("$portletMode")) {
            if (_wga.isTMLContextAvailable()) {
                Portlet portlet = _wga.tmlcontext().getportlet();
                if (portlet != null) {
                    return portlet.getmode();
                }
            }
        }
        else if (argumentName.startsWith("$portletCx") || argumentName.startsWith("$pCx")) {
            if (_wga.isTMLContextAvailable()) {
                Portlet portlet = _wga.tmlcontext().getportlet();
                if (portlet != null) {
                    return portlet.getcontext();
                }
            }
        }
        else if (argumentName.startsWith("$pVar_") || argumentName.startsWith("$portletVar_")) {
            Portlet portlet = _wga.tmlcontext().getportlet();
            if (portlet != null) {
                return portlet.getvar(argumentName.substring(6));
            }
        }
        else if (argumentName.startsWith("$urlParam_")) {
            if (_wga.call().isAvailable()) {
                return _wga.call().getParam(argumentName.substring(10));
                
            }
        }
        else if (argumentName.equals("$pEvent") || argumentName.equals("$portletEvent")) {
            if (_wga.isTMLContextAvailable()) {
                return ((TMLContext) _wga.tmlcontext()).fetchRequestPortletEvent();
            }
            else {
                return null;
            }
        }
        else if (argumentName.startsWith("$pEventParam_") || argumentName.startsWith("$portletEventParam_")) {
            if (_wga.isTMLContextAvailable()) {
                PortletEvent event = ((TMLContext) _wga.tmlcontext()).fetchRequestPortletEvent();
                if (event != null) {
                    return event.getParameter(argumentName.substring(13));
                }
            }
            
            return null;
            
        }
        else if (argumentName.equals("$tag")){
            if (!_wga.isTMLContextAvailable() || !_wga.tmlcontext().iswebenvironment()) {
                return null;
            }
            return _wga.tmlcontext().tag();
        }
        else if (argumentName.startsWith("$tagInfo_") || argumentName.startsWith("$tag_")) {
            
            if (!_wga.isTMLContextAvailable() || !_wga.tmlcontext().iswebenvironment()) {
                return null;
            }
            
            List<String> parts = WGUtils.deserializeCollection(argumentName, "_");
            if (parts.size() != 3) {
                return null;
            }
            
            String tagId = parts.get(1);
            String tagInfoName = parts.get(2);
            return _wga.tmlcontext().tag(tagId).getInfo(tagInfoName);
            
        }
        else if (argumentName.startsWith("$option_")) {
            
            if (!_wga.isTMLContextAvailable()) {
                return null;
            }
            String optionName = argumentName.substring(8);
            return _wga.tmlcontext().option(optionName);
            
        }
        else if (argumentName.equals("$sessionId")) {
            if (_wga.session().isAvailable()) {
                return _wga.session().getJavaHttpSession().getId();
            }
            else {
                return null;
            }
        }
        else if (argumentName.startsWith("$cookie_")) {
            
            if (!_wga.call().isAvailable()) {
                return null;
            }
            Cookie c = _wga.call().getCookie(argumentName.substring(8));
            if (c != null) {
                return c.getValue();
            }
            else {
                return null;
            }
            
        }
        else if(argumentName.startsWith("$is_")) {

            if (!_wga.isTMLContextAvailable() || !_wga.tmlcontext().iswebenvironment()) {
                return null;
            }

        	String option = argumentName.substring(4);
        	switch (option) {
				case "selected":
					return _wga.tmlcontext().isselected();
				case "firstloop":
					return _wga.tmlcontext().isfirstloop();
				case "lastloop":
					return _wga.tmlcontext().islastloop();
				case "root":
					return _wga.tmlcontext().isroot();
				case "maindocument":
					return _wga.tmlcontext().ismaindocument();
				case "browserinterface":
					return _wga.tmlcontext().isbrowserinterface();
				case "anonymous":
					return _wga.tmlcontext().isanonymous();
				default:
					return null;
			}
        }
        
        return null;
    }


}
