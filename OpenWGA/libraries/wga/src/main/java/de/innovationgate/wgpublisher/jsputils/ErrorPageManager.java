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

package de.innovationgate.wgpublisher.jsputils;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;

import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGAError;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;

public class ErrorPageManager {
    
    private PageContext _pageContext;
    private boolean _displayDefaultErrorPage = true;
    private WGAError _error;
    private WGA _wga;
    private JspHelper _jspHelper;

    public WGA getWga() {
        return _wga;
    }

    public ErrorPageManager(PageContext pageContext) throws ServletException, IOException {
        _pageContext = pageContext;
        HttpServletRequest request = (HttpServletRequest) _pageContext.getRequest();
        HttpServletResponse response = (HttpServletResponse) _pageContext.getResponse();
        Throwable exception = _pageContext.getException();
        
        _jspHelper = new JspHelper(pageContext);
        _wga = WGA.get(_jspHelper); 
        _jspHelper.setContentType("text/html");

        Throwable excAttribute = (Throwable) request.getAttribute(de.innovationgate.wgpublisher.WGACore.ATTRIB_EXCEPTION);

        if (excAttribute == null) {
            String outerDesign = (String) request.getAttribute(de.innovationgate.wgpublisher.WGACore.ATTRIB_OUTER_DESIGN);
            if (exception != null) {
                _error = new de.innovationgate.wgpublisher.WGAError(exception, outerDesign, request);
            }
            else {
                _error = new de.innovationgate.wgpublisher.WGAError(new HttpErrorException(request), outerDesign, request);
            }
        }
        else {
            _error = new de.innovationgate.wgpublisher.WGAError(excAttribute, (String) request.getAttribute(de.innovationgate.wgpublisher.WGACore.ATTRIB_OUTER_DESIGN), request);
        }
        
        request.setAttribute(de.innovationgate.wgpublisher.WGACore.ATTRIB_WGAERROR, _error);
        
        if (excAttribute instanceof HttpErrorException && !response.isCommitted()) {
            HttpErrorException exc = (HttpErrorException) excAttribute;
            response.setStatus(exc.getCode());
        }
               
        if (request.getParameter("$ajaxInfo") != null  && !response.isCommitted()) {
            response.reset();
            response.setContentType("text/html");
            request.getRequestDispatcher("/ajaxError.jsp").forward(request, response);
            _displayDefaultErrorPage = false;
        }
        else if (_wga.getCore().getDispatcher().dispatchErrorTmlRequest(_error, request, response)) {
            _displayDefaultErrorPage = false;
        }
        else if (_jspHelper.getCore().getWgaConfiguration().isCustomErrorPageEnabled() && _jspHelper.getCore().getErrorPage() != null) {
            response.reset();
            response.setContentType("text/html");
            request.getRequestDispatcher(_jspHelper.getCore().getErrorPage()).forward(request, response);
            _displayDefaultErrorPage = false;
        }
    }

    public boolean isDisplayDefaultErrorPage() {
        return _displayDefaultErrorPage;
    }

    public WGAError getError() {
        return _error;
    }

    public JspHelper getJspHelper() {
        return _jspHelper;
    }
    


}
