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

import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Iterator;

import org.dom4j.Element;
import org.dom4j.Node;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;

public class TagOutputFormatter implements ObjectFormatter {
	
	private String format = null;
    private TMLContext context;
    private boolean trim;
	
	public TagOutputFormatter(String format, TMLContext context, boolean trim) {
		this.format = format;
        this.context = context;
        this.trim = trim;
	}
    
    public TagOutputFormatter(TMLContext context) {
        this.format = null;
        this.context = context;
    }    
	
	private DateFormat getDateFormatter() throws WGException {
        return WGA.get(context).getDateFormat(format, null);
	}
	
	private NumberFormat getNumberFormatter() throws WGException {
        return WGA.get(context).getNumberFormat(format, null);
	}
	
	public String format(Object obj) throws FormattingException {
		
        try {
            if (obj == null) {
                return "";
            }
            else if (obj instanceof java.util.Date) {
                return this.getDateFormatter().format((java.util.Date) obj);
            }
            else if (obj instanceof Number) {
                return this.getNumberFormatter().format((Number) obj);
            }
            else if (obj instanceof Element) {
                Element element  = (Element) obj;
                if (element.hasMixedContent()) {
                    Iterator nodes = element.content().iterator();
                    Node node;
                    StringBuffer output = new StringBuffer();
                    while (nodes.hasNext()) {
                        node = (Node) nodes.next();
                        output.append(node.asXML());
                    }
                    return (trim ? output.toString().trim() : output.toString());
                }
                else {
                    return (trim ? element.getStringValue().trim() : element.getStringValue());
                }
                
            }
            else if (obj instanceof Node) {
                return ((Node) obj).getStringValue();
            }
            else {
               return (trim ?  obj.toString().trim() :  obj.toString());
            }
        }
        catch (WGException e) {
            throw new FormattingException("Exception formatting tag", e);
        }

	}
}

