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

package de.innovationgate.wga.config;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Root;

@Root(strict=false)
public class VirtualHostRedirect {
    
    @Attribute
    @NotNull
    private String path;

    @Attribute
    @NotNull
    private String redirect;
    

    @Attribute (required=false)
    private boolean enabled=true;

    @Attribute (required=false)
    private boolean forward=false;

    @Attribute (required=false)
    private boolean permanentRedirect=false;	// use 301 instead of 302

    public VirtualHostRedirect() {
    	forward=false;	// default
    	permanentRedirect=false;
    }
    
    public String getRedirect() {
        return redirect;
    }

    public void setRedirect(String redirect) {
        this.redirect = redirect;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }
    
    public void setForward(boolean forward){
    	this.forward = forward;
    }
    public boolean isForward(){
    	return forward;
    }
          
    public void setEnabled(boolean enabled){
    	this.enabled = enabled;
    }
    public boolean isEnabled(){
    	return enabled;
    }

    public void setPermanentRedirect(boolean value){
    	this.permanentRedirect = value;
    }
    public boolean isPermanentRedirect(){
    	return permanentRedirect;
    }
}
