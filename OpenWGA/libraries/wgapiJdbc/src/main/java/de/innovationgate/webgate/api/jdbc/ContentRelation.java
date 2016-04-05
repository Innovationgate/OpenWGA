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
package de.innovationgate.webgate.api.jdbc;



public class ContentRelation extends Entity implements java.io.Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String name;
    private String targetstructentry;
    private String targetlanguage;
    private Content parentcontent;
    private Content target;
    private int type;
    private String group;
     
     /** default constructor */
    public ContentRelation() {
    }

	/** minimal constructor */
    public ContentRelation(Content parentcontent, String name, String targetstructentry) {
        this.parentcontent = parentcontent;
        this.name = name;
        this.targetstructentry = targetstructentry;
    }
    
    /** full constructor */
    public ContentRelation(Content parentcontent, String name, String targetstructentry, String targetlanguage, Content targetcontent) {
        this.parentcontent = parentcontent;
        this.name = name;
        this.targetstructentry = targetstructentry;
        this.targetlanguage = targetlanguage;
        this.parentcontent = parentcontent;
        this.target = targetcontent;
    }

   
    // Property accessors

    public Content getParentcontent() {
        return this.parentcontent;
    }
    
    public void setParentcontent(Content parentcontent) {
        this.parentcontent = parentcontent;
    }

    public String getName() {
        return this.name;
    }
    
    public void setName(String name) {
        this.name = name;
    }

    public String getTargetstructentry() {
        return this.targetstructentry;
    }
    
    public void setTargetstructentry(String targetstructentry) {
        this.targetstructentry = targetstructentry;
    }

    public String getTargetlanguage() {
        return this.targetlanguage;
    }
    
    public void setTargetlanguage(String targetlanguage) {
        this.targetlanguage = targetlanguage;
    }

    public Content getTarget() {
        return this.target;
    }
    
    public void setTarget(Content targetcontent) {
        this.target = targetcontent;
    }

    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((parentcontent == null) ? 0 : parentcontent.hashCode());
        result = prime * result + ((targetlanguage == null) ? 0 : targetlanguage.hashCode());
        result = prime * result + ((targetstructentry == null) ? 0 : targetstructentry.hashCode());
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final ContentRelation other = (ContentRelation) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        }
        else if (!name.equals(other.name))
            return false;
        if (parentcontent == null) {
            if (other.parentcontent != null)
                return false;
        }
        else if (!parentcontent.equals(other.parentcontent))
            return false;
        if (targetlanguage == null) {
            if (other.targetlanguage != null)
                return false;
        }
        else if (!targetlanguage.equals(other.targetlanguage))
            return false;
        if (targetstructentry == null) {
            if (other.targetstructentry != null)
                return false;
        }
        else if (!targetstructentry.equals(other.targetstructentry))
            return false;
        return true;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getGroup() {
        return group;
    }

    public void setGroup(String group) {
        this.group = group;
    }
   








}
