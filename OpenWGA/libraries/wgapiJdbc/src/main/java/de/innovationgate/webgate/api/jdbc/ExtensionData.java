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

import java.io.File;
import java.util.Date;

import de.innovationgate.webgate.api.BinaryFieldData;

public class ExtensionData extends Entity implements Item {
    
    private Entity entity;
    private String name;
    private int type;
    private String text;
    private Double number;
    private Date date;
    private String binarySha512;
    
    private transient BinaryFieldData sourceData;
    
    public BinaryFieldData getSourceData() {
        return sourceData;
    }

    public void setSourceData(BinaryFieldData sourceData) {
        this.sourceData = sourceData;
    }
    
    /** Derived formula field */
    private Double bool; 
     
    private transient DeserialisationCache deserialisationCache;
    
    public DeserialisationCache getDeserialisationCache() {
        return deserialisationCache;
    }
    public void setDeserialisationCache(DeserialisationCache deserialisationCache) {
        this.deserialisationCache = deserialisationCache;
    }
     
    public Entity getEntity() {
        return entity;
    }
    public void setEntity(Entity entity) {
        this.entity = entity;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getType() {
        return type;
    }
    public void setType(int type) {
        this.type = type;
    }
    public String getText() {
        return text;
    }
    public void setText(String text) {
        this.text = text;
    }
    public Double getNumber() {
        return number;
    }
    public void setNumber(Double number) {
        this.number = number;
    }
    public Date getDate() {
        return date;
    }
    public void setDate(Date date) {
        this.date = date;
    }
    public Double getBoolean() {
        return bool;
    }
    public void setBoolean(Double theBoolean) {
        this.bool = theBoolean;
    }
    public String getBinarySha512() {
        return binarySha512;
    }
    public void setBinarySha512(String binarySha512) {
        this.binarySha512 = binarySha512;
    }

}
