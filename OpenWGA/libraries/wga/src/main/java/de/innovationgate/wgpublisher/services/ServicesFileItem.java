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

package de.innovationgate.wgpublisher.services;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import javax.activation.DataSource;

import org.apache.commons.fileupload.FileItem;

import de.innovationgate.utils.WGUtils;

public class ServicesFileItem implements FileItem {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private DataSource _att;
	private String _name;

    public ServicesFileItem(String name, DataSource data) {
        _att = data;
        _name = name;
    }

    public InputStream getInputStream() throws IOException {
        return _att.getInputStream();
    }

    public String getContentType() {
        return _att.getContentType();
    }

    public String getName() {
       return _name;
    }

    public boolean isInMemory() {
        return false;
    }

    public long getSize() {
        return -1;
    }

    public byte[] get() {
    	ByteArrayOutputStream out = new ByteArrayOutputStream();
    	try {
			WGUtils.inToOut(_att.getInputStream(), out, 1024);
		} catch (IOException e) {
			return null;
		}
    	return out.toByteArray();
    }

    public String getString(String arg0) throws UnsupportedEncodingException {
        return null;
    }

    public String getString() {
        return null;
    }

    public void write(File arg0) throws Exception {
        FileOutputStream out = new FileOutputStream(arg0);
        WGUtils.inToOut(getInputStream(), out, 1024);
        out.close();
    }

    public void delete() {
    }

    public String getFieldName() {
        return _name;
    }

    public void setFieldName(String name) {
        _name = name;
    }

    public boolean isFormField() {
        return false;
    }

    public void setFormField(boolean arg0) {
    }

    public OutputStream getOutputStream() throws IOException {
        return null;
    }

}
