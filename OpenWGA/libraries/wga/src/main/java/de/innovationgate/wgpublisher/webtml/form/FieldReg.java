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
package de.innovationgate.wgpublisher.webtml.form;

import java.util.List;


public class FieldReg {
		
	private String _name = null;
	private String _type = null;
	private String _format = null;
	private boolean _meta;
	private boolean _multiple;
    private String _validation = null;
    private String _message = null;
    private String _validationdivider = null;
    private boolean _trim;
    private String _mode;    
    
    // a list of Strings (fieldnames) to clear if this field has an validation error
    private List _cleariferror;
    
    // should this field be stored in storeIn... methods (default should be true)
    private boolean _store;
	private String _relationtype;

	public static String EDIT_MODE = TMLFormInfo.EDIT_MODE;
    public static String READONLY_MODE = TMLFormInfo.READONLY_MODE;
    public static String VIEW_MODE = TMLFormInfo.VIEW_MODE;


    /**
	public FieldReg(String name, String type, String format, boolean meta, boolean multiple) {
		_name = name;
		_type = type;
		_format = format;
		_meta = meta;
		_multiple = multiple;
	}**/

    public FieldReg(String name, String type, String format, boolean meta, 
            boolean multiple, String validation, String message, String validationdivider, boolean trim, String mode, List cleariferror, boolean store, String relationType) {
        _name = name;
        _type = type;
        _format = format;
        _meta = meta;
        _multiple = multiple;
        _validation = validation;
        _message = message;
        _validationdivider = validationdivider;
        _trim = trim;
        _mode = mode;
        _cleariferror = cleariferror;
        _store = store;
        _relationtype = relationType;
    }    
    
    /**
     * no-arg constructor for XStream on JRockit-VM
     *
     */
    private FieldReg() {
    }     
    
    /*
	public FieldReg(String name, String type, String format) {
		_name = name;
		_type = type;
		_format = format;
	}*/
	/*
	public FieldReg( String field ) {
		_name = field.substring(0, field.indexOf("["));
		_type = field.substring( field.indexOf("[") + 1, field.indexOf("]") );
		if(field.indexOf("$meta") != -1) {
            _meta = true;
        } else {
            _meta = false;
        }
		if(field.indexOf("$multiple") != -1) {
            _multiple = true;
        } else {
            _multiple = false;
        }
		int colonPos = _type.indexOf(":");
		if (colonPos != -1) {
			_format = _type.substring(colonPos + 1);
			_type = _type.substring(0, colonPos);
		}
	}*/
    
    /**
     * merge current with given fieldReg
     * @param fieldReg
     */
    public void importFieldReg(FieldReg fieldReg) {
        if (fieldReg.getFormat() != null && !fieldReg.getFormat().trim().equals("")) {
            this._format = fieldReg.getFormat();
        }
        /*
        if (fieldReg.getType() != null && !fieldReg.getType().trim().equals("")) {
            this._type = fieldReg.getType();
        }*/
        if (fieldReg.getValidation() != null && !fieldReg.getValidation().trim().equals("")) {
            this._validation = fieldReg.getValidation();
        }     
        if (fieldReg.getMessage() != null && !fieldReg.getMessage().trim().equals("")) {
            this._message = fieldReg.getMessage();
        }        
        if (fieldReg.getValidationdivider() != null && !fieldReg.getValidationdivider().trim().equals("")) {
            this._validationdivider = fieldReg.getValidationdivider();
        }
        /*
        if (fieldReg.getMeta() != null) {
            this._meta = fieldReg.getMeta();
        }*/
        this._trim = fieldReg.isTrim();
        this._mode = fieldReg.getMode();

        /*
        if (fieldReg.getMultiple() != null) {
            this._multiple = fieldReg.getMultiple();
        } */       
    }    
				
	/**
	 * @return
	 */
	public String getName() {
		return _name;
	}

	/**
	 * @return
	 */
	public String getType() {
		return _type;
	}
	   
    

	public boolean isMeta() {
        return _meta;
    }

    public boolean isMultiple() {
        return _multiple;
    }


    /* (Kein Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		
		StringBuffer output = new StringBuffer();
		output.append(this._name);
		output.append("[").append(this._type);
		if (_format != null) {
			output.append(":").append(this._format);		
		}
		output.append("]");
		if(isMeta()){
			output.append("$meta");
		}
		if(isMultiple()){
			output.append("$multiple");
		}
		return output.toString();
		
	}

	public String getFormat() {
		return _format;
	}

    public String getValidation() {
        return _validation;
    }

    public void setValidation(String validation) {
        _validation = validation;
    }

    public String getMessage() {
        return _message;
    }

    public void setMessage(String message) {
        _message = message;
    }

    public String getValidationdivider() {
        return _validationdivider;
    }

    public void setValidationdivider(String validationdivider) {
        _validationdivider = validationdivider;
    }

      
    
    public boolean hasValidation() {
        if (_validation != null && !_validation.trim().equals("")) {
            return true;
        } else {
            return false;
        }
    }

    
    public String getMode() {
        return _mode;
    }

    public void setMode(String mode) {
        _mode = mode;
    }

    public boolean isTrim() {
        return _trim;
    }

    public void setTrim(boolean trim) {
        _trim = trim;
    }

    public void setMeta(boolean meta) {
        _meta = meta;
    }

    public void setMultiple(boolean multiple) {
        _multiple = multiple;
    }
    public List getCleariferror() {
        return _cleariferror;
    }
    public void setCleariferror(List cleariferror) {
        _cleariferror = cleariferror;
    }

    public boolean isStore() {
        return _store;
    }

    public void setStore(boolean store) {
        _store = store;
    }

    
    public String getRelationtype() {
		return _relationtype;
	}

}
