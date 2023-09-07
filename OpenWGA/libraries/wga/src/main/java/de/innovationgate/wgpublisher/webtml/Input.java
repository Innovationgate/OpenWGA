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

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.tagext.DynamicAttributes;

import org.apache.commons.jxpath.JXPathContext;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSet;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Relation;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.webtml.FormBase.FormStatus;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.FieldReg;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TagOutputFormatter;

public class Input extends ActionBase implements DynamicAttributes {
    
    private static final long serialVersionUID = 1L;

    /**
     * represents an HTML-Option (for example in an HTML-selectbox, an HTML-checkbox or an HTML-option
     */
    private class InputOption {

        private String _value;
        private String _text;
        private boolean _disabled=false;
        
        public InputOption(String text, String value) {
        	_value = value;
        	int pos = value.indexOf("$disabled");        	
        	if(pos>=0){
        		_value = value.substring(0, pos);
        		_disabled = true;
        	}
            _text = text;
        }

        public String getText() {
            return _text;
        }

        public String getValue() {
            return _value;
        }
        
        public boolean isDisabled(){
        	return _disabled;
        }
        
    }

	private String type;
	private String name;
	private String options;
    private String optionsitem;
    private String optgroupsitem;
	private String multiple;
	private String cssclass;
	private String cssstyle;
	private String changeaction;
	private String meta;
	private String defaultvalue;
    private String validation;
    private String message;
    private String validationdivider;
    private String mode;
    private String onupdate;
    private String focus;
    private String encodeoptions;
    private String defaultexpression;
    private String labeled;
    private String optionstitle;
    private String sortoptions;
    
    // a comma separated list of fieldnames to clear if this field has an validationerror
    private String cleariferror;
    
    // should this input field be stored in storeIn... methods
    private String store;

    // type of content relation to create instead of using an item
    private String relationtype;

    public boolean isMultipleInput() {
        
        String type = this.getType();
        if (type.equals("text") || 
            type.equals("number") || 
            type.equals("date") || 
            type.equals("radio") || 
            type.equals("boolean")) {
            return false;
        }
        else if (type.equals("checkbox")) {
            return true;
        }
        else {
            return stringToBoolean(getMultiple());
        }
        
    }
    
    public void tmlEndTag() throws WGException {
        // Retrieve basic parameters
        String type = this.getType();
        String name = this.getName();
        String format = getStatus().format;
        if (format != null && WGUtils.isEmpty(format)) {
            format = null;
        }
        
        Object defaultvalue = this.getDefault();
        if (defaultvalue == null) {
        	String expression = getDefaultexpression();
			if (expression != null) {
				ExpressionEngine engine = ExpressionEngineFactory.getEngine(getDefaultExpressionLanguage());
				if (engine == null) {
					this.addWarning("Unknown expression language: " + getDefaultExpressionLanguage(), true);
				}
				
				
				Map<String,Object> objects = new HashMap<String,Object>();
				objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "DefaultExpression on " + getTagDescription());
				ExpressionResult result = engine.evaluateExpression(expression, this.getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, objects);
				if (result.isError()) {
				    addExpressionWarning(expression,result);
				}
				else {
					defaultvalue = result.getResult();
				}
			}
			
			// If no default value determined we must see if we have an input type with "implicit" default value (#00000263)
			else {
			    /* Deactivated as of #00000366 because it broke more than it saved
			    if (type.equals("select")) {
			        List<InputOption> options = retrieveInputOptions();
			        if (options.size() > 0) {
			            defaultvalue = options.get(0).getValue();
			        }
			    }
			    else */
			    if (type.equals("boolean")) {
			        defaultvalue = Boolean.FALSE;
			    }
			}
        }
        
        
        boolean readonlyMode = false;
        String computedMode = getMode();
        
        String cssClass = this.getCssclass();
        if (cssClass != null) {
            cssClass = "class=\"" + cssClass + "\" ";
        }

        String cssStyle = this.getCssstyle();
        if (cssStyle != null) {
            cssStyle = "style=\"" + cssStyle + "\" ";
        }        
        
        // Register with form (or item for BI-CustomEditor) parent (if present) and retrieve item values from it;
        FormInputRegistrator formBase = (FormInputRegistrator) getStatus().getAncestorTag(FormBase.class);
        
        if (formBase == null && isAjaxRequest()) {
        	// try to retrieve form from ajax call
        	final TMLForm form = getTMLContext().gettmlform();
        	if (form != null) {
        		formBase = new AjaxFormInputRegistrator(form);
        	}
        }
        
        List<Object> values;
        if (formBase != null) {
            // retrieve values from inputRegistrator
        	boolean useRelation = false;
        	if (getRelationtype() != null) {
        		useRelation = true;
        	}
        	
            values = formBase.getFieldValue(name, stringToBoolean(getMeta()), defaultvalue, useRelation);
 
            // compute mode
            if (formBase.getFormMode().equals(TMLFormInfo.VIEW_MODE)) {
                if (this.getMode().equals(FieldReg.EDIT_MODE)) {
                    computedMode = FieldReg.VIEW_MODE;
                }
                else {
                    computedMode = getMode();
                }
            }
            else if (formBase.getFormMode().equals(TMLFormInfo.READONLY_MODE)) {
                if (this.getMode().equals(FieldReg.EDIT_MODE)) {
                    computedMode = FieldReg.READONLY_MODE;
                }
                else {
                    computedMode = getMode();
                }
            }
            else {
                computedMode = getMode();
            }

            //build clear if errorlist
            List<String> cleariferrorList = new ArrayList<String>();
            String cleariferror = getCleariferror();
            if (cleariferror != null) {
                cleariferrorList = WGUtils.deserializeCollection(cleariferror, ",");
            }
            
            formBase.addField(new FieldReg(name, type, format, stringToBoolean(getMeta()), isMultipleInput(), getValidation(), getMessage(), getValidationdivider(),
                    stringToBoolean(getTrim()), computedMode, cleariferrorList ,stringToBoolean(getStore()), getRelationtype()), values);

            if (computedMode.equals(TMLFormInfo.EDIT_MODE)) {
                // nothing to do
            }
            else if (computedMode.equals(TMLFormInfo.VIEW_MODE)) {
                // clear results
                this.clearResult();
                // display optionText not optionValues
                
                if (type.equals("boolean")) {
                	String ret="";
                    List<InputOption> opts = this.retrieveInputOptions();
                	
                    Boolean boolValue = Boolean.FALSE;
                    if (values.size() >= 1) {
                        Object theValue = values.get(0);
                        if (theValue instanceof String) {
                            boolValue = Boolean.valueOf((String) theValue);
                        }
                        else if (theValue instanceof Boolean) {
                            boolValue = (Boolean) theValue;
                        }
                    }
                    
                    if(opts.size()==0){
                    	getStatus().encode = "none";
                    	ret = "<img align=\"bottom\" src=\"" + getWGPPath() + "/static/images/" + boolValue.toString() + ".png\"> ";
                    }
                    else if(opts.size()==1){
                    	getStatus().encode = "none";
                        ret = "<span";
                        if(!boolValue)
                        	ret += " style=\"text-decoration:line-through\"";
                        ret += ">";
                		InputOption opt = opts.get(0);
                		ret += opt.getText();
                		ret += "</span>";
                	}
                    else{
                        Iterator<InputOption> options = opts.iterator();
                        while (options.hasNext()) {
                            InputOption option = (InputOption) options.next();
                            List<String> stringValues = WGUtils.toString(values);
                            if (stringValues.contains(option.getValue())) {
                                ret = option.getText();
                                break;
                            }
                        }
                    }
                	
                	this.setResult(ret);
                }
                
                else if (type.equals("select") || type.equals("checkbox") || type.equals("radio")) {
                    List<String> textValues = new ArrayList<String>();
                    
                    List<InputOption> options = this.retrieveInputOptions(true);
                    if(options.size()>0){
                        // prepare regular list so we can use WGA.alias()
                    	ArrayList<String> optionsValues = new ArrayList<String>();
                    	String relationNullPlaceholderOptionValue=null;
                    	for(InputOption o: options){
                    		optionsValues.add(o.getText()+"|"+o.getValue());
                    		if(TMLForm.RELATION_NULLPLACE_HOLDER.equals(o.getValue())){
                    			relationNullPlaceholderOptionValue = o.getText();
                    		}
                    	}
                    	if(values.size()==0 && relationNullPlaceholderOptionValue!=null){
                			textValues.add(relationNullPlaceholderOptionValue);
                		}
                    	else{
	                    	WGA wga = WGA.get();
	                        List<String> stringValues = WGUtils.toString(values);
	                        textValues.addAll(wga.aliases(stringValues, optionsValues, true));
                    	}
                        this.setResult(textValues);
                        getStatus().divider = getMultiValueDivider();
                    }
                    else {
                        this.setResult(values);
                    }

                }
                else {
                    // do not render original value if type is password
                    if (this.getType().equals("password")) {
                        values = Collections.<Object>singletonList("********");
                    }
                    // if type is hashedpassword show "*" instead of hashed
                    // value in viewmode
                    if (this.getType().equals("hashedpassword")) {
                        values = Collections.<Object>singletonList("********");
                    }

                    // Set default divider for multivalue textarea fields
                    if (type.equals("textarea") && isMultipleInput()) {
                        getStatus().divider = getMultiValueDivider();
                    }
                    // do not display hidden values in VIEW_MODE
                    if (!this.getType().equals("hidden")) {
                        this.setResult(values);
                    }
                }
                return;
            }
            else if (computedMode.equals(TMLFormInfo.READONLY_MODE)) {
                readonlyMode = true;
            }
            else {
                this.addWarning("Unsupported mode: " + this.getMode(), true);
            }     
        }
        else {
        	values = new ArrayList<Object>();
        	String[] params = this.getTMLContext().getrequest().getParameterValues(name);
        	if(params!=null){        		
        		for(String param: params)
        			values.add(param);                
        	}
        	else values.add(defaultvalue);
        }
                  
        // Disable encoding, since this tag is in edit or readonly mode
        getStatus().encode = "none";
        
        String disabledString = "";
        if (!computedMode.equals(FieldReg.EDIT_MODE)) {
            disabledString = "disabled ";
        }        
        
        //this.setBlockDivider(true);
        
        if (values == null) {
            values = new ArrayList<Object>();
        }
        
        Object singleValue = (values.size() == 0 ? "" : values.get(0));
        
        String tagContent = this.getResultString(false);
        TMLForm form = getTMLContext().gettmlform();
        if(form!=null && form.hasmessage(getName()))
        		tagContent += " data-error=\"true\"";
        this.clearResult();
        
        // Render
        try {
            if (type.equals("text") || type.equals("password")) {
                renderSimpleInput(type, name, format, cssClass, cssStyle, singleValue, tagContent, disabledString);
            }   
            else if(type.equals("hidden")) {
            	if(isMultipleInput()){
            		String renderedValue = WGUtils.serializeCollection(values, "~~~");
            		renderSimpleInput(type, name, format, cssClass, cssStyle, renderedValue, tagContent, disabledString);
            	}
            	else renderSimpleInput(type, name, format, cssClass, cssStyle, singleValue, tagContent, disabledString);
            }
            else if(type.equals("boolean")) {
                renderBoolean( name, cssClass, cssStyle, formBase, singleValue, tagContent, disabledString);
            }   
            else if(type.equals("date")) {
            	if (!hasInputOptions()) {
            		renderDateInput(name, format, cssClass, cssStyle, singleValue, tagContent, disabledString);
            	} else {
            		renderSelectInput(name, cssClass, cssStyle, formBase, values, tagContent, disabledString, readonlyMode, format);
            	}
            } else if (type.equals("number")) {
            	if (!hasInputOptions()) {
            		renderSimpleInput("text", name, format, cssClass, cssStyle, singleValue, tagContent, disabledString);
            	} else {
            		renderSelectInput(name, cssClass, cssStyle, formBase, values, tagContent, disabledString, readonlyMode, format);
            	}
            }
            else if(type.equals("textarea")) {
                boolean multipleInput = isMultipleInput();
                Object renderValue = (multipleInput ? values : singleValue);
                renderTextArea( name, format, cssClass, cssStyle, renderValue, tagContent, isMultipleInput(), disabledString );
                
            }
            else if(type.equals("checkbox") || type.equals("radio")) {
                renderOptionInput(type, name, cssClass, cssStyle, formBase, values, tagContent, disabledString);
            }
            else if(type.equals("select")) {
                renderSelectInput(name, cssClass, cssStyle, formBase, values, tagContent, disabledString, readonlyMode, null);
            }
            else if( type.equals("file") ) {
                renderFileInput(type, name, cssClass, cssStyle, values, tagContent, disabledString);
            }
            else if ( type.equals("hashedpassword") ) {
                renderHashedPassword(name, cssClass, cssStyle, singleValue, tagContent, disabledString);
            }
            else {
                this.addWarning("Unknown input type:" + type, true);
                return;
            }
        }
        catch (FormattingException e) {
            throw new TMLException("Exception formatting input field", e, true);
        }
        
        if(formBase!=null && this.getFocus().equals("true")){
        	this.appendResult("<script>WGA.focus(document.forms['" + formBase.getId() + "'].elements['"+this.getName()+"'])</script>");
        }
        
        getStatus().divider = "";
        
    }    
    
    private void renderHashedPassword(String name, String cssClass, String cssStyle, Object singleValue, String tagContent, String disabled) throws FormattingException, WGException {                
        String formattedValue = new TagOutputFormatter(getTMLContext()).format(singleValue);
        // if there we got a value from source, the value is already hashed
        // register field on form to ensure not to hash twice
        if (formattedValue != null && !formattedValue.trim().equals("")) {
            FormStatus formBase = (FormStatus) getStatus().getAncestorTag(FormBase.class);
            formBase.registerHashedPasswordField(name, formattedValue);
        }
        
        this.appendResult("<input").appendResult(buildDynamicHtmlAttributes()).appendResult(" name=\"").appendResult(name).appendResult("\" type=\"password\" ");
        String theId = getId();
        if (theId!= null) {
            this.appendResult("id=\"" + theId + "\" ");
        }
        this.appendResult(" value=\"").appendResult(WGUtils.encodeHTML(formattedValue)).appendResult("\" ").appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(" />");                
    }

    private List<InputOption> retrieveInputOptions() throws WGAPIException {
    	return retrieveInputOptions(false);
    }
    private List<InputOption> retrieveInputOptions(boolean include_optgroups) throws WGAPIException {
        
        // Fetch options. Possible sources: 1. directly from item (Attribute optionsitem) 2. comma-separated string (Attribute options) 3. HDBModel relation targets
        String optionsItem = getOptionsitem();
        List<String> rawOptionsList = null;
        List<OptGroup> optgroups = null;
        if(include_optgroups)
        	optgroups = retrieveOptGroups();
                
        if (!WGUtils.isEmpty(optionsItem)) {
            rawOptionsList = WGUtils.toString(getTMLContext().itemlist(optionsItem));
        }
        else {
            String strOptions = this.getOptions();
            if (!WGUtils.isEmpty(strOptions)) {
            	if(getTMLContext().getDesignContext().getVersionCompliance().isAtLeast(7, 10))
            		rawOptionsList = WGUtils.deserializeCollection(strOptions, ",", true, '`', true);
            	else rawOptionsList = WGUtils.deserializeCollection(strOptions, ",");
            }
            else {
                String relType = getRelationtype();
                if (!WGUtils.isEmpty(relType)) {
                    String titleExpr = getOptionstitle();
                    if (titleExpr == null) {
                        titleExpr = "TITLE";
                    }
                    rawOptionsList = getHdbmodelRelationOptions(titleExpr);
                }
            }
        }
        
        if (rawOptionsList==null && optgroups==null) {
            return new ArrayList<InputOption>();
        }
        
        // Process raw options. Divide up value and text and create InputOption objects by them
        Iterator<String> rawOptions = rawOptionsList.iterator();
        List<InputOption> optionList = new ArrayList<InputOption>();
        String optionText;
        String optionValue;
        String rawOption;
        while (rawOptions.hasNext()) {
            rawOption = rawOptions.next();
            if (rawOption == null) {
                continue;
            }
            
            int divider = rawOption.lastIndexOf("|");
            if (divider != -1) {
                optionText = rawOption.substring(0, divider).trim();
                optionValue = rawOption.substring(divider + 1).trim();
            }
            else {
                optionText = evaluateOptionTitle(rawOption.trim());
                optionValue = rawOption.trim();
            }
            optionList.add(new InputOption(optionText, optionValue));
        }
        
        // Optionally sort options by their title
        String sort = getSortoptions();
        if (sort != null) {
            
            boolean byValue = false;
            boolean desc = false;
            List<String> sortParams = WGUtils.deserializeCollection(sort, ",");
            if (sortParams.size() >= 2) {
                byValue = "value".equalsIgnoreCase(sortParams.get(0));
                desc = "desc".equalsIgnoreCase(sortParams.get(1));
            }
            else {
                desc = "desc".equalsIgnoreCase(sortParams.get(0));
            }
            
            Comparator<InputOption> comparator;
            if (byValue) {
                comparator = new Comparator<InputOption>() {
                    @Override
                    public int compare(InputOption o1, InputOption o2) {
                        return Collator.getInstance().compare(o1.getValue(), o2.getValue());
                    }
                };
            }
            else {
                comparator = new Comparator<InputOption>() {
                    @Override
                    public int compare(InputOption o1, InputOption o2) {
                        return Collator.getInstance().compare(o1.getText(), o2.getText());
                    }
                };
            }
            Collections.sort(optionList, comparator);
            
            if (desc) {
                Collections.reverse(optionList);
            }
            
        }

        if(optgroups!=null){
        	// add all optgroups
        	for(OptGroup optgroup: optgroups){
        		optionList.addAll(optgroup.getOptions());
        	}
        }
        
        return optionList;
    }
    
    private String evaluateOptionTitle(String value) throws WGAPIException {
        
        String titleExpr = getOptionstitle();
        if (titleExpr == null) {
            return value;
        }
        
        RhinoExpressionEngine rhinoEngine = ExpressionEngineFactory.getTMLScriptEngine();
        getTMLContext().setvar("$O_VALUE", value);
        
        ExpressionResult result = rhinoEngine.evaluateExpression(titleExpr, getTMLContext(), RhinoExpressionEngine.TYPE_EXPRESSION, new HashMap<String, Object>());
        if (!result.isError()) {
            return String.valueOf(result.getResult());
        }
        else {
            getTMLContext().getlog().error("Exception evaluating title of option value '" + value + "'", result.getException());
            addWarning("Exception evaluating title for option value '" + value + "': " + result.getException().getClass().getName(), false);
            return value;
        }
        
    }

    private boolean hasInputOptions() throws WGAPIException {
    	return retrieveInputOptions().size() > 0;
    }
    
	private void renderFileInput(String type, String name, String cssClass, String cssStyle, List<Object> values, String tagContent, String disabled) throws WGException {
			// Build html
			this.appendResult("<input").appendResult(buildDynamicHtmlAttributes()).appendResult(" type=\"").appendResult(type).appendResult("\" name=\"").appendResult(name).appendResult("\" ");
			this.appendResult(" value=\"").appendResult("\" ");
			if (isMultipleInput()) {
				appendResult(" multiple ");
			}
			this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(">").appendResult("<br/>");
	}

	private void renderSelectInput(String name, String cssClass, String cssStyle, FormInputRegistrator form, List<Object> values, String tagContent, String disabled, boolean onlySelectedValues, String format) throws FormattingException, WGException {
		this.appendResult("<select").appendResult(buildDynamicHtmlAttributes()).appendResult(" name=\"").appendResult(name).appendResult("\" ");
		
		String theId = getId();
		if (theId!=null)
			this.appendResult("id=\"" + theId + "\" ");
		
		if (isMultipleInput()) {
			appendResult(" multiple ");
		}
		
		createChangeActionJS(name, form, "onchange");
		
		this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(" >");
		
		Iterator<InputOption> options = this.retrieveInputOptions().iterator();        
		String optionValue;
		String optionText;
		while (options.hasNext()) {
            InputOption option = options.next();
            optionValue = option.getValue();
            optionText = option.getText();
			
            if (optionValue != null && format != null) {
            	optionValue = new TagOutputFormatter(format, getTMLContext(), stringToBoolean(getTrim())).format(optionValue);
            }
            
            String displayOptionValue = optionValue;
            String displayOptionText = optionText;
            
            // Eventually encode options on output
            if (stringToBoolean(getEncodeoptions())) {
                displayOptionValue = WGUtils.encodeHTML(displayOptionValue);
                displayOptionText = WGUtils.encodeHTML(displayOptionText);
            }
            
			// Build html
            if (!onlySelectedValues) {
            	this.appendResult("<option ");
            	
            	if(option.isDisabled())
            		this.appendResult("disabled");
            	
            	if (optionValue != null) {
            		this.appendResult(" value=\"").appendResult(displayOptionValue).appendResult("\"");
            	}
    		
            	if (getType().equalsIgnoreCase("date")) {
            		Date dateValue = null;
					try {
						if (optionValue != null) {
							dateValue = getTMLContext().parsedate(optionValue, format);
						}
					} catch (Exception e) {
						addWarning("Unable to parse date for input option. Exception: " + e.getMessage());
					}
					if (dateValue != null && values.contains(dateValue)) {
	    				this.appendResult(" selected=\"true\"");
	    			}
            	} 
            	else if (getType().equalsIgnoreCase("number")) {
            		Number numberValue = null;
            		try {
	            		if (optionValue != null) {
	            			numberValue = getTMLContext().parsenumber(optionValue, format);
	            		}
            		} catch (Exception e) {
            			addWarning("Unable to parse number for input option. Exception: " + e.getMessage());
            		}
            		
            		/*
            		 * #00005673
            		 * "values" always is a list of doubles. We therefore need to convert numberValue to a double
            		 */
            		if (numberValue != null && values.contains(numberValue.doubleValue())) {
	    				this.appendResult(" selected=\"true\"");
	    			}
	    			
            	} 
            	else {
	    			if ((optionValue != null && values.contains(optionValue)) || (optionValue == null && values.contains(optionText))) {
	    				this.appendResult(" selected=\"true\"");
	    			}
            	}
    			
    			
    			this.appendResult(" >").appendResult(displayOptionText).appendResult("</option>");
            } 
            
            else  {
                if ( (values.contains(optionValue)) || (values.contains(optionText)) ) {
                    this.appendResult("<option ");
                    if (optionValue != null) {
                        this.appendResult(" value=\"").appendResult(displayOptionValue).appendResult("\"");
                    }
                    this.appendResult(" >").appendResult(displayOptionText).appendResult("</option>");
                }
            }
		}
		this.appendResult(renderOptGroup(values));
		this.appendResult("</select>");
	}

	private class OptGroup{
		private String _label;
		private List<InputOption> _options = new ArrayList<InputOption>(); 
		
		public OptGroup(String label, List<String> options) {
			_label = label;
			for(String rawOption: options){
				String optionText;
				String optionValue;
	            int divider = rawOption.lastIndexOf("|");
	            if (divider != -1) {
	                optionText = rawOption.substring(0, divider).trim();
	                optionValue = rawOption.substring(divider + 1).trim();
	            }
	            else {
	                optionText = optionValue = rawOption.trim();
	            }
	            _options.add(new InputOption(optionText, optionValue));
			}
		}
		public String getLabel(){
			return _label;
		}
		public List<InputOption> getOptions(){
			return _options;
		}
	}
	
	private List<OptGroup> retrieveOptGroups() throws WGAPIException{
		String itemname = getOptgroupsitem();
		if (WGUtils.isEmpty(itemname))
			return null;

		List<OptGroup> result = new ArrayList<OptGroup>();
		List<Object> optgroups = getTMLContext().itemlist(itemname);
		for(Object group: optgroups){
			if(!(group instanceof Map)){
				addWarning("optiongroupsitem no-map value: " + group.getClass().getName());
				continue;
			}
			@SuppressWarnings("rawtypes")
			Map groupmap = (Map)group;
			Object label = groupmap.get("label");
			Object options = groupmap.get("options");
			if(options==null || label==null){
				addWarning("optiongroup without label or options");
				continue;
			}
			if(!(label instanceof String)){
				addWarning("optiongroup label is no String");
				continue;				
			}
			if(!(options instanceof List)){
				addWarning("optiongroup options is no List");
				continue;				
			}
			result.add(new OptGroup((String)label, WGUtils.toString((List)options)));
		}
		
		return result;
	}
	
	private String renderOptGroup(List<Object> values) throws WGAPIException{
		
		List<OptGroup> optgroups = retrieveOptGroups();
		if(optgroups==null)
			return "";
		
		StringBuffer result = new StringBuffer();
		for(OptGroup optgroup: optgroups){
			result.append("<optgroup label=\"" + optgroup.getLabel() + "\">");
			for(InputOption option: optgroup.getOptions()){
				result.append("<option value=\"" + option.getValue() + "\"");
    			if (values.contains(option.getValue())) {
    				result.append(" selected");
    			}
    			if(option.isDisabled())
    				result.append(" disabled");
				result.append(">" + option.getText() + "</option>");				
			}
			result.append("</optgroup>");
		}

		return result.toString();
	}
	
    private void createChangeActionJS(String name, FormInputRegistrator form, String event) throws WGAPIException, TMLException {
        String changeaction = this.getChangeaction();
		if (!WGUtils.isEmpty(changeaction)) {
            if (isAjaxCall()) {
                // create ajax call
                TMLAction action = getTMLContext().getActionByID(changeaction, getDesignDBKey());
                if (action != null) {
                    this.appendResult(event + "=\"" + getAjaxJSFunction(action, null, Collections.<Object>singletonList(name), isKeepParamsOnAJAX()) + "\"");
                }
                else {
                    addWarning("Action of id '" + changeaction + "' is not defined");
                }
            } else {            
    			String actionLink = buildCallActionLink(changeaction, (form != null ? form.getId() : null), null, Collections.<Object>singletonList(name), null, null);
    			if (actionLink != null) {
    				this.appendResult(event + "=\"callAction('" + actionLink + "')\" ");
    			}
            }
		}
    }


	private void renderOptionInput(String type, String name, String cssClass, String cssStyle, FormInputRegistrator form, List<Object> values, String tagContent, String disabled) throws WGException {
		String optionValue;
		String optionText;
        Iterator<InputOption> options = this.retrieveInputOptions().iterator();
        
        String labeled = getLabeled();
        boolean doLabelling = false;
        boolean wrapWithLabels = false;
        if(labeled!=null){
            doLabelling = labeled.equalsIgnoreCase("true");
            wrapWithLabels = labeled.equalsIgnoreCase("wrap");        	
        }
        
        // We need an id for option inputs so we can reference them from their labels
        String theId = getId();
        if (doLabelling && theId == null) {
            theId = "option" + UIDGenerator.generateUID();
        }
        
        int idx=0;
        
		while (options.hasNext()) {
            InputOption option = options.next();
            idx++;
		    optionText = option.getText();
            optionValue = option.getValue();
			
			// Build html
			String htmlDivider = getMultiValueDivider();
			String optionId = "";
			String optionIdHtml = "";
			if (theId != null) {
			    optionId = theId + "_" + idx;
			    optionIdHtml = " id=\"" + optionId + "\"";
			}
		
			getTMLContext().setvar("$O_VALUE", optionValue);
			String dynHtml = buildDynamicHtmlAttributes();
			getTMLContext().removevar("$O_VALUE");
			
			if(wrapWithLabels)
				this.appendResult("<label>");
				
			this.appendResult("<input").appendResult(dynHtml).appendResult(optionIdHtml).appendResult(" type=\"").appendResult(type).appendResult("\" name=\"").appendResult(name).appendResult("\" ");

			// checked
			if ( optionValue != null ) {
				for( int i=0 ; i < values.size() ; i++){
					if( String.valueOf( values.get(i) ).equalsIgnoreCase(optionValue) ){
						this.appendResult(" checked=\"true\"");
					}
				}				
			}			

			String optionDisabled = disabled;
			if(option.isDisabled())
				optionDisabled = " disabled ";
			
			this.appendResult(" value=\"").appendResult(WGUtils.encodeHTML(optionValue)).appendResult("\" ");	            
            if(optionDisabled.isEmpty())
            	createChangeActionJS(name, form, "onclick");
            
			this.appendResult(cssClass).appendResult(cssStyle).appendResult(optionDisabled).appendResult(tagContent).appendResult(">");
			
			if (doLabelling) {
			    this.appendResult("<label for=\"").appendResult(optionId).appendResult("\">").appendResult(optionText).appendResult("</label>");
			}
			else {
			    this.appendResult(optionText);
			}

			if(wrapWithLabels)
				this.appendResult("</label>");
			
			if (options.hasNext()) {
				appendResult(htmlDivider);
			}
			
		}
	}

    private String getMultiValueDivider() {
        String htmlDivider = getDivider();
        if (htmlDivider == null || htmlDivider.equals("")) {
        	htmlDivider = "<br>";
        }
        return htmlDivider;
    }

	private void renderSimpleInput(
		String type,
		String name,
		String format,
		String cssClass,
		String cssStyle,
		Object singleValue,
		String tagContent,
        String disabled) throws FormattingException, WGException {
        
		String html_type=getDynamicHtmlAttribute("type");
		if(html_type!=null)
			type=html_type;
		
        singleValue = new TagOutputFormatter(format, getTMLContext(), stringToBoolean(getTrim())).format(singleValue);
                				
		this.appendResult("<input").appendResult(buildDynamicHtmlAttributes()).appendResult(" name=\"").appendResult(name).appendResult("\" ");
		if(getDynamicHtmlAttribute("type")==null)
			this.appendResult("type=\"").appendResult(type).appendResult("\" ");
		String theId = getId();
		if (theId!=null)
			this.appendResult("id=\"" + theId + "\" ");
		this.appendResult("value=\"").appendResult(WGUtils.encodeHTML(String.valueOf(singleValue), true, false)).appendResult("\" ");
		
		this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(">");
	}
	
	private void renderDateInput(
		String name,
		String format,
		String cssClass,
		String cssStyle,
		Object singleValue,
		String tagContent,
        String disabled) throws FormattingException, WGException {
	
		if (format==null){
			getStatus().format = "yyyy/MM/dd HH:mm:ss";
			format="yyyy/MM/dd HH:mm:ss";	
		}			
		renderSimpleInput("text", name, format, cssClass, cssStyle, singleValue, tagContent, disabled);
		/*
		 * The following Code seems to do nothing.
		 * May be a relict 
		FormInputRegistrator formBase = (FormInputRegistrator) getStatus().getAncestorTag(FormBase.class);	
        if (formBase != null) {
            if (formBase.getFormMode().equals(TMLFormInfo.READONLY_MODE) || formBase.getFormMode().equals(TMLFormInfo.VIEW_MODE)) {
                return;		// no calendar control if form is in readonly- or view-mode.
            }
        }
        */
	}
		
	private void renderTextArea( String name, String format, String cssClass, String cssStyle, Object value, String tagContent, boolean multiple, String disabled) throws FormattingException, WGException {
		
		if (value == null) {
			value = "";
		}
		
		if (format != null && (type.equals("date") || type.equals("number"))) {
			value = new TagOutputFormatter(format, getTMLContext(), stringToBoolean(getTrim())).format(value);
		}
		this.appendResult("<textarea").appendResult(buildDynamicHtmlAttributes()).appendResult(" name=\"").appendResult(name).appendResult("\" ");

		String theId = getId();
		if (theId!=null)
			this.appendResult("id=\"" + theId + "\" ");
		
		this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(">");
		
		if ( multiple ) {
			@SuppressWarnings("unchecked")
            List<Object> listEntries = (List<Object>) value;
			if( !listEntries.isEmpty() ){
				Iterator<Object> it = listEntries.iterator();
				while( it.hasNext() ) {
					Object singleValue = it.next();
                    if (singleValue == null) {
                        singleValue = "";
                    }
                    
                    String line = WGUtils.encodeHTML(singleValue.toString().trim(), true, WGUtils.LINEFEEDS_REMOVE);
                    if (it.hasNext()) {
                        line +="\r\n";
                    }
                    
                    this.appendResult( line );
				}
			}			
			this.appendResult("</textarea>");
		}
		else {
			this.appendResult(WGUtils.encodeHTML(value.toString(), true, WGUtils.LINEFEEDS_KEEP)).appendResult("</textarea>");
		}
	}

	private void renderBoolean( String name, String cssClass, String cssStyle, FormInputRegistrator form, Object singleValue, String tagContent, String disabled) throws WGException {
		Boolean value = new Boolean( false );
		if (singleValue instanceof Boolean){
			value = (Boolean)singleValue;
		}
		else{
			if (singleValue instanceof String){
				value = Boolean.valueOf( (String)singleValue );	
			}
			else{
				value = Boolean.FALSE;
			}
		}

		String labeled = getLabeled();
		boolean doLabelling = false;
		boolean wrapWithLabels = false;
		if(labeled!=null){
			doLabelling = labeled.equalsIgnoreCase("true");
			wrapWithLabels = labeled.equalsIgnoreCase("wrap");        	
		}

        // We need an id for option inputs so we can reference them from their labels
        String theId = getId();
        if (doLabelling && theId == null) {
            theId = "option" + UIDGenerator.generateUID();
        }

        List<InputOption> options = this.retrieveInputOptions();
        
        boolean renderCheckbox = false;
        if (options.size() == 0) {
            renderCheckbox = true;
        }
        else if (options.size() == 1 && options.get(0).getValue().equals("true")) {
            renderCheckbox = true;
        }
        
        int idx=0;
        if (!renderCheckbox) {
            Iterator<InputOption> optionsIt = options.iterator();
            String optionValue;
            String optionText;        
    		while (optionsIt.hasNext()) {
            	idx++;
                InputOption option = optionsIt.next();
    			optionValue = option.getValue();
                optionText = option.getText();
			    String optionId = null;

    			if (theId != null) {
    			    optionId = theId + "_" + idx;
    			}

    			String htmlDivider = getMultiValueDivider();
    	
    			if(wrapWithLabels)
    				this.appendResult("<label>");

    			this.appendResult("<input").appendResult(buildDynamicHtmlAttributes());
    			if(optionId!=null){
    				this.appendResult(" id=\"" + optionId + "\"");
    			}
    			this.appendResult(" type=\"radio\" name=\"").appendResult(name).appendResult("\" ");
    			this.appendResult(" value=\"").appendResult(optionValue).appendResult("\" ");
    			
    			createChangeActionJS(name, form, "onclick");
    
    			if ( optionValue != null ) {				
    				if( String.valueOf( value ).equalsIgnoreCase(optionValue) ){
    					this.appendResult(" checked=\"true\"");
    				}								
    			}			
    			this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(">");

    			if (doLabelling && optionId!=null) {
    			    this.appendResult("<label for=\"").appendResult(optionId).appendResult("\">").appendResult(optionText).appendResult("</label>");
    			}
    			else {
    			    this.appendResult(optionText);
    			}
    			
    			if(wrapWithLabels)
    				this.appendResult("</label>");

    			if (optionsIt.hasNext()) {
    				appendResult(htmlDivider);
    			}
    	
    		}
        }
        
        // If no options given Build a single checkbox representing "true"
        else {

        	if(wrapWithLabels)
    			this.appendResult("<label>");
        	
            this.appendResult("<input").appendResult(buildDynamicHtmlAttributes());
            if (theId!=null) {
            	this.appendResult(" id=\"" + theId + "\"");
            }
            this.appendResult(" type=\"checkbox\" name=\"").appendResult(name).appendResult("\" ");
            this.appendResult(" value=\"true\" ");
            
            createChangeActionJS(name, form, "onclick");
            
            if ( value.booleanValue() == true ) {                
                this.appendResult(" checked=\"true\"");
            }
            
            if (options.size() == 1 && options.get(0).isDisabled()) {
            	this.appendResult(" disabled ");
            	disabled="";	// do not disable twice
            }
            
            this.appendResult(cssClass).appendResult(cssStyle).appendResult(disabled).appendResult(tagContent).appendResult(">");

            if (options.size() == 1) {
            	String optionText = options.get(0).getText();
    			if (doLabelling) {
    			    this.appendResult("<label for=\"").appendResult(theId).appendResult("\">").appendResult(optionText).appendResult("</label>");
    			}
    			else {
    			    this.appendResult(optionText);
    			}
            }

    		if(wrapWithLabels)
    			this.appendResult("</label>");

        }
	}

	/**
	 * Gets the default
	 * @return Returns a String
	 */
	public String getDefault() {
		//return this.getTagAttributeValue("default", defaultvalue, (String) getTMLContext().db().getNoItemBehaviour().getForTMLFormEmptyField());
	    return this.getTagAttributeValue("default", defaultvalue, null);
	}
	
	/**
	 * Sets the default
	 * @param type The type to set
	 */
	public void setDefault(String defaultvalue) {
		this.defaultvalue = defaultvalue;
	}

	/**
	 * Gets the type
	 * @return Returns a String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", type, "text");
	}
	/**
	 * Sets the type
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * Gets the name
	 * @return Returns a String
	 */
	public String getName() {
		return this.getTagAttributeValue("name", name, null);
	}
	/**
	 * Sets the name
	 * @param name The name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Gets the options
	 * @return Returns a String
	 */
	public String getOptions() {
		return this.getTagAttributeValue("options", options, null);
	}
	private List<String> getHdbmodelRelationOptions(String titleExpression) {
        
	    
	    try {
	        // Prerequisites: Contentclass-based form, HDB model object, relation
	        TMLForm form = getTMLContext().gettmlform();
	        if (form == null || form.getforminfo().getContentClass() == null) {
	            return null;
	        }
	        
	        HDBModel model = (HDBModel) getTMLContext().gettmlform().gettargetcontext().db().getAttribute(WGACore.DBATTRIB_HDBMODEL);
	        if (model == null) {
	            return null;
	        }
	        
	        Relation relation = null;
            for (Content contentModel : model.getModelsForContentClass(form.getforminfo().getContentClass(), getTMLContext().content(), false)) {
                relation = (Relation) JXPathContext.newContext(contentModel).selectSingleNode("/relations[name='" + getName() + "']");
                if (relation != null) {
                    break;
                }
            }
            
            if (relation == null) {
                return null;
            }
	        
	        WGAbstractResultSet relationTargets = model.getRelationTargets(getTMLContext().content(), form.getforminfo().getContentClass(), getName());
	        String emptyTitle = relation.isOptional() ? getTMLContext().systemLabel("tmlform", "input.emptytitle") : null;
	        return getTMLContext().buildOptions(relationTargets, titleExpression, emptyTitle);
	        
	    }
	    catch (Throwable e) {
	        getTMLContext().getlog().error("Exception determining input option defaults", e);
	        return null;
	    }
	    
    }

    /**
	 * Sets the options
	 * @param options The options to set
	 */
	public void setOptions(String options) {
		this.options = options;
	}

	/**
	 * Gets the multiple
	 * @return Returns a String
	 */
	public String getMultiple() {
		return this.getTagAttributeValue("multiple", multiple, "false");
	}
	/**
	 * Sets the multiple
	 * @param multiple The multiple to set
	 */
	public void setMultiple(String multiple) {
		this.multiple = multiple;
	}

	/**
	 * Returns the cssClass.
	 * @return String
	 */
	public String getCssclass() {
		return this.getTagAttributeValue("cssclass", cssclass, null);
	}

	/**
	 * Sets the cssClass.
	 * @param cssClass The cssClass to set
	 */
	public void setCssclass(String cssClass) {
		this.cssclass = cssClass;
	}

	/**
	 * Returns the cssstyle.
	 * @return String
	 */
	public String getCssstyle() {
		return this.getTagAttributeValue("cssstyle", cssstyle, null);
	}

	/**
	 * Sets the cssstyle.
	 * @param cssstyle The cssstyle to set
	 */
	public void setCssstyle(String cssstyle) {
		this.cssstyle = cssstyle;
	}

	/**
	 * Returns the changeaction.
	 * @return String
	 */
	public String getChangeaction() {
		return this.getTagAttributeValue("changeaction", changeaction, null);
	}

	/**
	 * Sets the changeaction.
	 * @param changeaction The changeaction to set
	 */
	public void setChangeaction(String changeaction) {
		this.changeaction = changeaction;
	}

	/**
	 * @return
	 */
	public String getMeta() {
		return getTagAttributeValue("meta", meta, "false");
	}

	/**
	 * @param string
	 */
	public void setMeta(String string) {
		meta = string;
	}



    public String getValidation() {
        return getTagAttributeValue("validation" , validation, null);
    }



    public void setValidation(String validation) {
        this.validation = validation;
    }



    public String getMessage() {
        return getTagAttributeValue("message" , message, null);
    }



    public void setMessage(String message) {
        this.message = message;
    }



    public String getValidationdivider() {
        return getTagAttributeValue("validationdivider" , validationdivider, null);
    }

    public void setValidationdivider(String validationdivider) {
        this.validationdivider = validationdivider;
    }



    public String getMode() {
        return getTagAttributeValue("mode" , mode, FieldReg.EDIT_MODE);
    }

    public void setMode(String mode) {
        this.mode = mode;
    }
    
    
    
    public String getCleariferror() {
        return getTagAttributeValue("cleariferror" , cleariferror, null);
    }

    public void setCleariferror(String cleariferror) {
        this.cleariferror = cleariferror;
    }
    
    public String getEncode() {
        return this.getTagAttributeValue("encode", encode, (String) this.getTMLContext().getDesignContext().getDesignDB().getAttribute(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING));
    }
    
    public String getOnupdate() {
        return getTagAttributeValue("onupdate" , onupdate, null);
    }

    public void setOnupdate(String onupdate) {
        this.onupdate=onupdate;
    }
    
    public String getFocus() {
        return getTagAttributeValue("focus" , focus, "false");
    }

    public void setFocus(String focus) {
        this.focus=focus;
    }
    

    /**
     * @return Returns the optionsitem.
     */
    public String getOptionsitem() {
        return getTagAttributeValue("optionsitem", optionsitem, null);
    }

    /**
     * @param optionsitem The optionsitem to set.
     */
    public void setOptionsitem(String optionsitem) {
        this.optionsitem = optionsitem;
    }
    
    public String getOptgroupsitem(){
    	return getTagAttributeValue("optgroupsitem", optgroupsitem, null);
    }

    public void setOptgroupsitem(String value){
    	this.optgroupsitem = value;
    }

    public String getStore() {
        return getTagAttributeValue("store" , store, "true");
    }

    public void setStore(String store) {
        this.store = store;
    }

    public String getEncodeoptions() {
        return getTagAttributeValue("encodeoptions", encodeoptions, "true");
    }

    public void setEncodeoptions(String encodeoptions) {
        this.encodeoptions = encodeoptions;
    }

	public String getDefaultexpression() {
		return getTagAttributeValue("defaultexpression", defaultexpression, null);
	}

	public void setDefaultexpression(String defaultexpression) {
		this.defaultexpression = defaultexpression;
	}
	
    public String getRelationtype() {
        return getTagAttributeValue("relationtype" , relationtype, null);
    }

    public void setRelationtype(String relationtype) {
        this.relationtype = relationtype;
    }

    public String getLabeled() {
        return getTagAttributeValue("labeled", labeled, "false");
    }

    public void setLabeled(String labeled) {
        this.labeled = labeled;
    }

    public String getOptionstitle() {
        return getTagAttributeValue("optionstitle", optionstitle, null);
    }

    public void setOptionstitle(String optionstitle) {
        this.optionstitle = optionstitle;
    }

    public String getSortoptions() {
        return getTagAttributeValue("sortoptions", sortoptions, null);
    }

    public void setSortoptions(String sortoptions) {
        this.sortoptions = sortoptions;
    }
        
    @Override
    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list(super.getDynamicAttributePrefixes(), "html"); 
    }
    
    @Override
    protected boolean isTrimResultString() {
        return true;
    }

}

