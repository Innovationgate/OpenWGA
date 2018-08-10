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

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.jsp.PageContext;

import org.dom4j.Element;

import de.innovationgate.utils.FormattingChain;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.Base.DynamicAttribute;
import de.innovationgate.wgpublisher.webtml.FormBase.FormStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TagOutputFormatter;
import de.innovationgate.wgpublisher.webtml.utils.Warning;

public class BaseTagStatus {
    
   
    /**
     * The class of the tag whose status we are
     */
    public Class<?> tagClass = null;
    
    /**
     * The status of our tags parent tag 
     */
    public BaseTagStatus parentTag = null;
    
    
    /**
     * The cascadation level of this tag to the absolute root tag
     */
    protected Integer level = null;
    
    
    /**
     * All WebTML options for this tag
     */
    protected Map<String,TMLOption> tagOptions = null;
    
    /**
     * All options declared as dynamic attributes on this tag
     */
    protected Map<String,DynamicAttribute> dynamicOptions = new HashMap<String,DynamicAttribute>();
    
    /**
     * Regular WebTML variables with local scope since compliance 7.2
     */
    protected Map<String,Object> localVars = new HashMap<>();
    
    /**
     * Options set explicitly on this tag (excluding inherited ones) 
     */
    protected Map<String,Object> localTagOptions;

    /**
     * This tags own WebTML context
     */
    public TMLContext tmlContext;

    /**
     * The WebTML context inherited to child tags
     */
    public TMLContext childTMLContext;


    /**
     * Flag if the tag should evaluate its body content
     */
    public boolean evalBody = false;
    
    /**
     * Flag if the tag should write any variables
     */
    public boolean writeVars = true;


    /**
     * Flag if the tag should put out its result to the page
     */
    boolean resultOutput = true;

    /**
     * Iteration counter
     */
    public int iteration = 0;

    /**
     * Flag showing if a context change failed on this tag
     */
    boolean subContextError = false;

    /**
     * Flag showing if the tags execution was canceled
     */
    public boolean cancelTag = false;

    /**
     * Flag showing if the tag should keep its result after it's execution finishes (for other tags/functionalities to reference on)
     */
    protected boolean keepResult = false;


    /**
     * The tags result in native data type
     */
    public Object result = null;

    /**
     * A suffix for the tag result, put out after it in unencoded form
     */
    protected String suffix = "";

    /**
     * A suffix for the tag result, put out after it in unencoded form
     */
    protected String prefix = "";
    
    /**
     * Attribute delegate: Encodings to put out the tags result in. Initialized with the tags "encode" attribute content.
     */
    protected String encode;
    
    /**
     * Attribute delegate: Divider to put out between multiple result values. Initialized with the tags "divider" attribute content.
     */
    protected String divider;
    
    /**
     * Attribute delegate: Output formatting for date and number values. Initialized with the tags "format" attribute content.
     */
    protected String format;

    /**
     * WebTML debugger iteration nmde
     */
    protected Element iterationDebugNode = null;
    
    /**
     * Number of subtags on this tag (WebTML debugger)
     */
    protected int subtags = 0;
    
    /**
     * Starting time of the tag execution (WebTML debugger)
     */
    protected long starttime = 0;
    
    /**
     * Duration of the tag execution in milliseconds (WebTML Debugger)
     */
    protected long duration = 0;

    /**
     * Real processing time of the tags own functionality (excluding subtag duration) (WebTML debugger)
     */
    protected long processingTime = 0;
    
    /**
     * The tags unique id
     */
    public String id;

    /**
     * The line in the WebTML module which defines this tag
     */
    public int sourceLine = 0;

    /*
     * public TMLActionLink createCustomActionLink(Integer actionKey, List
     * params, TMLContext context) { return
     * createActionLink(String.valueOf(actionKey), params, context); }
     */
    
    /**
     * @see Base#tmlStartTag()
     */
    protected Element debugNode = null;

    public boolean trimResultString = false;
    
    protected TMLContext baseContext = null;

    public boolean directOutput = false;

    public PageContext pageContext;

    private String trim;

    public BaseTagStatus() {
    }
    
    public Object getTagInfo(String name) throws WGAPIException {

        name = name.toLowerCase().trim();

        if (name.equals("contentkey")) {
            return this.tmlContext.content().getContentKey().toString();
        }
        else if (name.equals("iteration")) {
            return iteration;
        }
        else if (name.equals("taglevel")) {
            return level;
        }
        else if (name.equals("result")) {
            return result;
        }
        else {
            return null;
        }
    }
    
    public void initAttributeDelegates(Base tag) {
        this.encode = tag.getEncode();
        this.divider = tag.getDivider();
        this.format = tag.getFormat();
        this.trim = tag.getTrim();
    }
    
    /**
     * Determining if the current tag may do direct output
     * @param tag
     */
    public void determineOutputType(Base tag) {
        
        // Tag must be generally DO capable
        if (!(this instanceof DirectOutputCapableTag)) {
            return;
        }
                
        DirectOutputCapableTag doTag = (DirectOutputCapableTag) this;
        
        // Tag must check itself it is in DO capable state
        if (!doTag.isDirectOutputCapable()) {
            return;
        }
        
        if (!isStatusDirectOutputCapable(tag)) {
            return;
        }
            
        this.directOutput = true;
        
    }

    public boolean isStatusDirectOutputCapable(Base tag) {
        // Parent tag must already have DO enabled
        if (parentTag != this && parentTag != null && !parentTag.directOutput) {
            return false;
        }
                    
        // The result of the tag must be put out
        if (!resultOutput) {
            return false;
        }
        
        // No vars may be written, was we would not be able to determine their value
        if (tag.isVarWritten()) {
            return false;
        }
                    
        // No formatting
        if (!WGUtils.isEmpty(this.format)) {
            return false;
         }
        
        // No encoding
        if (!WGUtils.isEmpty(this.encode) && !"none".equals(this.encode)) {
            return false;
        }
        
        // No trimming
        if (!"false".equals(trim)) {
            return false;
        }
        
        // No wrap attribute
        String wrap = tag.getWrap(); 
        if(wrap != null && !wrap.isEmpty())
        	return false;
        
        return true;
    }
    
    public BaseTagStatus getAncestorTag(Class<?> parentClass) {
        return getAncestorTag(parentClass, true);
    }
    
    
    public BaseTagStatus getAncestorTag(Class<? extends Object> parentClass, boolean traverseAcrossRootTags) {

        BaseTagStatus parent = this.getParentTag();

        while (parent != null) {
            
            if (!traverseAcrossRootTags && Root.class.isAssignableFrom(parent.tagClass)) {
                break;
            }
            
            if (parentClass.isAssignableFrom(parent.tagClass) || parentClass.isAssignableFrom(parent.getClass())) {
                return parent;
            }
            else {
                parent = parent.getParentTag();
            }
        }
        
        return null;

    }

    /**
     * search recursive for a parent includeTag with the given type
     * 
     * @param type
     * @return includeTag or null if not found
     */
    public BaseTagStatus getAncestorIncludeTag(String type) {

        Include.Status includeTag = (Include.Status) getAncestorTag(Include.class);
        if (includeTag != null) {
            if (includeTag.type.equals(type)) {
                return includeTag;
            }
            else {
                return includeTag.getAncestorIncludeTag(type);
            }
        }
        else {
            return null;
        }

    }
    public BaseTagStatus getParentTag() {
        if (parentTag == this) {
            return null;
        }
        else {
            return parentTag;
        }
    }

    public void retrieveParentTag(Base tag) {
        
        
        // Special functionality for tml:root searching its tml:include
        // parent
        if (this instanceof Root.Status) {
            PageContext pageContext = tag.getPageContext();
            BaseTagStatus includeTag = (BaseTagStatus) pageContext.getAttribute(Base.ATTRIB_INCLUDEPARENT, PageContext.REQUEST_SCOPE);
            if (includeTag != null) {
                pageContext.setAttribute(Base.ATTRIB_INCLUDEPARENT, null, PageContext.REQUEST_SCOPE);
                parentTag = includeTag;
            }
        }
        else {
            javax.servlet.jsp.tagext.Tag parent = tag.getParent();
            while (parent != null) {
                if (parent instanceof Base) {
                    parentTag = ((Base) parent).getStatus();
                    break;
                }
    
                parent = parent.getParent();
    
            }
        }
        

        

    }

    public BaseTagStatus getParentTag(Class<?> tagClass) {

        BaseTagStatus tag = this.getParentTag();
        if (tagClass.isAssignableFrom(tag.tagClass)) {
            return tag;
        }
        else {
            return null;
        }
    }

    public Root.Status getRootTag() {

        Root.Status tag = (Root.Status) tmlContext.getrequest().getAttribute(WGACore.ATTRIB_ROOT_TAG);
        if (tag != null) {
            return tag;
        }
        else if (this instanceof Root.Status) {
            return (Root.Status) this;
        }
        else {
            tmlContext.getlog().error("Tag " + id + " cannot identify root tag!");
            return null;
        }
    }
    
    public void addSubtags(int number) {
        subtags += number;
    }
    
    public BaseTagStatus getTagStatusById(String id) {
        return this.getTagStatusById(id, Base.class);
    }

    public BaseTagStatus getTagStatusById(String id, Class<?> tagClass) {

        BaseTagStatus tag = (BaseTagStatus) this.getTagIds().get(id);
        if (tag != null && (tagClass.isAssignableFrom(tag.getClass()) || tagClass.isAssignableFrom(tag.tagClass))) {
            return tag;
        }
        else {
            return null;
        }

    }
    
    @SuppressWarnings("unchecked")
    public Map<String,BaseTagStatus> getTagIds() {
        return (Map<String,BaseTagStatus>) this.tmlContext.getEnvironment().getPageContext().getAttribute(WGACore.ATTRIB_TAGIDS, PageContext.REQUEST_SCOPE);
    }
    
    public String getDesignDBKey() {
        return (String) this.getOption(Base.OPTION_DESIGNDB);
    }
    
    public Object getOption(String optionName) {
        TMLOption option = (TMLOption) tagOptions.get(optionName);
        if (option != null) {
            return option.getValue();
        }
        else {
            return null;
        }
    }
    
    public List<String> getOptionNames() {
        return new ArrayList<String>(tagOptions.keySet());
    }
    
    public TMLOption getOptionObject(String optionName) {
        TMLOption option = tagOptions.get(optionName);
        if (option != null) {
            return option;
        }
        else {
            return null;
        }
    }
   
    
    public void addWarning(String msg, boolean severe) {
        addWarning(tmlContext, msg, severe, null);
    }
    
    public void addWarning(TMLContext cx, String msg, boolean severe, Throwable cause) {

        
        WGACore core = tmlContext.getwgacore();
        if (!core.getWgaConfiguration().isWarningsEnabled()) {
            return;
        }

        PageContext pageContext = tmlContext.getEnvironment().getPageContext();
        @SuppressWarnings("unchecked")
        List<Warning> warnings = (List<Warning>) pageContext.getAttribute(Base.class.getName() + ":Warnings", PageContext.REQUEST_SCOPE);
        if (warnings == null) {
            warnings = new ArrayList<Warning>();
            pageContext.setAttribute(Base.class.getName() + ":Warnings", warnings, PageContext.REQUEST_SCOPE);
        }
        
        Warning warning = new Warning(this, cx, msg, severe);
        warnings.add(warning);

        if (core.getWgaConfiguration().isWarningsOutputOnConsole()) {
            if (cause != null) {
                core.getLog().warn(warning.getConsoleText(), cause);
            }
            else { 
                core.getLog().warn(warning.getConsoleText());
            }
        }

        if (severe == true) {
             cancelTag = true;
        }
        
    }
    

    
    public boolean isBrowserInterface4() {
        Boolean bi = (Boolean) tmlContext.gethttpsession().getAttribute(WGACore.ATTRIB_BI_VERSION4);
        if (bi != null && bi.booleanValue() == true) {
            return true;
        }
        else {
            return false;
        }
    }
    
    public String getRequestURL() {
        return tmlContext.getrequest().getAttribute(WGACore.ATTRIB_REQUESTURL).toString();
    }
    
    public void setOption(String name, Object value, String scope) {
        TMLOption option = new TMLOption(name, value, scope);
        tagOptions.put(name, option);
        localTagOptions.put(name, option.getValue());
        
    }
    
    public void removeOption(String option) {
        tagOptions.remove(option);
    }
    

    
    public String getRelevantForm() {

        // Form defined in this WebTML request
        FormStatus form = (FormStatus) getAncestorTag(Form.class);
        if (form != null) {
            return form.getId();
        }

        // Superform for AJAX request
        String subForm = (String) tmlContext.getrequest().getAttribute(WGACore.ATTRIB_IS_AJAX_SUBFORM_OF);
        if (subForm != null) {
            return subForm;
        }
        return null;
    }
    
    public String getTMLModuleName() {
        return (String) getOption(Base.OPTION_TMLMODULE_NAME);
    }
    
    public String getTMLModuleMediaKey() {
        return (String) getOption(Base.OPTION_TMLMODULE_MEDIAKEY);
    }
    
    /**
     * Returns a map of all WebTML options that were defined directly for the current WebTML tag.
     * WebTML options that were inherited from the parent tag are omitted here.
     * @return Map containing WebTML options, option names as key, option values as content
     */
    public Map<String,Object> getLocalOptions() {
        
        return new HashMap<String,Object>(localTagOptions);
        
    }
    
    public String getWGPPath() {
        return tmlContext.getEnvironment().getPublisherURL();
    }
    
    public List<Warning> getWarnings() {
        return tmlContext.getEnvironment().getWarnings();
    }
    
    public Map<String,TMLOption> getTagOptions() {
        return tagOptions;
    }
    
    public WGACore getCore() {
        return tmlContext.getwgacore();
    }
    

    
    public String getResultString(boolean includeFormatting, boolean trim) {

        if (result == null) {
            return "";
        }

        // Convert result to a list
        List<?> results;
        if (result instanceof List) {
            results = (List<?>) result;
        }
        else if (result instanceof Collection) {
            results = new ArrayList<Object>((java.util.Collection<?>) result);
        }
        else {
            results = Collections.singletonList(result);
        }

        FormattingChain formatters = new FormattingChain();
        if (includeFormatting == true) {
            // Get formatter objects regarding the Attributes format and
            // encoding

            // Attribute format
            formatters.addFormatter(new TagOutputFormatter(format, tmlContext, trim && !trimResultString));

            // Attribute encode
            if (this.encode != null) {
                formatters.addFormatter(new ObjectFormatter() {
                    @Override
                    public String format(Object obj) throws FormattingException {
                        try {
                            return WGA.get(tmlContext).encode(BaseTagStatus.this.encode, obj);
                        }
                        catch (WGException e) {
                            throw new FormattingException(e);
                        }
                    }
                });
            }
        }

        // Get a string from it. Serialize if it is a collection. Format output
        String result = de.innovationgate.utils.WGUtils.serializeCollection(results, (includeFormatting ? divider : ""), formatters);
        if (trim && trimResultString) {
            result = result.trim();
        }
        
        // Set formatting errors as warnings
        Iterator<FormattingException> errors = formatters.getErrors().iterator();
        while (errors.hasNext()) {
            FormattingException e = errors.next();
            Throwable rootCause = WGUtils.getRootCause(e);
            addWarning(tmlContext, "Formatting/Encoding error: " + rootCause.getMessage(), false, rootCause);
        }

        // Append prefix and suffix and return, maybe trimmed
        StringBuilder completeResult = new StringBuilder(prefix.length() + result.length() + suffix.length());
        completeResult.append(prefix);
        completeResult.append(result);
        completeResult.append(suffix);
        if (trim) {
            return completeResult.toString().trim();
        }
        else {
            return completeResult.toString();
        }

    }
    
    public void clearResult() {
        this.result = null;
        this.prefix = null;
        this.suffix = null;
    }

    public BaseTagStatus appendResult(String tmlResult) throws WGAPIException {
        
        try {
    
            if (tmlResult != null && WGUtils.stringToBoolean(this.trim) == true && !trimResultString) {
                tmlResult = tmlResult.trim();
            }
    
            if (directOutput) {
                Writer out = this.pageContext.getOut();
                out.write(tmlResult);
                return this;
            }
    
            
            if (result == null || !de.innovationgate.utils.WGUtils.isCollection(result)) {
                List<String> resultList = new ArrayList<String>();
                resultList.add(tmlResult);
                result = resultList;
            }
            else {
                @SuppressWarnings("unchecked")
                java.util.Collection<Object> resultCollection = (java.util.Collection<Object>) result;
                resultCollection.add(tmlResult);
            }
            return this;
        }
        catch (IOException e) {
            throw new WGAPIException("Exception appending tag result", e);
        }
    
    }
    
    public BaseTagStatus appendResult(Reader reader) throws WGAPIException {
        
        try {
    
            if (directOutput) {
                Writer out = this.pageContext.getOut();
                WGUtils.inToOut(reader, out, 4092);
                return this;
            }
            
            String tmlResult = WGUtils.readString(reader);
            if (tmlResult != null && WGUtils.stringToBoolean(this.trim) == true && !trimResultString) {
                tmlResult = tmlResult.trim();
            }
    
            
            if (result == null || !de.innovationgate.utils.WGUtils.isCollection(result)) {
                List<String> resultList = new ArrayList<String>();
                resultList.add(tmlResult);
                result = resultList;
            }
            else {
                @SuppressWarnings("unchecked")
                java.util.Collection<Object> resultCollection = (java.util.Collection<Object>) result;
                resultCollection.add(tmlResult);
            }
            return this;
        }
        catch (IOException e) {
            throw new WGAPIException("Exception appending tag result", e);
        }
    
    }
    
    public void setLocalVar(String name, Object object) {
        localVars.put(name, object);
    }
    
    public boolean hasLocalVar(String name) {
        return localVars.containsKey(name);
    }
    
    public Object getLocalVar(String name) {
        return localVars.get(name);
    }
    
    public Object removeLocalVar(String name) {
        return localVars.remove(name);
    }
    
    
    


}