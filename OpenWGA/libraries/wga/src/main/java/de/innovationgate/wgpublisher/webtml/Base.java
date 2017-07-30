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
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.tagext.BodyTag;
import javax.servlet.jsp.tagext.BodyTagSupport;
import javax.servlet.jsp.tagext.DynamicAttributes;
import javax.servlet.jsp.tagext.Tag;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.URLBuilder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.DeployedLayout;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.websockets.TMLPageWebSocket;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionCallParameters;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.AjaxActionDefinition;
import de.innovationgate.wgpublisher.webtml.utils.BooleanItemExpression;
import de.innovationgate.wgpublisher.webtml.utils.HTMLHeadInclusion;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;
import de.innovationgate.wgpublisher.webtml.utils.RootTagReceptor;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLOptionPreserver;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.TMLSilentCancelException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.VoidAttribute;

public abstract class Base extends BodyTagSupport implements DynamicAttributes {
    
    private static final long serialVersionUID = 1L;
    
    
    public enum DynamicAttributeValueType {
        STRING, ITEM_EXPRESSION
    }
    
    public class DynamicAttribute {
        
        private String _fullName;
        public DynamicAttribute(String fullName, String prefix, String baseName, String value, DynamicAttributeValueType valueType) {
            _fullName = fullName;
            _prefix = prefix;
            _baseName = baseName;
            _value = value;
            _valueType = valueType;
        }
        
        private String _prefix;
        private String _baseName;
        private String _value;
        private DynamicAttributeValueType _valueType;
        
        public String getPrefix() {
            return _prefix;
        }
        public String getBaseName() {
            return _baseName;
        }
        public String getValue() {
            return Base.this.getTagAttributeValue(_fullName, _value, null);
        }
        public DynamicAttributeValueType getValueType() {
            return _valueType;
        }
        
        public Object getDynamicValue(TMLContext tmlContext) throws WGException {

                String resolvedValue = getValue();
            
                if (_valueType == DynamicAttributeValueType.ITEM_EXPRESSION) {
                    return tmlContext.item(resolvedValue);
                }
                else {
                    return resolvedValue;
                }
        }
        
        public String getFullName() {
            return _fullName;
        }
        
        
    }

    public class MixedAttributeResolver implements ReplaceProcessor {
        
        public int replace(String text, int from, int to, Writer out) throws IOException {
            
            // find end position
            int endPos = text.indexOf("}", from);
            if (endPos == -1) {
                out.write("{");
                return to + 1;
            }
            
            // Cut out dynamic part
            String dynamicPart = text.substring(from, endPos + 1);
            
            // Resolve
            out.write(Base.this.resolveDynamicAttribute(dynamicPart, null));
            
            // Continue after dynamic part
            return endPos + 1;
            
        }
        
    }
    
    public interface ResultInterceptor {
        
        public boolean interceptResult(BaseTagStatus tagStatus);
        
    }

    protected static final Logger log = Logger.getLogger("wga");
    
    public static final DateFormat DEBUG_TIMESTAMP_FORMAT = new SimpleDateFormat("HH:mm:ss SSSS");
    
    public static final String SYSTEMOPTION_PREFIX = "$$wga_";
    
    public static final String OPTION_DEFAULT_XPLANGUAGE = SYSTEMOPTION_PREFIX + "defaultxpl";
    public static final String OPTION_DESIGNDB = SYSTEMOPTION_PREFIX + "designdb";
    public static final String OPTION_LINK_ACTION = SYSTEMOPTION_PREFIX + "linkAction";
    public static final String OPTION_LINK_MEDIUM = SYSTEMOPTION_PREFIX + "linkMedium";
    public static final String OPTION_PORTLET_NAMESPACE = SYSTEMOPTION_PREFIX + "portletKey";
    public static final String OPTION_CURRENT_MEDIAKEY = SYSTEMOPTION_PREFIX + "currentMediaKey";
    public static final String OPTION_TMLMODULE_NAME = SYSTEMOPTION_PREFIX + "tmlmoduleName";
    public static final String OPTION_TMLMODULE_MEDIAKEY = SYSTEMOPTION_PREFIX + "tmlmoduleMediakey";
    public static final String OPTION_RESULT_INTERCEPTORS = SYSTEMOPTION_PREFIX + "resultInterceptors";
    public static final String OPTION_DEFAULT_LABELFILE = SYSTEMOPTION_PREFIX + "defaultlabelbundle:";
    public static final String OPTION_DEFAULT_LABELCONTAINER = SYSTEMOPTION_PREFIX + "defaultlabelcontainer:";
    public static final String OPTION_INNER_LAYOUT = SYSTEMOPTION_PREFIX + "innerlayout";
    public static final String OPTION_PORTLET_EVENT_STARTINDEX = SYSTEMOPTION_PREFIX + "portletEventStartIndex";
    public static final String OPTION_WEBTML_SCOPE = SYSTEMOPTION_PREFIX + "webtmlScope";
    public static final String OPTION_IMAGE_DERIVATES = SYSTEMOPTION_PREFIX + "imageDerivates";
    
    public static final String OPTION_MODULE_CONTROLLER = SYSTEMOPTION_PREFIX + "moduleController";
    public static final String OPTION_META_MAPPINGS = SYSTEMOPTION_PREFIX + "metaMappings";
    public static final String OPTION_ITEM_MAPPINGS = SYSTEMOPTION_PREFIX + "itemMappings";
    public static final String OPTION_INCLUDELEVEL = SYSTEMOPTION_PREFIX + "includeLevel";
    
    // Collects all options that should not be recovered in AJAX requests
    public static final Set<String> UNRECOVERABLE_OPTIONS = new HashSet<String>();
    static {
        UNRECOVERABLE_OPTIONS.add(Base.OPTION_PORTLET_EVENT_STARTINDEX);
        UNRECOVERABLE_OPTIONS.add(Base.OPTION_DESIGNDB);
        UNRECOVERABLE_OPTIONS.add(Base.OPTION_TMLMODULE_NAME);
        UNRECOVERABLE_OPTIONS.add(Base.OPTION_TMLMODULE_MEDIAKEY);
        UNRECOVERABLE_OPTIONS.add(Base.OPTION_MODULE_CONTROLLER);
    }
    
    public static final String URL_VERSION_PARAMETER = "?version=" + WGAVersion.WGAPUBLISHER_MAJOR_VERSION + "-" + WGAVersion.WGAPUBLISHER_MINOR_VERSION + "-" + WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION;
    
    public static final String TAGVALUE_STATUS = "TagStatus";
    public static final String TAGVALUE_DYNAMIC_ATTRIBUTES = "TagDynamicAttributes";

    public static final String ATTRIB_INCLUDEPARENT = Include.class.getName() + ":IncludeParent";

    public static final String AJAX_MODE_NO_PORTLET_REFRESH = "norefresh";


    public BaseTagStatus getStatus() {
        return (BaseTagStatus) getValue(TAGVALUE_STATUS);
    }

    protected BaseTagStatus createTagStatus() {
        return new BaseTagStatus();
    }

    // Generic tag attributes
    private String context = null;
    private String privateContext = null;
    private String output = null;
    private String var = null;
    private String _if = null;
    private String _unless = null;
    private String sessionvar = null;
    private String appendvar = null;
    private String divider = null;
    private String format = null;
    protected String encode = null;
    private String sourceline = null;
    private String trim = null;
    private String uid = null;

    // Tags environment
    
    public void setResult(Object result) {
        getStatus().result = result;
    }

    protected Base appendResult(String tmlResult) throws WGAPIException {
        getStatus().appendResult(tmlResult);
        return this;
    }

    protected void clearResult() {

        getStatus().result = null;

    }

    public void addWarning(String message, boolean cancelTag) {
        getStatus().addWarning(getTMLContext(), message, cancelTag, null);
    }

    public void addWarning(String message) {
        addWarning(message, false);
    }

    public void clearWarnings() {
        this.pageContext.setAttribute(Base.class.getName() + ":Warnings", null, PageContext.REQUEST_SCOPE);
    }

    public TMLContext getMainContext() {
        
        // Wayyyyyy easier and faster....
        return getTMLContext().getmaincontext();
        
        //return this.getTMLContextForDocument((WGContent) this.pageContext.getRequest().getAttribute(WGACore.ATTRIB_MAINCONTEXT));
    }

    public TMLContext getTMLContextForDocument(WGDocument doc) {
        return getTMLContext().getTMLContextForDocument(doc);
    }

    public boolean stringToBoolean(String expr) {
        try {
            return WGUtils.stringToBoolean(expr);
        }
        catch (IllegalArgumentException e) {
            addWarning(e.getMessage(), false);
            return false;
        }
    }
    
    public int stringToInteger(String expr, int defaultValue) {
        
        if (expr == null) {
            return defaultValue;
        }
        
        try {
            int result = WGUtils.parseInt(expr);
            if (result == Integer.MAX_VALUE) { // Most likely because input contained something evaluating to "Infinity" (#00004187)
                this.addWarning("WebTML attribute evaluates to positive infinity, which is invalid: " + expr + ". Falling back to default: " + defaultValue);
                return 0;
            }
            return result;
        }
        catch (NumberFormatException exc) {
            this.addWarning("Cannot interpret WebTML attribute as number: " + expr + ". Falling back to default: " + defaultValue);
            return defaultValue;
        }
    }
    
    public BaseTagStatus getTagStatusById(String id) {
        return getStatus().getTagStatusById(id);
    }

    public BaseTagStatus getTagStatusById(String id, Class<?> tagClass) {
        return getStatus().getTagStatusById(id, tagClass);
    }

    /**
     * @see BodyTagSupport#doStartTag()
     */
    public final int doStartTag() throws JspException {

        try {
            
            // Basic initializations of status
            BaseTagStatus status = initializeStatus();

            // Eventually create debug node
            Element parentDebugNode = getParentTagDebugNode();
            if (parentDebugNode != null) {
                createDebugNode(parentDebugNode);
                status.iterationDebugNode = status.debugNode.addElement("starttag");
            }

            // First get tml context to allow script execution (maybe already needed in id-calculation)
            TMLContext parentContext = this.getParentTagContext();
            TMLContext baseContext = null;
            
            // We have no parent tag, so we are absolute root: Read main context from thread main context and construct WebTML page version of it
            if (parentContext == null) {
                TMLContext mainContext = TMLContext.getThreadMainContext();
                baseContext = new TMLContext(mainContext.content(), this);
                baseContext.makeThreadMainContext();
                status.baseContext = baseContext;
                
                // Import vars
                baseContext.importEnvironmentData(mainContext);
                
                // Import options from independent main context.
                for (String optionName : mainContext.getDesignContext().getOptionNames()) {
                    TMLOption option = mainContext.getDesignContext().getOption(optionName);
                    baseContext.setoption(option.getName(), option.getValue(), option.getScope());
                }
                
                
            }
            else {
                baseContext = new TMLContext(parentContext, status);
            }

            this.setTMLContext(baseContext);
            
            // Register status with id
            String id = this.getId();
            if (id != null) {
                status.id = id;
                Map<String,BaseTagStatus> tagIds = status.getTagIds();
                tagIds.put(id, status);
            }

            TMLContext tmlContext = null;

            // Set tag contexts
            status.childTMLContext = baseContext;

            if (this.getContext() != null) {
                tmlContext = baseContext.context(this.getContext(), false);
                if (tmlContext != null) {
                    this.setTMLContext(tmlContext);
                    this.setChildTagContext(tmlContext);
                }
                else {
                    status.subContextError = true;
                    

                    // Tag should not be canceled if the context validity
                    // is checked via iscontextvalid
                    boolean cancelTag = true;
                    if (this instanceof ConditionBase) {
                        ConditionBase conditionTag = (ConditionBase) this;
                        if (conditionTag.getIscontextvalid() != null) {
                            cancelTag = false;
                        }
                    }
                    if (cancelTag == true) {
                        String msg = "Failed context change: " + getContext();
                        if (baseContext.getlasterror() != null) {
                            msg += ". Reason: " + baseContext.getlasterror();
                        }
                        this.addWarning(msg, true);
                        setCancelTag(true);
                        return SKIP_BODY;
                    }
                    
                }
            }
            
            if (isRequestCancelled()) {
                return SKIP_BODY;
            }

            if (this.getPrivatecontext() != null) {
                tmlContext = baseContext.context(this.getPrivatecontext());
                if (baseContext.getlasterror() == null) {
                    this.setTMLContext(tmlContext);
                }
                else {
                    this.addWarning(tmlContext.getlasterror(), false);
                }
            }
            
            // Set boolean flags
            this.setEvalBody(true);

            if (this.getVar() != null || this.getSessionvar() != null || this.getAppendvar() != null) {
                if (this.getOutput() != null && this.stringToBoolean(this.getOutput()) == true) {
                    this.setResultOutput(true);
                }
                else {
                    this.setResultOutput(false);
                }
            }
            else {
                if (this.getOutput() != null && this.stringToBoolean(this.getOutput()) == false) {
                    this.setResultOutput(false);
                }
                else {
                    this.setResultOutput(true);
                }
            }

            if (status.iterationDebugNode != null) {
                status.iterationDebugNode.addAttribute("startedTagSpecific", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }
            
            // Let status object initialize its attribute delegates, needed for tag-specific processing
            status.initAttributeDelegates(this);
            
            // Check condition attributes
            
            if (!mayExecute()) {
                status.cancelTag = true;
                return BodyTag.SKIP_BODY;
            }
            
            // Execute specialized tml function
            try {
                this.tmlStartTag();
            }
            catch (TMLException exc) {
                handleTMLException(exc);
            }
            catch (Exception exc) {
                log.error("Error in tml processing", exc);
                this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
                this.setCancelTag(true);
            }
            
            if (status.iterationDebugNode != null) {
                status.iterationDebugNode.addAttribute("endedTagSpecific", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }

            status.determineOutputType(this);
            if (status.directOutput && status.result != null) {
                writeOutput();
                status.result = null;
            }
            
            if (status.debugNode != null && status.directOutput) {
                status.debugNode.addAttribute("directOutput", "true");
            }
            
            // Evaluate body, if not denied
            if (this.isEvalBody() && this.getCancelTag() == false) {
                
                try {
                    this.tmlInitBody();
                    this.iterationIncrement();
                }
                catch (TMLException exc) {
                    try {
                        handleTMLException(exc);
                    }
                    catch (HttpErrorException e) {
                        getPageContext().getRequest().setAttribute(WGACore.ATTRIB_EXCEPTION, e);
                        throw new JspException("Page rendering canceled bc. of HTTP error output");
                    }
                }
                catch (Exception exc) {
                    log.error("Error in tml processing", exc);
                    this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
                    this.setCancelTag(true);
                }
                catch (Error exc) {
                    log.error("Error in tml processing", exc);
                    this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
                    this.setCancelTag(true);
                }
                
                if (status.directOutput) {
                    pageContext.getOut().write(getPrefix());
                    return BodyTag.EVAL_BODY_INCLUDE;
                }
                else {
                    return BodyTag.EVAL_BODY_BUFFERED;
                }
                
            }
            else {
                return Tag.SKIP_BODY;
            }
            
        }
        catch (HttpErrorException exc) {
            getPageContext().getRequest().setAttribute(WGACore.ATTRIB_EXCEPTION, exc);
            throw new JspException("Page rendering canceled bc. of HTTP error output");
        }
        catch (Exception exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return Tag.SKIP_BODY;
        }
        catch (Error exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return Tag.SKIP_BODY;
        }
    }
    
    private boolean mayExecute() throws WGException {

        String ifTerm = getIf();
        if (ifTerm != null) {
            DynamicAttribute ifEquals = getStatus().dynamicOptions.get("if_equals");
            DynamicAttribute ifIn = getStatus().dynamicOptions.get("if_in");
            if (!evaluateItemConditionAttribute(ifTerm, ifEquals, ifIn)) {
                return false;
            }
        }
        
        String unlessTerm = getUnless();
        if (unlessTerm != null) {
            DynamicAttribute unlessEquals = getStatus().dynamicOptions.get("unless_equals");
            DynamicAttribute unlessIn = getStatus().dynamicOptions.get("unless_in");
            if (evaluateItemConditionAttribute(unlessTerm, unlessEquals, unlessIn)) {
                return false;
            }
        }
        
        return true;
        
    }

    /**
     * @param condition
     * @param equalsAttribute
     * @param inAttribute
     * @return
     * @throws WGException
     * @throws WGIllegalArgumentException
     */
    protected boolean evaluateItemConditionAttribute(String condition, DynamicAttribute equalsAttribute, DynamicAttribute inAttribute) throws WGException, WGIllegalArgumentException {
        
        // We want to equal the items from the condition to values given comma-separated
        if (equalsAttribute != null) {
            
            // Equality to a list of item expressions
            if (equalsAttribute.getValueType() == DynamicAttributeValueType.ITEM_EXPRESSION) {
                if (new BooleanItemExpression(condition, equalsAttribute.getValue()).isTrue(getTMLContext()) == false) {
                    return false;
                }
            }
            
            // Equality to a constant string value
            else {
                if (new BooleanItemExpression(condition).equalsValue(getTMLContext(), equalsAttribute.getValue()) == false) {
                    return false;
                }
            } 
                
            
            
        }
        
        // We want to have the items in a range of values, specified as lists or individual items (item lists can be given, where each list element is evaluated)
        else if (inAttribute != null) {
            
            List<Object> values = new ArrayList<>();
            if (inAttribute.getValueType() == DynamicAttributeValueType.ITEM_EXPRESSION) {
                for (String itemName : WGUtils.deserializeCollection(inAttribute.getValue(), ",", true, '\'', false)) {
                    Object value = getTMLContext().item(itemName);
                    if (value instanceof java.util.Collection<?>) {
                        values.addAll((java.util.Collection<?>) value);
                    }
                    else if (value instanceof Map<?,?>) {
                        values.addAll(((Map<?,?>) value).values());
                    }
                    else {
                        values.add(value);
                    }
                }
            }
            else {
                // A little pointless, but valid according to the spec
                values.add(inAttribute.getValue());
            }
            
            if (new BooleanItemExpression(condition).equalsValues(getTMLContext(), values) == false) {
                return false;
            }
            
        }
        
        // We want the items from the condition to evaluate to boolean true
        else {
            if (new BooleanItemExpression(condition).isTrue(getTMLContext()) == false) {
                return false;
            }
        }
        
        return true;
    }
    
    protected boolean evaluateItemInConditionAttribute(String condition, DynamicAttribute inAttribute) throws WGException, WGIllegalArgumentException {
        
        // We want to equal the items from the condition to values
        if (inAttribute != null) {
            
            // Equality to a list of item expressions
            if (inAttribute.getValueType() == DynamicAttributeValueType.ITEM_EXPRESSION) {
                if (new BooleanItemExpression(condition, inAttribute.getValue()).isTrue(getTMLContext()) == false) {
                    return false;
                }
            }
            
            // Equality to a constant string value
            else {
                if (new BooleanItemExpression(condition).equalsValue(getTMLContext(), inAttribute.getValue()) == false) {
                    return false;
                }
            } 
                
            
            
        }
        
        // We want the items from the condition to be true
        else {
            if (new BooleanItemExpression(condition).isTrue(getTMLContext()) == false) {
                return false;
            }
        }
        
        return true;
    }

    private boolean isRequestCancelled() {
        Boolean cancelled = (Boolean) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_REQUEST_CANCELLED);
        return (cancelled != null && cancelled.booleanValue());
    }

    private BaseTagStatus initializeStatus() {
        BaseTagStatus status = createTagStatus();
        setValue(TAGVALUE_STATUS, status);
        status.tagClass = getClass();
        status.retrieveParentTag(this);
        status.tagOptions = this.getParentTagOptions();
        status.localTagOptions = new HashMap<String,Object>();
        status.trimResultString = isTrimResultString();
        status.pageContext = getPageContext();
        String sourceLineStr = getSourceline();
        if (sourceLineStr != null) {
            status.sourceLine = Integer.parseInt(getSourceline());
        }
        
        if (this instanceof DynamicAttributes) {
            @SuppressWarnings("unchecked")
            Map<String,DynamicAttribute> dynaAtts = (Map<String, DynamicAttribute>) getValue(TAGVALUE_DYNAMIC_ATTRIBUTES);
            if (dynaAtts != null) {
                status.dynamicOptions = dynaAtts;
                removeValue(TAGVALUE_DYNAMIC_ATTRIBUTES);
            }
        }
        
        return status;
    }
    
    
    public BaseTagStatus getParentTag() {
        return getStatus().getParentTag();
    }

    /**
     * @return
     */
    private Element getParentTagDebugNode() {
        BaseTagStatus parent = getParentTag();
        if (parent != null) {
            return parent.iterationDebugNode;
        }
        else {
            return null;
        }
    }

    protected void writeOutput() {

        try {
            String result = this.getResultString();
            this.pageContext.getOut().write(result);
        }
        catch (java.io.IOException exc) {
            log.error("Error writing tml output", exc);
        }
    }

    private TMLContext getParentTagContext() throws WGAPIException {
        BaseTagStatus parent = this.getParentTag();
        if (parent != null) {
            return parent.childTMLContext;
        }
        else {
            return null;
        }
    }

    private Map<String,TMLOption> getParentTagOptions() {

        BaseTagStatus parent = this.getParentTag();
        
        if (parent != null) {
            return new HashMap<String,TMLOption>(parent.tagOptions);
        }
        else {
            return new HashMap<String,TMLOption>();
        }
    }
    


    protected String getTagAttributeValue(String att, String value, String defaultValue) {

        String attResult = null;

        // Nonexistent attribute
        if (value == null) {
            return defaultValue;
        }
        
        // Empty attribute
        if (value.length() == 0) {
            return value;
        }

        char firstChar = value.charAt(0);
        char lastChar = value.charAt(value.length() - 1);
        
        // Normal dynamic attribute
        if (firstChar == '{' && lastChar == '}') {
            attResult = resolveDynamicAttribute(value, defaultValue);
        }
        
        // Mixed attribute: Dynamic parts are contained in {}
        else if (firstChar == '[' && lastChar == ']') {
            attResult = resolveMixedAttribute(value);
        }
        else {
            attResult = value;
        }
        
        // Trace debug information
        Element debugNode = getStatus().debugNode;
        if (debugNode != null && attResult != null) {
            Element attElement = (Element) debugNode.selectSingleNode("attribute[@name='" + att + "']");
            if (attElement == null) {
                attElement = debugNode.addElement("attribute");
                attElement.addAttribute("name", att);
                attElement.addCDATA(attResult);
            }
        }

        return attResult;

    }

    private String resolveMixedAttribute(String value) {
        
        // Cutoff []
        value = value.substring(1, value.length() - 1);
        MixedAttributeResolver resolver = new MixedAttributeResolver();
        return WGUtils.strReplace(value, "{", resolver, true);
        
    }

    private String resolveDynamicAttribute(String value, String defaultValue) {
        
        String attResult = null;
        
        String lcValue = value.toLowerCase();
        if (lcValue.startsWith("{tag:")) {
            String tagId = value.substring(5, value.length() - 1).trim();
            BaseTagStatus tag = this.getTagStatusById(tagId);
            if (tag == null) {
                this.addWarning("Could not retrieve tag: " + tagId, false);
                attResult = "";
            }
            
            if (tag.result instanceof java.util.Collection) {
                attResult = WGUtils.serializeCollection((java.util.Collection<?>) tag.result, ",");
            }
            else {
                attResult = String.valueOf(tag.result);
            }
        }
        else if (lcValue.startsWith("{item:")) {
            String itemName = value.substring(6, value.length() - 1).trim();
            Object obj = null;
            try {
                obj = this.getTMLContext().itemlist(itemName);
                attResult = de.innovationgate.utils.WGUtils.serializeCollection((List<?>) obj, ",");
            }
            catch (WGAPIException e) {
                this.addWarning("Could not retrieve item: " + itemName + " bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage(), false);
                getTMLContext().getlog().error("Exception retrieving item: " + itemName, e);
                attResult = "";
            }
            if (obj == null) {
                this.addWarning("Could not retrieve item: " + itemName, false);
                attResult = "";
            }
        }
        else if (lcValue.startsWith("{meta:")) {
            String metaType ="content";
            String metaName = value.substring(6, value.length() - 1).trim();
            int slashPos = metaName.indexOf("/");
            if (slashPos != -1) {
                metaType = metaName.substring(0, slashPos).trim();
                metaName = metaName.substring(slashPos + 1).trim();
            }
            
            Object obj = null;
            try {
                obj = this.getTMLContext().metalist(metaType, metaName);
                attResult = de.innovationgate.utils.WGUtils.serializeCollection((List<?>) obj, ",");
            }
            catch (WGAPIException e) {
                this.addWarning("Could not retrieve meta: " + metaType + "/" + metaName + " bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage(), false);
                getTMLContext().getlog().error("Exception retrieving meta: " + metaType + "/" + metaName, e);
                attResult = "";
            }
            if (obj == null) {
                this.addWarning("Could not retrieve meta: " + metaType + "/" + metaName, false);
                
                attResult = "";
            }
            
        }
        else if (lcValue.startsWith("{label:")) {
            String labelName = value.substring(7, value.length() - 1).trim();
            int paramStartPos = labelName.indexOf("(");
            int paramEndPos = labelName.indexOf(")");
            List<String> params = null;
            if (paramStartPos != -1 && paramEndPos != -1) {
                params = WGUtils.deserializeCollection(labelName.substring(paramStartPos + 1, paramEndPos), ",", true);
                labelName = labelName.substring(0, paramStartPos);
            }
            attResult = getTMLContext().label(labelName, params);
        }
        else if (lcValue.startsWith("{option:")) {
            String optionName = value.substring(8, value.length() - 1).trim();
            Object option = this.getTMLContext().option(optionName);
            if (option == null) {
                this.addWarning("Could not retrieve option: " + optionName, false);
                attResult = "";
            }
            else {
                attResult = String.valueOf(option);
            }
        }
        else if (lcValue.startsWith("{plugin:")) {
            String pluginName = value.substring(8, value.length() - 1).trim();
            attResult = getTMLContext().plugindbkey(pluginName);
        }
        else if (lcValue.startsWith("{scoped:")) {
            String str = value.substring(8, value.length() - 1).trim();
            try {
                return WGA.get(getTMLContext()).scoped(str);
            }
            catch (WGException e) {
                this.addWarning("Could not scope string bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage(), false);
                getTMLContext().getlog().error("Exception scoping string", e);
            }
        }
        else if (value.length() > 0) {
        
            String expression = value.substring(1, value.length() - 1);
            de.innovationgate.wgpublisher.expressions.ExpressionEngine engine = de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory.getEngine(this.getDefaultExpressionLanguage());
            
            Map<String,Object> customObjects = new HashMap<String,Object>();
            customObjects.put("VOID", VoidAttribute.INSTANCE);
            
            de.innovationgate.wgpublisher.expressions.ExpressionResult result = engine.evaluateExpression(expression, getTMLContext(), ExpressionEngine.TYPE_EXPRESSION, customObjects);
            if (result.isError()) {
                addExpressionWarning(expression, result);
                attResult = "";
            }

            Object resultValue = result.getResult();
            if (resultValue == null) {
                attResult = "";
            }
            else if (resultValue == VoidAttribute.INSTANCE) {
                return defaultValue;
            }
            else if (resultValue instanceof java.util.Collection) {
                attResult = de.innovationgate.utils.WGUtils.serializeCollection((java.util.Collection<?>) resultValue, ",");
            }
            else {
                attResult = resultValue.toString();
            }
        }
        else {
            attResult = "";
        }
        return attResult;
    }

    /**
     * @see BodyTagSupport#doEndTag()
     */
    public final int doEndTag() throws JspException {

        BaseTagStatus status = getStatus();
        
        try {
            
            if (getTMLContext() == null) {
                if (this instanceof Root) {
                    return SKIP_PAGE;
                }
                else {
                    return EVAL_PAGE;
                }
            }

            if (this.getCancelTag() == true) {
                return EVAL_PAGE;
            }
            
            // Cancel if request was cancelled
            if (isRequestCancelled()) {
                return SKIP_PAGE;
            }

            if (status.debugNode != null) {
                status.iterationDebugNode = status.debugNode.addElement("endtag");
                status.iterationDebugNode.addAttribute("started", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }

            if (status.iterationDebugNode != null) {
                status.iterationDebugNode.addAttribute("startedTagSpecific", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }
            
            try {
                this.tmlEndTag();
            }
            catch (TMLException exc) {
                handleTMLException(exc);
            }
            catch (Exception exc) {
                log.error("Error in tml processing", exc);
                this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
                this.setCancelTag(true);
            }
            
            if (status.iterationDebugNode != null) {
                status.iterationDebugNode.addAttribute("endedTagSpecific", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }

            if (this.getCancelTag() == true) {
                return EVAL_PAGE;
            }
            
            if (status.writeVars) {
                if (this.getVar() != null) {
                    this.getTMLContext().setvar(this.getVar(), optionallyReduceListValue(status.result), false);
                }
    
                if (this.getSessionvar() != null) {
                    this.getTMLContext().setSessionVar(this.getSessionvar(), optionallyReduceListValue(status.result), false, false);
                }
                
                if (this.getPsessionvar() != null) {
                    this.getTMLContext().getportlet().setsessionvar(this.getPsessionvar(), optionallyReduceListValue(status.result), false, false);
                }
                
                if (this.getAppendvar() != null) {
                    this.getTMLContext().appendvar(this.getAppendvar(), status.result);
                }
            }

            if (this.isResultOutput() == true) {
                if (status.directOutput) {
                    pageContext.getOut().write(getSuffix());
                }
                else if (status.result != null) {
                    this.writeOutput();
                }
            }
            
            @SuppressWarnings("unchecked")
            List<ResultInterceptor> interceptors = (List<ResultInterceptor>) getOption(OPTION_RESULT_INTERCEPTORS);
            if (interceptors != null) {
                Iterator<ResultInterceptor> icIt = interceptors.iterator();
                while (icIt.hasNext()) {
                    ResultInterceptor i = icIt.next();
                    if (!i.interceptResult(status)) {
                        icIt.remove();
                    }
                }
            }

            if (this.getId() == null && status.keepResult == false) {
                status.result = null;
            }

            if (status.debugNode != null) {
                status.debugNode.addAttribute("ended", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
                status.debugNode.addAttribute("subtags", new Integer(status.subtags).toString());
                
                
                status.duration = (System.currentTimeMillis() - status.starttime);
                status.processingTime += status.duration;
                status.debugNode.addAttribute("duration", new Long(status.duration).toString());
                status.debugNode.addAttribute("processingtime", new Long(status.processingTime).toString());
                
                BaseTagStatus parent = getParentTag();
                if (parent != null) {
                    parent.addSubtags(status.subtags + 1);
                    parent.processingTime -= status.duration;
                }
                
            }
            return EVAL_PAGE;
        }
        catch (Exception exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return EVAL_PAGE;
        }
        catch (Error exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return EVAL_PAGE;
        }
        finally {
            // Absolute must-do cleanup operations of the tag
            try {
                tmlCleanup();
            }
            catch (Exception e) {
                log.error("Error in tml tag cleanup", e);
                this.addWarning("Exception on tag cleanup: " + e.getClass().getName() + ":" + e.getMessage(), true);
            }
            
            if (status.baseContext != null) {
                status.baseContext.removeThreadMainContext();
            }
            
            // Remove the tag status for memory reasons
            removeValue(TAGVALUE_STATUS);
        }
        

    }
    
    private Object optionallyReduceListValue(Object result) {

        if (getTMLContext().getDesignContext().getVersionCompliance().isAtLeast(7,2) && result instanceof List) {
            List<?> list = (List<?>) result;
            if (list.size() == 0) {
                return null;
            }
            else if (list.size() == 1) {
                return list.get(0);
            }
        }
        
        return result;
        
        
        
    }

    public void addResultInterceptor(ResultInterceptor ic) {
        
        @SuppressWarnings("unchecked")
        List<ResultInterceptor> interceptors = (List<ResultInterceptor>) getOption(OPTION_RESULT_INTERCEPTORS);
        if (interceptors == null) {
            interceptors = new ArrayList<>();
            getStatus().setOption(OPTION_RESULT_INTERCEPTORS, interceptors, TMLOption.SCOPE_LOCAL);
        }
        
        interceptors.add(ic);
        
    }

    /**
     * Method to overwrite for cleanup operations of the tag, that should be executed in any case
     * after tag processing, even when an error occurs
     */
    protected void tmlCleanup() {
    }

    /**
     * @see BodyTagSupport#doAfterBody()
     */
    public final int doAfterBody() throws JspException {

        try {
            
            BaseTagStatus status = getStatus();
            
            if (getTMLContext() == null) {
                return SKIP_BODY;
            }
            
            if (isRequestCancelled()) {
                return SKIP_BODY;
            }
            if (!status.directOutput) {
                this.appendResult(this.getBodyContent().getString());
                this.getBodyContent().clearBody();
            }
            this.setEvalBody(false);

            try {
                this.tmlAfterBody();
            }
            catch (TMLException exc) {
                handleTMLException(exc);
            }
            catch (Exception exc) {
                log.error("Error in tml processing", exc);
                this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
                this.setCancelTag(true);
            }

            if (this.isEvalBody() && this.getCancelTag() == false) {
                iterationIncrement();
                if (status.directOutput) {
                    pageContext.getOut().write(getDivider());
                    return EVAL_BODY_AGAIN;
                }
                else {
                    return EVAL_BODY_BUFFERED;
                }
            }
            else {
                if (status.iterationDebugNode != null) {
                    status.iterationDebugNode.addAttribute("ended", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
                }
                return SKIP_BODY;
            }
        }
        catch (Exception exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return SKIP_BODY;
        }
        catch (Error exc) {
            log.error("Error in tml processing", exc);
            this.addWarning(exc.getClass().getName() + ":" + exc.getMessage(), true);
            this.setCancelTag(true);
            return SKIP_BODY;
        }
    }

    private void handleTMLException(TMLException exc) throws JspException, HttpErrorException {

        // If root canceled we need to exit before anything else. WebTML environment may not be ready.
        if (this instanceof Root && exc.isCancelTag()) {
            throw new HttpErrorException(HttpServletResponse.SC_BAD_REQUEST, exc.getMessage(), null);
        }
        
        if (!(exc instanceof TMLSilentCancelException)) {
            this.addWarning(exc.getMessage(), exc.isCancelTag());
            if (exc.getRootCause() != null) {
                log.error("Exception in WebTML warning: " + exc.getMessage(), exc.getRootCause());
            }
        }
        
        if (exc.isCancelTag()) {
            this.setCancelTag(true);
        }
        
    }

    public void tmlStartTag() throws TMLException, WGException {
    }

;

    public void tmlAfterBody() throws TMLException, WGException {
    };

    public void tmlInitBody() throws TMLException, WGException {
    };

    public void tmlEndTag() throws TMLException, WGException {
    };

    /**
     * Gets the context
     * 
     * @return Returns a String
     */
    public String getContext() {
        return this.getTagAttributeValue("context", context, null);
    }

    /**
     * Sets the context
     * 
     * @param context
     *            The context to set
     */
    public void setContext(String context) {
        this.context = context;
    }

    /**
     * Gets the id
     * 
     * @return Returns a String
     */
    public String getId() {
        return this.getTagAttributeValue("id", id, null);
    }

    /**
     * Gets the output
     * 
     * @return Returns a String
     */
    public String getOutput() {
        return this.getTagAttributeValue("output", output, null);
    }

    /**
     * Sets the output
     * 
     * @param output
     *            The output to set
     */
    public void setOutput(String output) {
        this.output = output;
    }

    /**
     * Gets the privateContext
     * 
     * @return Returns a String
     */
    public String getPrivatecontext() {
        return this.getTagAttributeValue("privatecontext", privateContext, null);
    }

    /**
     * Sets the privateContext
     * 
     * @param privateContext
     *            The privateContext to set
     */
    public void setPrivatecontext(String tmlContext) {
        this.privateContext = tmlContext;
    }

    /**
     * Gets the var
     * 
     * @return Returns a String
     */
    public String getVar() {
        
        // Retrieve portlet variable name + prefix, if present
        String pvar = getPvar();
        if (pvar != null && getTMLContext() != null) {
            TMLPortlet portlet;
            try {
                portlet = getTMLContext().getportlet();
            }
            catch (WGAPIException e) {
                log.error("Exception retrieving portlet variable name", e);
                return null;
            }
            if (portlet != null && !portlet.isroot()) {
                return portlet.getVarPrefix() + pvar;
            }
        }
        
        return this.getTagAttributeValue("var", var, null);
    }

    /**
     * Sets the var
     * 
     * @param var
     *            The var to set
     */
    public void setVar(String var) {
        this.var = var;
    }

    /**
     * Gets the result
     * 
     * @return Returns a String
     */
    protected Object getResult() {
        return getStatus().result;
    }

    public String getResultString() {
        return getResultString(true);
    }

    public String getResultString(boolean includeFormatting) {

        BaseTagStatus status = getStatus();
        return status.getResultString(includeFormatting, stringToBoolean(getTrim()));

    }


    public WGDatabase openContentDB(String key) throws WGException {
        return this.getCore().openContentDB(getTMLContext().resolveDBKey(key), (HttpServletRequest) pageContext.getRequest());
    }

    public javax.servlet.jsp.PageContext getPageContext() {
        return this.pageContext;
    }

    public void iterationIncrement() {
        BaseTagStatus status = getStatus();
        status.iteration++;
        if (status.debugNode != null) {
            if (status.iterationDebugNode != null) {
                status.iterationDebugNode.addAttribute("ended", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            }
            status.iterationDebugNode = status.debugNode.addElement("body");
            status.iterationDebugNode.addAttribute("started", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            status.iterationDebugNode.addAttribute("iteration", String.valueOf(status.iteration));
            try {
                status.iterationDebugNode.addAttribute("childcontext", getChildTagContext().getpath());
            }
            catch (WGAPIException e) {
                status.iterationDebugNode.addAttribute("childcontext", "Error: " + e.getClass().getName()+ " message: " + e.getMessage());
            }
            status.iterationDebugNode.addAttribute("ccloadeddocs", String.valueOf(getChildTagContext().db().getSessionContext().getTotalFetchedCores()));
        }
    }

    /**
     * Gets the childTmlContext
     * 
     * @return Returns a TmlContext
     */
    public TMLContext getChildTagContext() {
        return getStatus().childTMLContext;
    }

    /**
     * Sets the childTagContext
     * 
     * @param childTagContext
     *            The childTagContext to set
     */
    public void setChildTagContext(TMLContext childTagContext) {
        this.getStatus().childTMLContext = childTagContext;
    }

    /**
     * Gets the tagContext
     * 
     * @return Returns a TagContext
     */
    public TMLContext getTMLContext() {
        return getStatus().tmlContext;
    }

    /**
     * Sets the tagContext
     * 
     * @param tagContext
     *            The tagContext to set
     */
    public void setTMLContext(TMLContext tagContext) {
        BaseTagStatus status = getStatus();
        status.tmlContext = tagContext;
        if (status.debugNode != null) {
            try {
                status.debugNode.addAttribute("context", getTMLContext().getpath());
            }
            catch (WGAPIException e) {
                status.debugNode.addAttribute("context", "Error: " + e.getClass().getName()+ " message: " + e.getMessage());
            }
        }
    }

    /**
     * Gets the evalBody
     * 
     * @return Returns a boolean
     */
    public boolean isEvalBody() {
        return getStatus().evalBody;
    }

    /**
     * Sets the evalBody
     * 
     * @param evalBody
     *            The evalBody to set
     */
    public void setEvalBody(boolean evalBody) {
        getStatus().evalBody = evalBody;
    }
    
    public boolean isWriteVars() {
        return getStatus().writeVars;
    }
    
    public void setWriteVars(boolean writeVars) {
        getStatus().writeVars = writeVars;
    }

    /**
     * Gets the iteration
     * 
     * @return Returns a int
     */
    public int getIteration() {
        return getStatus().iteration;
    }

    /**
     * Sets the iteration
     * 
     * @param iteration
     *            The iteration to set
     */
    public void setIteration(int iteration) {
        getStatus().iteration = iteration;
    }

    /**
     * Gets the resultOutput
     * 
     * @return Returns a boolean
     */
    public boolean isResultOutput() {
        return getStatus().resultOutput;
    }

    /**
     * Sets the resultOutput
     * 
     * @param resultOutput
     *            The resultOutput to set
     */
    public void setResultOutput(boolean resultOutput) {
        getStatus().resultOutput = resultOutput;
    }

    /**
     * Gets the divider
     * 
     * @return Returns a String
     */
    public String getDivider() {
        return this.getTagAttributeValue("divider", divider, "");
    }

    /**
     * Sets the divider
     * 
     * @param divider
     *            The divider to set
     */
    public void setDivider(String divider) {
        this.divider = divider;
    }

    /**
     * Gets the format
     * 
     * @return Returns a String
     */
    public String getFormat() {
        return this.getTagAttributeValue("format", format, null);
    }

    /**
     * Sets the format
     * 
     * @param format
     *            The format to set
     */
    public void setFormat(String format) {
        this.format = format;
    }

    public de.innovationgate.wgpublisher.WGPDispatcher getDispatcher() {
        return (de.innovationgate.wgpublisher.WGPDispatcher) this.pageContext.getServletContext().getAttribute(WGACore.ATTRIB_DISPATCHER);
    }

    public de.innovationgate.wgpublisher.WGACore getCore() {
        return (de.innovationgate.wgpublisher.WGACore) this.pageContext.getServletContext().getAttribute(WGACore.ATTRIB_CORE);
    }

    public String getMimeType() {
        return this.pageContext.getAttribute(WGACore.ATTRIB_MIMETYPE, PageContext.REQUEST_SCOPE).toString();
    }

    /**
     * Gets the encode
     * 
     * @return Returns a String
     */
    public String getEncode() {
        return this.getTagAttributeValue("encode", encode, null);
    }

    /**
     * Sets the encode
     * 
     * @param encode
     *            The encode to set
     */
    public void setEncode(String encode) {
        this.encode = encode;
    }

    public Integer getLevel() {

        BaseTagStatus status = getStatus();
        if (status.level == null) {
            BaseTagStatus parent = this.getParentTag();
            if (parent != null) {
                if (this instanceof Root) {
                    status.level = parent.level;
                }
                else {
                    status.level = new Integer(parent.level + 1);
                }

            }
            else {
                status.level = new Integer(0);
            }
        }
        return status.level;
    }

    public String getWGPPath() {
        return getTMLContext().getEnvironment().getPublisherURL();
    }




    
    public boolean isCollectionsShowReleasedOnly() {
        
        if (!getTMLContext().isbrowserinterface()) {
            return true;
        }
        
        Boolean colReleasedOnly = (Boolean) this.pageContext.getSession().getAttribute(WGACore.ATTRIB_BI_COLLECTIONS_SHOW_RELEASED_ONLY);
        if (colReleasedOnly != null && colReleasedOnly.booleanValue() == true) {
            return true;
        }
        else {
            return false;
        }
        
        
    }




    protected boolean isChildContextErrornous() {
        return getStatus().subContextError;
    }

    /**
     * Gets the cancelTag
     * 
     * @return Returns a boolean
     */
    public boolean getCancelTag() {
        return getStatus().cancelTag;
    }

    /**
     * Sets the cancelTag
     * 
     * @param cancelTag
     *            The cancelTag to set
     */
    public void setCancelTag(boolean cancelTag) {
        getStatus().cancelTag = cancelTag;
    }




    public OutputStream getOutputStream() {
        try {
            return this.getResponse().getOutputStream();
        }
        catch (IOException e) {
            log.error("Error retrieving output stream", e);
            return null;
        }
    }

    public HttpServletResponse getResponse() {

        HttpServletResponse response = (HttpServletResponse) this.pageContext.getRequest().getAttribute(WGACore.ATTRIB_SERVLETRESPONSE);
        if (response != null) {
            return response;
        }
        else {
            return (HttpServletResponse) getPageContext().getResponse();
        }
    }


    




    public Object getOption(String optionName) {
        return getStatus().getOption(optionName);
    }

    /**
     * @see javax.servlet.jsp.tagext.Tag#release()
     */
    public void release() {
        removeValue(TAGVALUE_STATUS);
        super.release();
    }

    public TMLUserProfile getUserProfile(WGDatabase db) {
            try {
               return getCore().getPersManager().fetchUserProfile((HttpServletRequest) getPageContext().getRequest(), (HttpServletResponse) getPageContext().getResponse(), db);
            }
            catch (WGAPIException e) {
                getCore().getLog().error("Unable to retrieve user profile", e);
            }
        return null;
        }

    public TMLUserProfile getUserProfile() {
        return this.getUserProfile(this.getTMLContext().getdocument().getDatabase());
    }

    /**
     * Returns the sessionvar.
     * 
     * @return String
     */
    public String getSessionvar() {
        return this.getTagAttributeValue("sessionvar", sessionvar, null);
    }

    /**
     * Sets the sessionvar.
     * 
     * @param sessionvar
     *            The sessionvar to set
     */
    public void setSessionvar(String sessionvar) {
        this.sessionvar = sessionvar;
    }

    public String getDefaultExpressionLanguage() {

        
        // Static tml has always expression language tmlscript, as this works for all DB implementations
        String requestType =
        (String) this.pageContext.getRequest().getAttribute(WGACore.ATTRIB_REQUESTTYPE);
        if(requestType.equals(WGPDispatcher.REQUESTTYPE_STATICTML)) {
            return ExpressionEngineFactory.ENGINE_TMLSCRIPT;
        }
         
        // Try to retrieve XPL set by Attrib defaultxpl in Range-Tag
        String xplang = (String) this.getOption(Base.OPTION_DEFAULT_XPLANGUAGE);

        // Get XPL by current design db
        if (xplang == null) {
            WGDatabase db = null;
            try {
                db = openContentDB(getDesignDBKey());
            }
            catch (WGException e) {
            }
            if (db != null) {
                xplang = (String) db.getAttribute(WGACore.DBATTRIB_EXPRESSION_DEFAULT);
            }
            else {
                return ExpressionEngineFactory.ENGINE_TMLSCRIPT;
            }
        }
        return xplang;

    }

    

    





    /*
     * public TMLActionLink createCustomActionLink(Integer actionKey, List
     * params, TMLContext context) { return
     * createActionLink(String.valueOf(actionKey), params, context); }
     */

    private String pvar;

    private String psessionvar;

    /**
     * @param rootElement
     */
    protected void createDebugNode(Element parent) {
        
        BaseTagStatus status = getStatus();
        
        status.starttime = System.currentTimeMillis();
        
        String className = getClass().getName();
        className = className.substring(className.lastIndexOf(".") + 1).toLowerCase();
        
        status.debugNode = parent.addElement("tmltag");
        status.debugNode.addAttribute("name", className);
        if (this instanceof Root) {
            status.debugNode.addAttribute("resource", ((Root) this).getResource());
            status.debugNode.addAttribute("path", status.debugNode.getUniquePath());
        }
        status.debugNode.addAttribute("sourceline", getSourceline());
        status.debugNode.addAttribute("started", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
        status.debugNode.addAttribute("level", String.valueOf(getLevel()));
        
        if (status.debugNode.getDocument().getRootElement().attributeValue("traceroptions", "false").equals("false")) {
            return;
        }
        
        Element options = status.debugNode.addElement("options");
        Iterator<String> opts = status.tagOptions.keySet().iterator();
        String key;
        Element optionElem;
        TMLOption tmloption;
        while (opts.hasNext()) {
            key = (String) opts.next();
            if (!key.startsWith("$")) {
                tmloption = status.tagOptions.get(key);
                optionElem = options.addElement("option");
                optionElem.addAttribute("name", tmloption.getName());
                optionElem.addAttribute("type", (tmloption.getValue() != null ? tmloption.getValue().getClass().getName() : "(none)"));
                optionElem.addText(String.valueOf(tmloption.getValue()));
            }
        }
    }

    /**
     * @param string
     */
    public void setSuffix(String string) {
        getStatus().suffix = string;
    }

    /**
     * @param string
     */
    public void setPrefix(String string) {
        getStatus().prefix = string;
    }

    public String getSuffix() {
        return getStatus().suffix;
    }

    public String getPrefix() {
        return getStatus().prefix;
    }

    public Element getDebugNode() {
        return getStatus().debugNode;
    }

    public Element getIterationDebugNode() {
        return getStatus().iterationDebugNode;
    }

    public String getSourceline() {
        return sourceline;
    }

    public void setSourceline(String string) {
        sourceline = string;
    }

    protected String buildCallActionLink(String sAction, String formID, Map<String,Object> namedParams, List<Object> params, String portletMode, String portletContext) throws WGAPIException, TMLException {
        TMLAction tmlAction = getTMLContext().getActionByID(sAction, getDesignDBKey());
        if (tmlAction != null) {
            TMLActionLink actionLinkObj = tmlAction.createActionLink(namedParams, params, getTMLContext());
            actionLinkObj.setPortletmode(portletMode);
            if (portletContext != null) {
            	actionLinkObj.setPortletContextPath(getTMLContext(), portletContext);
            }
            
            String actionLink;

            //if (formBase != null && formBase.getMode().equals(FormInfo.EDIT_MODE)) {
            //--> none editable forms should be submitted to ensure access in tml:action
            try {
                if (formID != null) {
                    actionLink = actionLinkObj.getJavascriptLink(getCore(), formID, getTMLContext().getDesignContext().getVersionCompliance());
                }
                else {
                    actionLink = actionLinkObj.getJavascriptLink(getCore(), null, getTMLContext().getDesignContext().getVersionCompliance());
                }
            }
            catch (UnsupportedEncodingException e) {
                throw new TMLException("Exception creating action link", e, true);
            }
            return actionLink;
        }
        else {
            return null;
        }
    }





    protected void addExpressionWarning(String expression, ExpressionResult result) {
        WGExpressionException exc = result.getException();
        
        // Put out as WebTML Warning
        String warningMessage = exc.getMessage() + (exc.getExpression() != null ? "\nExpression line: " + exc.getExpression() : "\nExpression:\n" + expression);
        if (!WGUtils.isEmpty(exc.getNativeStackTrace())) { 
            warningMessage += "\n" + exc.getNativeStackTrace();
        }
        this.addWarning(warningMessage);
        
        // Put out on log
        if (exc.getCause() != null) {
           getTMLContext().getlog().error("Exception executing tmlscript", exc.getCause()); 
        }
        else if (!WGUtils.isEmpty(exc.getNativeStackTrace())) {
            getTMLContext().getlog().error("Exception executing tmlscript: " + exc.getMessage() + "\n" + exc.getNativeStackTrace());
        }
    }

    /**
     * @return Returns the trim.
     */
    public String getTrim() {
        return getTagAttributeValue("trim", trim, "false");
    }

    /**
     * @param trim
     *            The trim to set.
     */
    public void setTrim(String trim) {
        this.trim = trim;
    }

    public String includeScript(String script) {

        StringBuffer scriptresult = new StringBuffer("");
        WGContent content = this.getTMLContext().content();
        script = script.toLowerCase();

        @SuppressWarnings("unchecked")
        List<String> includedscripts = (List<String>) getPageContext().getRequest().getAttribute("includedscripts");
        if (includedscripts != null && includedscripts.contains(script))
            return "";


        if (script.equals("htmlhead")) {
            scriptresult.append("<script type=\"text/javascript\" src=\"" + getWGPPath() + "/static/js/htmlhead.js" + URL_VERSION_PARAMETER + "\"></script>\n");
        }

        if (includedscripts == null) {
            includedscripts = new ArrayList<String>();
        }
        includedscripts.add(script);
        getPageContext().getRequest().setAttribute("includedscripts", includedscripts);

        return scriptresult.toString();
    }



    public WGDatabase openDesignDB() throws WGException {
    	
    	String designKey = (String) getDesignDBKey();
    	if (designKey != null) {
    		return this.openContentDB(designKey);
    	}
    	else {
    		return this.getTMLContext().db();
    	}
    	
    }


    

    


    public String getPvar() {
        return getTagAttributeValue("pvar", pvar, null);
    }
    
    public String getPsessionvar() {
        return getTagAttributeValue("psessionvar", psessionvar, null);
    }

    public void setPvar(String pvar) {
        this.pvar = pvar;
    }
    
    public void setPsessionvar(String pvar) {
        this.psessionvar = pvar;
    }
    
    public boolean isVarWritten() {
        return this.var != null || this.sessionvar != null || this.pvar != null || this.psessionvar != null || this.appendvar != null;
    }

    public String getTagName() {
        String className = getClass().getName();
        return className.substring(className.lastIndexOf(".") + 1).toLowerCase();
    }
    public void buildHTMLHead(boolean metaOutput, String scripts) throws WGException {
        TMLContext context = this.getTMLContext();
    	WGContent content = context.content();
    	
    	if( content!=null && metaOutput==true){		
    		// put out meta tags
    		this.appendResult("<meta name=\"generator\" content=\"").appendResult(WGACore.getGeneratorString()).appendResult("\">\n");		
   			this.appendResult("<meta name=\"keywords\" content=\"").appendResult(de.innovationgate.utils.WGUtils.serializeCollection(content.getKeywords(), ",")).appendResult("\">\n");
    	}
    
    	boolean includeHTMLHeadScript = true;
    	if(scripts!=null){
	    	try{
	    		includeHTMLHeadScript = WGUtils.stringToBoolean(scripts);
	    	}
	    	catch(Exception e){
	    		addWarning(e.getMessage(), false);
	    	}
    	}
    	if(includeHTMLHeadScript)
    		this.appendResult(includeScript("htmlhead"));
    	this.appendResult("<script type=\"text/javascript\" id=\"wga-htmlhead\">");
    	this.appendResult("WGA.contextpath=\"" + getWGPPath() + "\";");    // used by htmlhead.js since wga-4
    	this.appendResult("WGA.uriHash =\"" + getTMLContext().getUriHash() + "\";");
    	
    	StringBuilder out = new StringBuilder();
    	initPageConnectionClient(out);
    	this.appendResult(out.toString());
    	
    	if (getStatus().debugNode != null) {
    	    this.appendResult("WGA.debug = true;");
    	}
    	
        this.appendResult("</script>\n");
    
    	// optional includes for input fields	
        //	no longer supported since ... a long time. Removed Code.
        /*
         * 	We don't have any includeble scripts anymore in 2016
    	if (scripts!=null && !scripts.equalsIgnoreCase("none")){
    		java.util.StringTokenizer options = new java.util.StringTokenizer(scripts, ",");
    		while (options.hasMoreTokens()) {
    			this.appendResult(includeScript(options.nextToken().trim()));
    		}
    	}
    	*/
    	
    	// Process HTML head inclusion modules
    	for (HTMLHeadInclusion inc : getCore().getHtmlHeadInclusions()) {
    	    try {
    	        CharSequence result = inc.processInclusion(getTMLContext());
    	        if (result != null) {
    	            appendResult(String.valueOf(result));
    	        }
    	    }
    	    catch (Throwable e) {
    	        getTMLContext().getlog().error("Exception processing HTML head inclusion " + inc.getClass().getName(), e);
    	    }
    	}
    }

    protected void initPageConnectionClient(StringBuilder out) throws WGException {
        TMLPageImpl page = (TMLPageImpl) WGA.get(getTMLContext()).tmlPage();
    	PageConnection pageConn = page.getPageConnection(false);
    	if (pageConn == null || pageConn.isClientInited()) {
    	    return;
    	}
    	
    	WGA wga = WGA.get(getTMLContext());
    	URLBuilder url = wga.urlBuilder(wga.server().getBaseURL() + TMLPageWebSocket.PATH); 
    	url.clearParameters();
    	int port = url.getPort();
    	if (getTMLContext().getrequest().isSecure()) {
    	    url.setProtocol("wss");
    	}
    	else {
    	    url.setProtocol("ws");
    	}
    	
    	// We stay on the same port
    	if (port != -1) {
    	    url.setPort(port);
    	}
    	
    	boolean noCloseHandler = "true".equals(System.getProperty("de.innovationgate.wga.unittest"));
    	
    	out.append("WGA.onload.register(function() {WGA.websocket.start('")
    	.append(WGUtils.encodeJS(url.build(true))).append("', '")
    	.append(WGUtils.encodeJS(pageConn.getPageId())).append("', '")
    	.append(getTMLContext().gethttpsession().getId()).append("', ")
    	.append(noCloseHandler)
    	.append(")});");
    	
    	pageConn.setClientInited(true);
    }


    public String getTagDescription() {
        
        String requestType = (String) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_REQUESTTYPE);
        if (requestType.equals(WGPDispatcher.REQUESTTYPE_STATICTML)) {
                return "tml:" + getTagName() + " in static WebTML-Module " + getStatus().getRootTag().resource;   
        }
        else {
                return "tml:" + getTagName() + " on line " + getSourceline() + " in WebTML-Module " + getStatus().getTMLModuleName() + "/" + getStatus().getTMLModuleMediaKey() + " (" + getDesignDBKey() + ")";    
        }
        
    }
    
    public boolean isAjaxRequest() {
    	return (getPageContext().getRequest().getAttribute(WGACore.ATTRIB_AJAXINFO) != null);
    }

    protected StringBuffer createItemEditorDeclaration(String itemName, String editor, String label) {
        
        StringBuffer prefix = new StringBuffer("<div class=\"WGA-Item\">\n");
    	prefix.append("<div class=\"WGA-Item-Info\" style=\"display:none\">");
    	prefix.append(itemName+"|"+editor);
    	prefix.append("</div>\n");
    	
    	if (!editor.equalsIgnoreCase("custom")) {
    		prefix.append("<div class=\"WGA-Editor-Options\" style=\"display:none\">");
    		prefix.append("{" + getResultString(false) +"}");
    		prefix.append("</div>\n");
    	
    		// Old Style WGA4 Options:
    		if (editor.equalsIgnoreCase("rtf")) {
        		prefix.append("<div class=\"WGA4-Editor-Options\" style=\"display:none\">");
        		prefix.append("{");
        		String opt;
        		
        		opt = (String) getTMLContext().option("tabelStyleList");
        		if(opt != null)
        			prefix.append("tabelStyleList:[" + opt  + "],");
        		
        		opt = (String) getTMLContext().option("trStyleList");
        		if(opt != null)
        			prefix.append("trStyleList:[" + opt + "],");
        				
        		opt = (String) getTMLContext().option("tdStyleList");
        		if(opt != null)
        			prefix.append("tdStyleList:[" + opt + "],");
        		
        		opt = (String) getTMLContext().option("paragraphStyleList");
        		if(opt != null)
        			prefix.append("paragraphStyleList:[" + opt + "],");
        
        		opt = (String) getTMLContext().option("showoptions");
        		if(opt != null)
        			prefix.append("showoptions:\"" + opt + "\",");
        
        		opt = (String) getTMLContext().option("hideoptions");
        		if(opt != null)
        			prefix.append("hideoptions:\"" + opt + "\",");
        
        		prefix.append("dummy:true");
        		prefix.append("}");
        		prefix.append("</div>\n");
    		}
    	}
    	
    	prefix.append("<div class=\"WGA-Item-Edit\" style=\"display:none\"></div>");
    	prefix.append("<div class=\"WGA-Item-Label\" style=\"display:none\">");
    	prefix.append(label);
    	prefix.append("</div>\n");
        return prefix;
    }
    
    public String getDesignDBKey() {
        return getStatus().getDesignDBKey();
    }

    /**
     * Determines if AJAX action URLs are to keep URL parameters of original requests 
     */
    public boolean isKeepParamsOnAJAX() {
        
        TMLDesignContext designContext = getTMLContext().getDesignContext();
        
        // Try to determine via explicit setting as publisher option
        WGDatabase designDB = designContext.getDesignDB();
        if (designDB != null) {
            String keepParams = (String) designDB.getAttribute(WGACore.DBATTRIB_AJAX_KEEP_URL_PARAMS);
            if (keepParams != null) {
                if (Boolean.TRUE.equals(keepParams)) {
                    return true;
                }
                else if (Boolean.FALSE.equals(keepParams)) {
                    return false;
                }
            }
        }
        
        // Determine by design compliance
        if (designContext.getVersionCompliance().isAtLeast(6, 0)) {
            return true;
        }
        else {
            return false;
        }
        
    }
    
    /**
     * Determines if Non-AJAX action URLs are to keep URL parameters of original requests 
     */
    public boolean isKeepParamsOnNonAJAX() {
        
        TMLDesignContext designContext = getTMLContext().getDesignContext();
        
        // Design compliance > 6.0 always keeps them
        if (designContext.getVersionCompliance().isAtLeast(6, 0)) {
            return true;
        }
        
        // Below that it keeps them only on form posts
        else {
            return getStatus().getRelevantForm() != null;
        }
        
    }

    public String getAppendvar() {
        return getTagAttributeValue("appendvar", appendvar, null);
    }

    public void setAppendvar(String appendvar) {
        this.appendvar = appendvar;
    }

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }


    protected String buildDynamicHtmlAttributes() throws WGException {
        
        Map<String,DynamicAttribute> dynAtts = getStatus().dynamicOptions;
        if (dynAtts == null) {
            return "";
        }
        
        Map<String,String> htmlAtts = new HashMap<String,String>();
        for (DynamicAttribute att : dynAtts.values()) {
            if (att.getPrefix().equals("html")) {
                Object value = att.getDynamicValue(getTMLContext());
                if (value != null) {
                    htmlAtts.put(att.getBaseName(), String.valueOf(value));
                }
            }
        }
        
        StringBuilder html = new StringBuilder();
        for (Map.Entry<String,String> htmlAtt : htmlAtts.entrySet()) {
            html.append(" ");
            html.append(htmlAtt.getKey()).append("=\"").append(htmlAtt.getValue()).append("\"");
        }
        
        return html.toString();
        
    }

    protected void performInlineInclude(Inline.Pointer inlinePointer) throws TMLException, WGException {
        
        BaseTagStatus status = (BaseTagStatus) getStatus();
        if (!(status instanceof RootTagReceptor)) {
            throw new TMLException("Invalid attempt to perform an inline include from a tag, whose status is no RootTagReceptor: " + getClass().getName());
        }
        RootTagReceptor receptor = (RootTagReceptor) status;
        
        receptor.setStartTime(System.currentTimeMillis());
        
        DeployedLayout lyt = getCore().getDeployer().getLayoutMappings().get(inlinePointer.getLayoutKey());
        if (lyt == null) {
            throw new TMLException("Unable to read inline '" + inlinePointer.getDescription() + "' as there is no inline deployment", true);
        }
        
        String mediaKey = lyt.getMainLibMediaKey();
        status.setOption(OPTION_CURRENT_MEDIAKEY, mediaKey, null);
        String tmlResource = lyt.getResourcePath();
    
        
        // Do the include
        this.pageContext.setAttribute(ATTRIB_INCLUDEPARENT, status, PageContext.REQUEST_SCOPE);
        
        TMLOptionPreserver optionPreserver = new TMLOptionPreserver(getTMLContext().getDesignContext());
        optionPreserver.preserve(Base.OPTION_DESIGNDB,  inlinePointer.getDesignDB());
        optionPreserver.preserve(Base.OPTION_PORTLET_NAMESPACE, inlinePointer.getPortletKey());
        optionPreserver.preserve(Base.OPTION_MODULE_CONTROLLER, inlinePointer.getModuleController());
        try {
            this.pageContext.include(tmlResource);
        } 
        catch (Exception e) {
            throw new TMLException("Error executing inline '" + inlinePointer.getDescription() + " from tml module \"" + lyt.getMainLibName() + "/" + lyt.getMainLibMediaKey() + "\"", e, false);
        }
        finally {
            this.pageContext.setAttribute(ATTRIB_INCLUDEPARENT, null, PageContext.REQUEST_SCOPE);
            optionPreserver.restore();
        }
    
        // Transfer result from included root to this tag
        de.innovationgate.wgpublisher.webtml.Root.Status rootTagStatus = receptor.getRootTagStatus();
        if (rootTagStatus != null && !rootTagStatus.directOutput) {
            this.setResult(rootTagStatus.getResultString(true, false));
            rootTagStatus.clearResult();
        }
        else {
            throw new TMLException("Could not retrieve result of included resource: " + tmlResource);
        }
        
        
    }
    
    
    /**
     * Returns if the trim function on this tag should trim the completed result string instead of each result token
     */
    protected boolean isTrimResultString() {
        return false;
    }

    public void defaultSetDynamicAttribute(List<String> prefixes, String uri, String localName, Object value) throws JspException {
        
        
        for (String prefix : prefixes) {
            if (localName.startsWith(prefix + "_") || localName.startsWith(prefix + "-")) {

                int prefixSize = prefix.length() + 1;
                char divider = localName.charAt(prefix.length());
                String baseName= localName.substring(prefixSize);
                
                @SuppressWarnings("unchecked")
                Map<String,DynamicAttribute> dynAtts = (Map<String, DynamicAttribute>) getValue(TAGVALUE_DYNAMIC_ATTRIBUTES);
                if (dynAtts == null) {
                    dynAtts = new HashMap<String,DynamicAttribute>();
                    setValue(TAGVALUE_DYNAMIC_ATTRIBUTES, dynAtts);
                }
                
                dynAtts.put(prefix + "_" + baseName, new DynamicAttribute(localName, prefix, baseName, String.valueOf(value), divider == '-' ? DynamicAttributeValueType.ITEM_EXPRESSION : DynamicAttributeValueType.STRING));
                return;

                
            }
        }
        
        getCore().getLog().error("WebTML-Attribute '" + localName + "' is unknown");

    }

    public String getIf() {
        return getTagAttributeValue("if", _if, null);
    }

    public void setIf(String if1) {
        _if = if1;
    }

    public String getUnless() {
        return getTagAttributeValue("unless", _unless, null);
    }

    public void setUnless(String unless) {
        _unless = unless;
    }

    public Map<String,Object> buildNamedActionParameters(boolean serializable) throws WGException {
        
        Map<String,DynamicAttribute> dynAtts = getStatus().dynamicOptions;
        if (dynAtts == null) {
            return Collections.emptyMap();
        }
        
        Map<String,Object> params = new HashMap<String, Object>();
        for (DynamicAttribute att : dynAtts.values()) {
            
            if (att.getPrefix().equals("a")) {
                
                Object result = att.getDynamicValue(getTMLContext());
                
                TMLScript tmlscript = WGA.get(getTMLContext()).tmlscript();
                if (serializable && tmlscript.isNativeObject(result)) {
                    result = ExpressionEngineFactory.getTMLScriptEngine().serializeScriptable(getTMLContext(),result);
                }
                params.put(att.getBaseName(), result);
            }
            
        }
        return params;
    
        
        
    }

    /**
     * creates an javascript functioncall 
     * to call the given action with given params as ajaxAction
     * @param action
     * @param params
     * @param ajaxMode the ajax mode for this action call - valid values: ActionBase.AJAX_MODE_NO_PORTLET_REFRESH
     * @param portletMode the portlet mode to set after action call
     * @return Javascript-Function call    
     * @throws WGAPIException 
     * @throws TMLException 
     * @throws UnsupportedEncodingException 
     */
    protected String getAjaxJSFunction(TMLAction action, Map<String,Object> namedParams, List<Object> params, TMLActionCallParameters callParams, boolean keepParams, Map<String,String> additionalDefProps) throws WGAPIException, TMLException {
               
        TMLActionLink actionLink = action.createActionLink(namedParams, params, getTMLContext());
        
        //F00004242
        String ajaxMode = callParams.getAjaxMode();
        if (ajaxMode != null && ajaxMode.equalsIgnoreCase(Base.AJAX_MODE_NO_PORTLET_REFRESH)) {
            actionLink.setMode(TMLActionLink.MODE_AJAX_NO_PORTLET_REFRESH);
        }
        
        actionLink.setPortletmode(callParams.getPortletMode());
        if (callParams.getPortletContext() != null) {
        	actionLink.setPortletContextPath(getTMLContext(), callParams.getPortletContext());
        }
                        
        String id = (String) this.getOption(Include.OPTION_AJAX_DIVTAG_ID);        
        if (id == null) {
            throw new TMLException("Unable to render AJAX JavaScript Function: We do not seem to be inside an AJAX portlet");
        }
        
        String strAction;
        try {
            strAction = actionLink.getJavascriptLink(getCore(), callParams.getRelevantForm(), getTMLContext().getDesignContext().getVersionCompliance());
        }
        catch (UnsupportedEncodingException e) {
            throw new TMLException("Exception creating action link", e, true);
        }
    
        
        AjaxActionDefinition actionDef = new AjaxActionDefinition(strAction, id);
        actionDef.setKeepParams(keepParams);
        if (additionalDefProps != null) {
            actionDef.getAdditionalProps().putAll(additionalDefProps);
        }
        
        //F00004242
        if (ajaxMode != null && ajaxMode.equalsIgnoreCase(Base.AJAX_MODE_NO_PORTLET_REFRESH)) {
            actionDef.setGraydiv(false);
            actionDef.setMode(Base.AJAX_MODE_NO_PORTLET_REFRESH);             
        }
        
        
        return "WGA.ajax.action(" + actionDef.toJavaScriptObject() + ")";
    }

    /**
     * creates an javascript functioncall 
     * to call the given action with given params as simple action.
     * @param action
     * @param params
     * @param portletMode the portletmode to set after action call
     * @return Javascript-Function call    
     * @throws WGAPIException 
     * @throws UnsupportedEncodingException 
     */
    protected String getJSFunction(TMLAction action, Map<String,Object> namedParams, List<Object> params, TMLActionCallParameters callParams, boolean keepParams, Map<String,String> additionalDefProps) throws WGAPIException, TMLException {
        
        TMLActionLink actionLink = action.createActionLink(namedParams, params, getTMLContext());
        
        actionLink.setPortletmode(callParams.getPortletMode());
        if (callParams.getPortletContext() != null) {
        	actionLink.setPortletContextPath(getTMLContext(), callParams.getPortletContext());
        }
        
        try {
            String strAction = actionLink.getJavascriptLink(getCore(), callParams.getRelevantForm(), getTMLContext().getDesignContext().getVersionCompliance());
            
            StringBuffer actionCall = new StringBuffer();
            actionCall.append("WGA.action({");
            
            actionCall.append("action:'").append(strAction).append("'");
            actionCall.append(", keepParams:").append(keepParams);
    
            if (additionalDefProps != null) {
                for (Map.Entry<String,String> prop : additionalDefProps.entrySet()) {
                    actionCall.append(", " + prop.getKey() + ":" + prop.getValue());
                }
            }
            
            actionCall.append("})");
            return actionCall.toString();
        }
        catch (UnsupportedEncodingException e) {
            throw new TMLException("Exception creating action link", e, true);
        }
        
        
    }

    public void writePortletEventRegistration(StringBuffer out, TMLAction action, TMLActionCallParameters callParams, boolean keepParams, String onEvent) throws WGAPIException, TMLException {
    
        Map<String,String> additionalActionProps = new HashMap<String,String>();
        additionalActionProps.put("portletEvent", "e");
        
        String jsFunction;
        if (callParams.getAjaxMode() != null && !callParams.getAjaxMode().equals("false")) {
            jsFunction = "function(e){" + getAjaxJSFunction(action, null, null, callParams, keepParams, additionalActionProps) + "}";
        }
        else {
            jsFunction = "function(e){" + getJSFunction(action, null, null, callParams, keepParams, additionalActionProps) + "}";
        }
        
        
        out.append("WGA.event.addListener(\"");
        out.append(getTMLContext().getportlet().getportletkey());
        out.append("\", \"");
        out.append(onEvent);
        out.append("\", " + jsFunction + ");\n");
    }
    
    @Override
    public final void setDynamicAttribute(String uri, String localName, Object value) throws JspException {
        defaultSetDynamicAttribute(getDynamicAttributePrefixes(), uri, localName, value);
    }

    protected List<String> getDynamicAttributePrefixes() {
        return WGUtils.list("if", "unless");
    }
}
