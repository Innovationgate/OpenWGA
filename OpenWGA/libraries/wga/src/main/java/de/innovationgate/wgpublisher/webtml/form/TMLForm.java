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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.activation.DataSource;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileItemHeaders;
import org.apache.commons.fileupload.FileItemHeadersSupport;
import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.fileupload.util.LimitedInputStream;
import org.apache.commons.fileupload.util.Streams;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.log4j.Logger;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MD5HashingInputStream;
import de.innovationgate.utils.ReplaceProcessor;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.utils.security.HashedPassword;
import de.innovationgate.utils.security.HashingException;
import de.innovationgate.utils.security.HashingService;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.types.TMLFormSourceModuleType;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Form;
import de.innovationgate.wga.server.api.tml.Portlet;
import de.innovationgate.wgpublisher.InvalidEncryptionException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.services.ServicesFileItem;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.PCFile;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.utils.CSVWriter;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContext;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE,delegate=Form.class)
public class TMLForm extends de.innovationgate.wgpublisher.webtml.utils.TMLForm implements Form {
    
    public static final String VALIDATIONVAR_PARSEDVALUE = "$P_VALUE";
    public static final String VALIDATIONVAR_ENTEREDVALUE = "$E_VALUE";
    public static final String VALIDATIONVAR_FIELDNAME = "$FIELDNAME";

    public static class FileUploadLimitExceededException extends IOException {

        /**
         * 
         */
        private static final long serialVersionUID = 1L;
        
    }
    
    public static class FileUploadInputStream extends LimitedInputStream {
        
        public FileUploadInputStream(InputStream pIn, long pSizeMax) {
            super(pIn, pSizeMax);
        }

        @Override
        protected void raiseError(long pSizeMax, long pCount) throws IOException {
            throw new FileUploadLimitExceededException();
        }
        
    }
	
    public static class MultipartFormData implements Serializable {
	    
	    /**
         * 
         */
        private static final long serialVersionUID = 1L;

        private String _encoding = null;
        
        private transient List<FileItem> _fileItems = new ArrayList<FileItem>();
        private transient FileItem _formInfoItem = null;
        private transient FileItem _formActionItem = null;
        private transient FileItem _ajaxModeItem = null;
        private transient FileItem _ajaxGrayDivItem = null;
        private transient FileItem _ajaxCallIdItem = null;
        
        private transient FormParseInfo _parseInfo = null;
        private transient boolean _filesDropped = false;

        private FileItem _portletStatesItem;
        
        public boolean isValid() {
            return _fileItems != null;
        }
        
        public MultipartFormData(HttpServletRequest request, WGACore core) throws FileUploadException, IOException, OptionConversionException, InvalidEncryptionException {
            
            _encoding = core.getCharacterEncoding();
            
            ServletFileUpload dfu = new ServletFileUpload();
            TemporaryFile repository = new TemporaryFile("upload", null, core.getWgaTempDir());
            repository.getFile().delete();
            repository.getFile().mkdir();
            repository.deleteOnEviction(_fileItems);
            DiskFileItemFactory itemFactory = new DiskFileItemFactory(1024 * 10, repository.getFile());
            
            //F000037B2
            if (_encoding != null) {
                dfu.setHeaderEncoding(_encoding);
            }
            
            FileItemIterator itemStreams = dfu.getItemIterator(request);
            while (itemStreams.hasNext()) {
                FileItemStream itemStream = itemStreams.next();
                FileItem fileItem = itemFactory.createItem(itemStream.getFieldName(), itemStream.getContentType(), itemStream.isFormField(), itemStream.getName());
                
                InputStream in = itemStream.openStream();
                
                int limitMB = (Integer) core.getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_WEBTML_FILEUPLAD_MAXSIZE);
                if (_parseInfo != null) {
                    limitMB = _parseInfo.getMaxUploadSize();
                }
                
                boolean skipItem = false;
                if (!itemStream.isFormField()) {
                    if (limitMB == 0) {
                        _filesDropped = true;
                        continue;
                    }
                    else if (limitMB > 0) {
                        in = new FileUploadInputStream(in, limitMB * 1024 * 1024);
                    }
                }
                
                OutputStream out = fileItem.getOutputStream();
                try {
                    Streams.copy(in, out, true);
                }
                catch (FileUploadLimitExceededException e) {
                    _filesDropped = true;
                    core.getLog().warn("File upload for client " + request.getRemoteAddr() + " cancelled because it exceeded the size limit of " + limitMB + " MBytes");
                    try {
                        in.close();
                        out.close();
                    }
                    catch (Exception ex) {}
                    continue;
                }
                
                if (fileItem instanceof FileItemHeadersSupport) {
                    FileItemHeaders fih = itemStream.getHeaders();
                    ((FileItemHeadersSupport) fileItem).setHeaders(fih);
                }
                
                if (SYSTEMFIELD_PARSEINFO.equals(fileItem.getFieldName())) {
                    String encrypted = getDecodedItemValue(fileItem);
                    
                    byte[] zipped = null; 
                    try {
                       zipped = core.getSymmetricEncryptionEngine().decryptBase64Web(encrypted);
                    }
                    catch (Exception e) {
                        throw new InvalidEncryptionException("TMLForm ParseInfo could not be decrypted", e);
                    }
                    
                    String xml = WGUtils.unzipString(zipped);
                    FormParseInfo formParseInfo = (FormParseInfo) XStreamUtils.XSTREAM_CLONING.fromXML(xml);
                    
                    // Look if there is a process context for this parseinfo on the session, to prevent the parseinfo from being "stolen" from other sessions
                    ProcessContextRegistration contexts = TMLContext.fetchProcessContextRegistration(request.getSession());
                    if (contexts != null && contexts.getProcessContext(formParseInfo.getProcessId()) != null) {
                        _parseInfo = formParseInfo;
                    }
                    
                }
                else if (SYSTEMFIELD_FORMINFO.equals(fileItem.getFieldName())) {
                    _formInfoItem = fileItem;
                }
                else if (SYSTEMFIELD_FORMACTION.equals(fileItem.getFieldName())) {
                    _formActionItem = fileItem;
                }
                else if (SYSTEMFIELD_AJAXCALLID.equals(fileItem.getFieldName())) {
                    _ajaxCallIdItem = fileItem;
                }
                else if (SYSTEMFIELD_AJAXMODE.equals(fileItem.getFieldName())) {
                    _ajaxModeItem = fileItem;
                }
                else if (SYSTEMFIELD_AJAXGRAYDIV.equals(fileItem.getFieldName())) {
                    _ajaxGrayDivItem = fileItem;
                }
                else if (SYSTEMFIELD_PORTLETSTATES.equals(fileItem.getFieldName())) {
                    _portletStatesItem = fileItem;
                }
                else {
                    _fileItems.add(fileItem);
                }
                


            }
           
        }
    
        public List<FileItem> getFileItems() {
            return _fileItems;
        }
    
        public FileItem getFormInfoItem() {
            return _formInfoItem;
        }
    
        public FileItem getFormActionItem() {
            return _formActionItem;
        }

        public FileItem getAjaxModeItem() {
            return _ajaxModeItem;
        }

        public FileItem getAjaxGrayDivItem() {
            return _ajaxGrayDivItem;
        }

        public FileItem getAjaxCallIdItem() {
            return _ajaxCallIdItem;
        }
        
        public String getDecodedItemValue(FileItem fi) throws UnsupportedEncodingException {
            
            if (_encoding != null) {
                return fi.getString(_encoding);
            }
            else {
                return fi.getString();
            }
            
        }

        public boolean isFilesDropped() {
            return _filesDropped;
        }

        public FileItem getPortletStatesItem() {
            return _portletStatesItem;
        }
        
        
    }
	



    public static class FormParseInfo {
        
        private int _maxUploadSize = 10;
        public int getMaxUploadSize() {
            return _maxUploadSize;
        }
    
        public void setMaxUploadSize(int maxUploadSize) {
            _maxUploadSize = maxUploadSize;
        }
    
        private String _processId = null;
        
        public String getProcessId() {
            return _processId;
        }
    
        public FormParseInfo(String processContextId) {
            _processId = processContextId;
        }
    
    }




    private String _formAction = null;
	private Logger _log;
    
	private Map<String,TMLFormField> _fields = new HashMap<String,TMLFormField>();
    
	private List _errors 		= new ArrayList();
	private static final String FILE_SEPARATOR_WIN32 	= "\\";
	private static final String FILE_SEPARATOR_UNIX 	= "/";

	@CodeCompletion
	public static final String RELATION_NULLPLACE_HOLDER = "##NULL##";
	
	public static final String SYSTEMFIELD_PARSEINFO ="$parseinfo";
	public static final String SYSTEMFIELD_FORMINFO ="$forminfo";
	
    public static final String SYSTEMFIELD_AJAXCALLID = "$ajaxcallid";
    
    public static final String SYSTEMFIELD_AJAXGRAYDIV = "$ajaxgraydiv";

    public static final String SYSTEMFIELD_AJAXMODE = "$ajaxmode";
	public static final String SYSTEMFIELD_FORMACTION ="$formaction";
	public static final String SYSTEMFIELD_PORTLETSTATES ="$portletstates";
      
	// contains not submitted form information, for e.g. id, validation, messages etc.
    private TMLFormInfo _formInfo;
	
    // contains validation message by fieldname
    private Map<String,String> _messages = new HashMap<String,String>();
    
    // contains form global messages, mapped by condition
    // linkedHashmap to save order
    private Map<String,String> _globalMessages = new LinkedHashMap<String,String>();
    
    // contains custom messages
    private List<String> _customMessages = new ArrayList<String>();
    
    // flag if the form was validated in this request
    // resetted by root:tag
    private boolean _wasValidatedInThisRequest;
    
    /**
     * May be used to distinguish, if a TMLForm was submitted (true) or has just been defined for the current request
     */
    private boolean _submitted = false;
    
    /**
     * Used to determine if file uploads have been dropped bc. of to large size
     */
    private boolean _filesDropped = false;
    
    private TMLFormProcessContext _processContext;
    private Object _passwordSalt;
    private ModuleDefinition _formSourceModuleDefinition;
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getcreateddoc()
     */
	@Override
    public WGDocument getcreateddoc() {
		return getprocesscontext().getCreatedDoc();
	}
	
	/**
	 * Constructor to create a new form
	 * @param core The WGA core
	 * @param formInfo Information about the form to create
	 * @param context Target context path for the form (if not already set on forminfo)
	 * @throws WGException
	 */
	@CodeCompletion
	public TMLForm(WGACore core, TMLFormInfo formInfo, TMLContext context) throws WGException {
		
		try {
            _log = core.getLog();
            _formInfo = formInfo;
            _passwordSalt = createPasswordSalt();
            if (_formInfo.getTargetContextPath() == null) {
                _formInfo.setTargetContextPath(context.getpath());
            }
            _formSourceModuleDefinition = fetchFormSourceModuleDefinition();
            addDocumentAttachments();
            
        }
        catch (WGAPIException e) {
            throw new TMLException("Exception creating TMLForm", e, true);
        }
		
	} 
	
	private void addDocumentAttachments() throws WGAPIException {
        
	    if (!"content".equals(_formInfo.getSource()) || !_formInfo.isSyncFiles()) {
	        return;
	    }
	        
        TMLContext targetContext = gettargetcontext();
        
        if (targetContext != null && targetContext.content() != null) {
            List<String> filenames = targetContext.content().getFileNames();
            if(filenames!=null && filenames.size() > 0){
                TMLFormProcessContext pc = getprocesscontext();
	            for (String fileName : filenames) {
	                pc.addDocumentFile(targetContext.content(), fileName);
	            }
            }
        }
        
    }

    private TMLFormProcessContext retrieveProcessContext() {

	    if (_formInfo == null) {
	        throw new IllegalStateException("Trying to retrieve TMLForm process context before formInfo is available");
	    }
	    
	    ProcessContextRegistration contexts = TMLContext.getThreadMainContext().getEnvironment().getProcessContextRegistration();
        synchronized (contexts) {
            TMLFormProcessContext pContext = (TMLFormProcessContext) contexts.getProcessContext(_formInfo.getProcessId());
            if (pContext == null) {
                pContext = new TMLFormProcessContext(_formInfo.getProcessId(), _formInfo.getFormId(), contexts);
                contexts.setProcessContext(_formInfo.getProcessId(), pContext);
            }
            else {
                if (!_formInfo.getVersionCompliance().isAtLeast(6, 0) && !getforminfo().isPersistent()) {
                    pContext.clearFiles();
                }
            }
            
            
            return pContext;
        }
	    
    }

    /**
     * Constructor to read form from posted data
     * @param core The WGACore object
     * @param formData The posted data
     * @throws WGException
     * @throws UnsupportedEncodingException
     */
    @CodeCompletion
	public TMLForm(WGACore core, TMLForm.MultipartFormData formData) throws WGException, UnsupportedEncodingException {			    

		_log = core.getLog();
        _submitted = true;
        _filesDropped = formData.isFilesDropped();
        
        // Retrieve system field information
        _formInfo = retrieveFormInfo(formData, core);
        _passwordSalt = createPasswordSalt();
        _formSourceModuleDefinition = fetchFormSourceModuleDefinition();
        
        if (formData.getFormActionItem() != null) {
            _formAction = formData.getDecodedItemValue(formData.getFormActionItem());
        }

        // Parse fields into submitted fields map
        Map<String,TMLFormField> submittedFields = new HashMap<String,TMLFormField>();      
		Iterator<FileItem> iter = formData.getFileItems().iterator();

		// put all FileItemObject into Map and put all transmitted fields into field map					
		while( iter.hasNext() ) {
			FileItem fi = iter.next();

			// File items
			if( !fi.isFormField()){
                if (fi.getSize() > 0) {
                    parseFileItem(fi);
                }
			}
			
            // Standard form items
			else if(!fi.getFieldName().startsWith("$") ) {
                
			    TMLFormField field = (TMLFormField) submittedFields.get(fi.getFieldName());
				if (field == null) {
				    field = new TMLFormField(fi.getFieldName());
                    submittedFields.put(fi.getFieldName(), field);											
				}
				field.addValue(formData.getDecodedItemValue(fi));
			}
			
		}
        
        processSubmittedFields(submittedFields);
        
        retrieveCustomFields();
        
        enforceFieldDefs();
        
	}



    private TMLFormInfo retrieveFormInfo(MultipartFormData formData, WGACore core) throws TMLException, UnsupportedEncodingException {
        
        if (formData.getFormInfoItem() == null) {
            throw new TMLException("FormInfo is missing");
        }
        
        String formInfoStr = formData.getDecodedItemValue(formData.getFormInfoItem());
        
        // get FormInfo from hidden field
        // decrypt
        TMLFormInfo formInfo = null;
        byte[] serFormInfo = null;
        try {
            serFormInfo = core.getSymmetricEncryptionEngine().decryptBase64Web(formInfoStr);
            String unzipped = WGUtils.unzipString(serFormInfo);
            formInfo = (TMLFormInfo) new XStream(new Dom4JDriver()).fromXML(unzipped);
            return formInfo;
        }
        catch (Exception e) {
            throw new TMLException("FormInfo is unreadable");
        }       
    }

    private void processSubmittedFields(Map submittedFields) {
        // if form is not in edit mode - ignore posted data
        if (!_formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) {
            return;
        }                
        // iterate over submittedFields
        // if form support htmlInputs put all fields in fieldlist
        // if not put only enabled Fields in fieldlist        
        Iterator submittedIt = submittedFields.keySet().iterator();
        while (submittedIt.hasNext()) {
            String fieldname = (String) submittedIt.next();
            TMLFormField field = (TMLFormField) submittedFields.get(fieldname);
            if (_formInfo.containsFieldRegistration(fieldname)) {
                FieldReg fieldReg = _formInfo.getFieldRegistration(fieldname);
                // put only enabled fields in fieldlist
                if (fieldReg.getMode().equals(FieldReg.EDIT_MODE)) {
                    _fields.put(field.getName(), field);
                }
            } else { 
                if (_formInfo.isHtmlInput()) {
                    _fields.put(field.getName(), field);
                }
                else if (!_formInfo.getHtmlInput().trim().equalsIgnoreCase(TMLFormInfo.HTMLINPUT_IGNORE)){
                    TMLContext.getThreadMainContext().addwarning("Formfield '" + field.getName() + "' ignored. - HTML-inputs are disabled on tmlform with id '" + _formInfo.getFormId() + "'.", false);
                }
            }
            
        }       
    }
    
    
    /**
     * retrieve not submitted fields (given only by tmlscript) 
     * from formInfo and add them to fieldlist
     */
    private void retrieveCustomFields() {        
        Map<String,TMLFormField> customFields = _formInfo.getCustomFields();
        Iterator<String> customFieldnames = customFields.keySet().iterator();
        while (customFieldnames.hasNext()) {
            String customFieldname = customFieldnames.next();
            if (!_fields.containsKey(customFieldname)) {
                _fields.put(customFieldname, customFields.get(customFieldname));
            }
        }
    }
    
    /*
    private void removeNotRegisteredFields() {
        Iterator fieldnames = fields.keySet().iterator();
        while (fieldnames.hasNext()) {
            String fieldname = (String) fieldnames.next();
            if (!_formInfo.containsFieldRegistration(fieldname)) {
                fieldnames.remove();
            }
        }
    }*/

    private void parseFileItem(FileItem fi) {
        // create TempFileFolder and put TempFiles in them
        try {
            String fiName = fi.getName();
            
            // Strip of platform-specifiy file separator, prepending paths (even possible this happens?)
            if (fiName.indexOf(FILE_SEPARATOR_WIN32) != -1) {
                fiName = fiName.substring(fiName.lastIndexOf(FILE_SEPARATOR_WIN32) + 1);
            }
            // in case the file separator is UNIX-OS separator
            else {
                fiName = fiName.substring(fiName.lastIndexOf(FILE_SEPARATOR_UNIX) + 1);
            }
            
            fiName = WGUtils.normalizeUnicode(fiName);
            
            // Add to files of process context
        	getProcessContext().addFile(fi.getInputStream(), fiName);

        	// Set form field with name of the file
        	TMLFormField field = (TMLFormField) _fields.get(fi.getFieldName());
        	if( field == null ) {
                field = new TMLFormField(fi.getFieldName());
                _fields.put(fi.getFieldName(), field);             
        	}
        	field.addValue(fiName);
        	
        }
        catch (IOException e) {
        	e.printStackTrace();
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
    }

   
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#validate()
     */
    @Override
    public boolean validate() {
        // set validated flag for this request
        _wasValidatedInThisRequest = true;
        
        try {                        
            _log.debug( "***************** TMLForm validation *****************" );
            
            TMLContext formContext = getFormContext();
            
            // We ensure that the validation context points to the current TMLForm (B00005E06)
            TMLContext validationContext = new TMLContext(formContext.getdocument(), formContext.getwgacore(), formContext.getprofile(), this, formContext.getrequest(), formContext.getresponse(), formContext.gethttpsession());
            validationContext.importEnvironmentData(formContext);
            
            // keep validation status in formInfo
            _formInfo.setValidated(true);
    
            // clear messages
            this._messages.clear();
            
            // init expression engine
            RhinoExpressionEngine engine = (RhinoExpressionEngine) ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
            if (engine == null) {
                TMLContext.getThreadMainContext().addwarning("Formvalidation cannot be processed. Error initializing tmlscript-engine.", false);
                return false;
            }        
            
            // validate each field
            Iterator fieldIt = _fields.values().iterator();            
            boolean formIsValid = true;
            while (fieldIt.hasNext()) {
                TMLFormField field = (TMLFormField) fieldIt.next();
                
                // check if we have a registration for this field
                // we can only validate tml:input fields
                if (_formInfo.containsFieldRegistration(field.getName())) {
                    if (validateField(field, formContext, validationContext, engine) == false) {
                        formIsValid = false;
                    }
                }
            }
            
            // validate global validations (tml:validate tag)
            Iterator globalValidations = _formInfo.getFormValidations().keySet().iterator();
            while (globalValidations.hasNext()) {
                String expression = (String) globalValidations.next();
                TMLFormInfo.FormValidation formValidation = (TMLFormInfo.FormValidation) _formInfo.getFormValidations().get(expression);
                // check if formvalidation should be processed this time
                // all dependent fields (from attrib ifnoerror of tml:validate) are validated successfully
                boolean executeYet = true;
                Iterator ifnoerrorFields = formValidation.getIfnoerror().iterator();
                while (ifnoerrorFields.hasNext()) {
                    String fieldname = (String) ifnoerrorFields.next();
                    if (_messages.containsKey(fieldname)) {
                        executeYet = false;
                        break;
                    }
                }
                if (executeYet) {
                    ExpressionResult result = engine.evaluateExpression(expression, validationContext, ExpressionEngine.TYPE_EXPRESSION, buildValidationExpressionParams(null));                
                    if (result.isError()) {
                        formIsValid = false;
                        String errorMsg = "Validation-Expression could not be processed. Warning: " + result.getException().getMessage() + " - expression was: " + expression;
                        if (result.getException() != null) {
                            // See if there is a TMLFormValidationException "somewhere down there". If so we take it as negative validation result
                            Throwable cause = result.getException();
                            while (!(cause instanceof TMLFormValidationException) && cause.getCause() != null && cause.getCause() != cause) {
                                cause = cause.getCause();
                            }
                            
                            if (cause instanceof TMLFormValidationException) {
                                errorMsg = cause.getMessage();
                            }
                            else {
                                TMLContext.getThreadMainContext().addwarning(errorMsg, false);
                                TMLContext.getThreadMainContext().getlog().error("Error running validation expression", result.getException());
                            }
                        }

                        _log.debug(errorMsg);                                       
                        _globalMessages.put(expression, errorMsg);
                        // clear given dependent fields
                        clearFields(formValidation.getCleariferror());
                    }
                    else if (result.isFalse()) {
                        formIsValid = false;
                        // resolve scriptlets in message
                        Map params = new HashMap();
                        params.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_SCRIPTLETS);
                        String message = engine.resolveScriptlets(formValidation.getMessage(), validationContext, params);
                        _globalMessages.put(expression, message);                        
                        _log.debug("Validation result for expression '" + expression + "' is '" + result.isTrue() + "'.");
                        // clear given dependent fields
                        clearFields(formValidation.getCleariferror());                        
                    } else if (result.isTrue()) {
                        _log.debug("Validation result for expression '" + expression + "' is '" + result.isTrue() + "'."); 
                    }            
                }
            }
                
            return formIsValid;
            
        } catch (Exception e) {
            TMLContext.getThreadMainContext().addwarning("Formvalidation failed. Exception: " + e.getMessage(), false);
            _log.error("TMLFormValidation failed.", e);
            return false;
        }
    }

    private boolean validateField(TMLFormField field, TMLContext formContext, TMLContext validationContext, RhinoExpressionEngine engine) throws WGException, ParseException {
        FieldReg fieldReg = _formInfo.getFieldRegistration(field.getName());
        String fieldname = field.getName();
        String validation = fieldReg.getValidation();
        boolean multiple = fieldReg.isMultiple();      
        
        boolean fieldIsValid = true;
              
        // if we have a validation, validate field
        if ( (validation != null) && (!validation.trim().equals("")) ) {
            
            // do not validate hashedpassword fields if hashed string was posted
            if (fieldReg.getType().equals("hashedpassword")) {
                // if entered and parsed value are equal a hashed string was posted
                String enteredValue = field.getFirstEnteredStringValue();
                Object parsedValue = field.getFirstParsedValue();
                if (enteredValue != null && !enteredValue.trim().equals("")) {                            
                    if (enteredValue.equals(parsedValue)) {
                        return true;
                    }
                }
            }
                                                            
            // set tmlscript variables containing the current field value
            setValidationVariables(validationContext, field, multiple);                        
            
            List validationList = new ArrayList();
            List messageList = new ArrayList();
            
            //if a validationdivider is set for this field, tokenize validation and message
            String msg = fieldReg.getMessage();
            
            if (fieldReg.getValidationdivider() != null) {
                validationList = WGUtils.deserializeCollection(validation, fieldReg.getValidationdivider());
                if (msg == null) {
                    messageList = Collections.nCopies(validationList.size(), TMLContext.getThreadMainContext().systemLabel("tmlform", "msg_failed_validation") + fieldReg.getName());
                }
                else {
                    messageList = WGUtils.deserializeCollection(msg, fieldReg.getValidationdivider());
                }
            }
            else {
                // add single validation and message
                validationList.add(validation);
                if (msg == null) {
                    msg = TMLContext.getThreadMainContext().systemLabel("tmlform", "msg_failed_validation") + fieldReg.getName();
                }
                messageList.add(msg);
            }
            
            //process validations for field               
            for (int i = 0; i < validationList.size(); i++) {
                
                String expression = (String) validationList.get(i);
                
                String message = "";
                if (i < messageList.size()) {
                    message = (String) messageList.get(i);
                    // resolve scriptlets in message
                    Map params = new HashMap();
                    params.put(RhinoExpressionEngine.SCRIPTLETOPTION_LEVEL, RhinoExpressionEngine.LEVEL_SCRIPTLETS);
                    message = engine.resolveScriptlets(message, validationContext, params);
                } 
                
                ExpressionResult result = engine.evaluateExpression(expression, validationContext, ExpressionEngine.TYPE_EXPRESSION, buildValidationExpressionParams(fieldReg));                
                if (result.isError() || result.isFalse()) {  
                    
                    fieldIsValid = false;
                    String errorMsg = message;
                    
                    // Validation had an error itself, we put out the error cause instead of the validation message
                    if (result.isError()) {
                        errorMsg = "Validation-Expression could not be processed. Warning: " + result.getException().getMessage() + " - expression was: " + expression;
                        if (result.getException() != null) {
                            // See if there is a TMLFormValidationException "somewhere down there". If so we take it as negative validation result
                            Throwable cause = result.getException();
                            while (!(cause instanceof TMLFormValidationException) && cause.getCause() != null && cause.getCause() != cause) {
                                cause = cause.getCause();
                            }
                            if (cause instanceof TMLFormValidationException) {
                                errorMsg = cause.getMessage();
                            }
                            else {
                                formContext.addwarning(errorMsg, false);
                                formContext.getlog().error("Error running validation expression", result.getException());
                            }
                        }
                    }
                                        
                    if (WGUtils.isEmpty(errorMsg)) {
                        formContext.addwarning("No message defined for validation '" + expression + "'", false);
                    }
                    
                    _log.debug(errorMsg);                                                   
                    _messages.put(fieldname, errorMsg);
                    
                    // clear field if type is hashedpassword
                    // to ensure the validation can be executed again
                    if (fieldReg.getType().equals("hashedpassword")) {
                       field.clear();
                    }
                    // clear given dependent fields
                    clearFields(fieldReg.getCleariferror());                                                       
                    break; // stop further validation of this field
                }
                else if (result.isTrue()) {
                    _log.debug("Validation result for field '" + fieldReg.getName() + "' result of '" + expression + "' is '" + result.isTrue() + "'.");
                }                            
            }
        }
        
        // In WGA5 behaviour we automatically check for conversion errors and treat them like validation errors
        if (fieldIsValid && !WGUtils.isEmpty(field.getEnteredValues()) && getforminfo().getVersionCompliance().isAtLeast(CSConfig.getComplianceVersion(CSConfig.VERSIONCOMPLIANCE_WGA50))) {
            if (!field.couldBeParsed()) {
                fieldIsValid = false;
                
                List labelParams = new ArrayList();
                labelParams.add(field.getName());
                labelParams.add(fieldReg.getType());
                labelParams.add(WGUtils.serializeCollection(field.getEnteredValues(), ", "));
                labelParams.add(fieldReg.getFormat());
                String message;
                if (fieldReg.getFormat() != null) {
                    message = getFormContext().systemLabel("tmlform", "msg_failed_conversion_formatted", labelParams);
                }
                else {
                    message = getFormContext().systemLabel("tmlform", "msg_failed_conversion", labelParams);
                }
                
                _log.debug(message);
                _messages.put(fieldname, message);
                // clear given dependent fields
                clearFields(fieldReg.getCleariferror());   
            }
        }
        
        return fieldIsValid;
    }

    private Map buildValidationExpressionParams(FieldReg fieldReg) {
        Map exprParams = new HashMap();
        DesignResourceReference actionLocator = null;
        
        StringBuffer scriptName = new StringBuffer();
        scriptName.append("WebTML-Form '").append(getformid()).append("' ");
        
        // Add form design information if available
        if (getforminfo().getDefinitionDatabase() != null && getforminfo().getDefinitionModule() != null) {
            scriptName.append("(WebTML-Module ").append(getforminfo().getDefinitionDatabase()).append("/").append(getforminfo().getDefinitionModule()).append(") ");
            actionLocator = new DesignResourceReference(getforminfo().getDefinitionDatabase(), getforminfo().getDefinitionModule());
        }
        
        // Type of validation
        if (fieldReg != null) {
            scriptName.append("Field validation for '").append(fieldReg.getName()).append("'");
        }
        else {
            scriptName.append("Global validation");
        }
        
        // Set as params 
        exprParams.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, scriptName.toString());
        if (actionLocator != null) {
            exprParams.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, actionLocator);                                
        }
        return exprParams;
    }
    
    /**
     * clears the given list of (String) fieldnames
     * @param fieldsToClear List of Strings (fieldnames) to clear
     */
    private void clearFields(List fieldsToClear) {
        Iterator it = fieldsToClear.iterator();
        while (it.hasNext()) {
            String fieldToClear = (String) it.next();
            if (_fields.containsKey(fieldToClear)) {
                TMLFormField tmlFormFieldToClear = (TMLFormField) _fields.get(fieldToClear);
                if (tmlFormFieldToClear != null) {
                    tmlFormFieldToClear.clear();
                }
            }
        }        
    }

    private void setValidationVariables(TMLContext context, TMLFormField field, boolean multiple) throws WGAPIException {
        
        context.setvar(VALIDATIONVAR_FIELDNAME, field.getName());
        
        if (!multiple) {
            String enteredValue = field.getFirstEnteredStringValue();
            Object parsedValue = field.getFirstParsedValue();
            
            context.setvar(VALIDATIONVAR_ENTEREDVALUE, enteredValue);
            context.setvar(VALIDATIONVAR_PARSEDVALUE, parsedValue);
            _log.debug("Field: " + field.getName() + " E_VALUE=" + enteredValue + " P_VALUE=" + parsedValue);
        } 
        
        else {
            List enteredValues = field.getEnteredStringValues();
            List parsedValues = field.getParsedValues();
 
            context.setvar(VALIDATIONVAR_ENTEREDVALUE, enteredValues);
            context.setvar(VALIDATIONVAR_PARSEDVALUE, parsedValues);
            _log.debug("Field: " + field.getName() + " E_VALUE=" + WGUtils.serializeCollection(enteredValues, ",") + " P_VALUE=" + WGUtils.serializeCollection(parsedValues, ","));
        }
    }    
    
    
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getformid()
     */
	@Override
    public String getformid() {
		return _formInfo.getFormId();
	}

    /**
     * Iterate over lastRenderedFields, convert values by type and store as parsedValue, 
     * fill empty fields
     */
	private void enforceFieldDefs() {        
        Iterator<String> lastRenderedFields = _formInfo.getLastRenderedFormFields().iterator();    
        while (lastRenderedFields.hasNext()) {
            String currentFieldname = lastRenderedFields.next();
            if (currentFieldname.trim().equals("")) {
                continue;
            }
            
            FieldReg currentFieldReg = _formInfo.getFieldRegistration(currentFieldname);   
            TMLFormField field = _fields.get(currentFieldReg.getName()); 
            
            // Operations to do if the registered field was not submitted
            if (field == null) {
                
                // Simulate the submit of an empty field for HTML controls where submit may have been suppressed on empty value
                if (currentFieldReg.getType().equalsIgnoreCase("checkbox") || 
                        currentFieldReg.getType().equalsIgnoreCase("radio") || 
                        currentFieldReg.getType().equalsIgnoreCase("boolean") ||
                        currentFieldReg.getType().equalsIgnoreCase("select")) {
                    
                    field = new TMLFormField(currentFieldReg.getName());
                    
                    // Empty boolean fields default to false
                    if (currentFieldReg.getType().equalsIgnoreCase("boolean")) {
                        field.setValue(Boolean.FALSE);
                    }
                    
                    _fields.put(currentFieldReg.getName(), field);
                    
                }
    			
            }
		}
        _formInfo.clearLastRenderedFormFields();
        
        
        // Create parsed values from entered values
        _errors.clear();
        Iterator<TMLFormField> fields = _fields.values().iterator();
        while (fields.hasNext()) {            
            TMLFormField field = fields.next();
            FieldReg reg = _formInfo.getFieldRegistration(field.getName());
            if (reg != null) {
                try {
                    parseField(reg, field);
                }
                catch (Exception e) {
                    _log.error("Exception parsing field", e);
                }
            }
        }
        
        
	}

    private void parseField(FieldReg fieldReg, TMLFormField field) throws WGException {
        String format = fieldReg.getFormat();
        
        // we might have to trim field values
        if (fieldReg.isTrim() || _formInfo.isTrim()) {
            field.trim();
        }                    
        
        field.setMultiple(fieldReg.isMultiple());
        
        if( fieldReg.getType().equalsIgnoreCase("textarea") && fieldReg.isMultiple()) {
            field.setType(TMLFormField.TYPE_TEXTAREA);
            field.setMultipleDivider("\r\n");
            field.parse(this);
        }
        else if(fieldReg.getType().equalsIgnoreCase("date")) { 
        	convertDateValues(format, field);
        }
        else if(fieldReg.getType().equals("number")) {
        	convertNumberValues(format, field);
        }
        else if(fieldReg.getType().equals("boolean")) {
            field.setType(TMLFormField.TYPE_BOOLEAN);
        	field.parse(this);
        } 
        else if (fieldReg.getType().equals("hashedpassword")) {
            convertHashedPassword(fieldReg, field);                            
        }
    }

    private void convertHashedPassword(FieldReg currentFieldReg, TMLFormField field) 
        throws WGException {
        field.setType(TMLFormField.TYPE_HASHEDPASSWORD);
        // check if field is already hashed
        TMLFormField hashedField = _formInfo.getHashedPasswordField(currentFieldReg.getName());
        if (hashedField != null) {
            // check if the user has changed the field
            Object postedValue = field.getFirstEnteredValue();
            Object renderedValue = hashedField.getFirstEnteredValue();
            if (postedValue != null && renderedValue != null) {
                if (postedValue.equals(renderedValue)) {
                    // user did not change the field,
                    // so we do not need to hash it
                } else {
                    // user changed the field, we have to hash
                    field.parse(this);
                }
            }
        } else {
            // field is not yet registered, so we have to hash
            field.parse(this);
        }
    }
    
    private void convertNumberValues(String format, TMLFormField field) {        
        field.setType(TMLFormField.TYPE_NUMBER);        
        try {
            field.setFormat(WGA.get(TMLContext.getThreadMainContext()).getNumberFormat(format, null));
            field.parse(this);
        }
        catch (WGException e) {
            String errMsg = "Cannot parse '" + field.getFirstEnteredStringValue() + " as number: " + e.getMessage();
            _errors.add(errMsg);            
        }
    }    
    
    private void convertDateValues(String format, TMLFormField field) throws WGException {
        field.setType(TMLFormField.TYPE_DATE);
        field.setFormat(WGA.get(TMLContext.getThreadMainContext()).getDateFormat(format, null));
        
        try {
            field.parse(this);
        }
        catch (TMLFormParsingException e) {
            String errMsg = "Cannot parse '" + field.getFirstEnteredStringValue() + "' as date format " + format + ": " + e.getMessage();
            _errors.add(errMsg);            
        }
    }
     
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getfieldnames()
     */
	@Override
    public List<String> getfieldnames() {
		return new ArrayList<String>(this._fields.keySet());
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#hasfield(java.lang.String)
     */
	@Override
    public boolean hasfield(String name) {
		return this._fields.containsKey(name);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#field(java.lang.String)
     */
	@Override
    public Object field(String name) {	
		if (hasfield(name)) {
		    FieldReg fieldReg = _formInfo.getFieldRegistration(name);
		    boolean flattenList = true; 
		    if (_formInfo.getVersionCompliance().isAtLeast(7,2)) { // New list flattening behavour is dependent on the field registrations multiple property (#00004611)
		        flattenList = (fieldReg == null || !fieldReg.isMultiple());
		    }
            Object value = TMLContext.flattenList(fieldlist(name), flattenList);
            if (value == null) {
                value = getFormContext().db().getNoItemBehaviour().getForTMLFormEmptyField(); 
            }
            return value;
        }
        else {
            return getFormContext().db().getNoItemBehaviour().getForTMLFormField();
        }
        	
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#setfield(java.lang.String, java.lang.Object)
     */
	@Override
    public void setfield(String name, Object value) throws WGException {		
        TMLFormField field = (TMLFormField)  _fields.get(name);
        if (field == null) {
            field = new TMLFormField(name);
        }
		field.setValue(value);
        _fields.put(name, field);
                
        // put field also in formInfo.customFields, 
        // to support not submitted but via tmlscript given fields
        _formInfo.addCustomField(field);
        
        // if we have a fieldReg -> we should parse the field
        if (_formInfo.containsFieldRegistration(name)) {
            FieldReg fieldReg = _formInfo.getFieldRegistration(name);
            this.parseField(fieldReg, field);
        }        
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#appendtofield(java.lang.String, java.lang.Object)
     */
	@Override
    public void appendtofield(String name, Object value) throws WGException {
        TMLFormField field = (TMLFormField) _fields.get(name);
        if (field != null)  {
            field.addValue(value);
        } else {
        	//B000045F6
        	setfield(name, value);
        }
    }
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#removefield(java.lang.String)
     */
	@Override
    public void removefield(String name) {		
		this._fields.remove(name);
		//this.metaFields.remove(name);        
        _formInfo.removeFieldReg(name);
        // field might be a customfield, set by tmlscript  - so remove this too
        _formInfo.removeCustomField(name);   

        _formInfo.getLastRenderedFormFields().remove(name);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#fieldlist(java.lang.String)
     */
	@Override
    public List fieldlist(String name) {
		TMLFormField field = (TMLFormField) _fields.get(name);
        if (field != null) {
            if (!field.couldBeParsed()) {
                return getFormContext().db().getNoItemBehaviour().getForTMLFormEmptyFieldList();
            } else {
                return field.getParsedValues();
            }
        } else {
            return getFormContext().db().getNoItemBehaviour().getForTMLFormFieldList();
        }	
	}

    private TMLContext getFormContext() {
        TMLContext cx = TMLContext.getThreadMainContext();
        if (_formInfo.getTargetContextPath() != null) {
            cx = cx.context(_formInfo.getTargetContextPath());
        }
        return cx;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#parsedvalue(java.lang.String)
     */
    @Override
    public Object parsedvalue(String fieldname) {
        TMLFormField field = (TMLFormField) this._fields.get(fieldname);
        if (field != null) {
            if (field.isMultiple()) {
                return field.getParsedValues();
            } else {
                return field.getFirstParsedValue();
            }
        } else {
            return null;
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#enteredvalue(java.lang.String)
     */    
    @Override
    public Object enteredvalue(String fieldname) {
        TMLFormField field = (TMLFormField) this._fields.get(fieldname);
        if (field != null) {
            if (field.isMultiple()) {
                return field.getEnteredStringValues();
            } else {
                return field.getFirstEnteredStringValue();
            }
        } else {
            return null;
        }
    }
    
    /**
     * returns the valuelist for the given fieldname
     * the valuelist contains the parsedvalue, if successfully parsed
     * otherwise the valuelist contains the enteredvalue
     * @param fieldname
     * @return
     */
    @CodeCompletion
    public List getEnteredOrParsedValues(String fieldname) {
        TMLFormField field = (TMLFormField) _fields.get(fieldname);
        if (field != null) {
            List parsedValues = field.getParsedValues();
            if (parsedValues.contains(null)) {
                List enteredOrParsed = new ArrayList();
                for (int i=0; i<parsedValues.size(); i++) {                    
                    Object parsedValue = parsedValues.get(i);
                    if (parsedValue == null) {
                        enteredOrParsed.add(field.getEnteredValues().get(i));
                    } else {
                        enteredOrParsed.add(parsedValue);
                    }
                }
                return enteredOrParsed;
            } else {
                return parsedValues;
            }
        } else {
            return null;
        }
    }
	
	public boolean storeinprofile(TMLUserProfile profile) throws WGException {

        if (!this.validate()) {
            return false;
        }

        UserProfileFormSource fs = new UserProfileFormSource();
        fs.init(TMLContext.getThreadMainContext());
        fs.setProfile(profile);
        return fs.storeForm(this, false);
	    
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storeinprofile()
     */
	@Override
    public boolean storeinprofile() throws WGException {

        if (!this.validate()) {
            return false;
        }

        FormSource fs = new UserProfileFormSource();
        fs.init(TMLContext.getThreadMainContext());
        return fs.storeForm(this, false);

	    
	}
	
	public boolean storeindocument(WGDocument doc) throws WGException {
		
        if (!this.validate()) {
            return false;
        }
        
        FormSource formSource = new ContextDocumentFormSource();
        formSource.init(getFormContext().context(doc));
        return formSource.storeForm(this, false);

	}
	


    public void pushFields(WGDocument doc) throws WGAPIException {
        Iterator fieldNames = this.getfieldnames().iterator();
	
		String fieldName;
		while (fieldNames.hasNext()) {
			fieldName = (String) fieldNames.next();
            storefield(fieldName, doc);            		
		}
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storefield(java.lang.String, de.innovationgate.webgate.api.WGDocument)
     */
    @Override
    public void storefield(String fieldName, WGDocument doc) throws WGAPIException {
        storefield(fieldName, doc, null);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storefield(java.lang.String, de.innovationgate.webgate.api.WGDocument, java.lang.String)
     */
    @Override
    public void storefield(String fieldName, WGDocument doc, String targetName) throws WGAPIException {
        FieldReg fieldReg = _formInfo.getFieldRegistration(fieldName);
        TMLFormField field = (TMLFormField) _fields.get(fieldName);
        
        if (targetName == null) {
            targetName = fieldName;
        }
        
        // Fields with field registration
        if (fieldReg != null) {
            
            // store only fields with store==true
            if (fieldReg.isStore()) {
                
                // Metadata field
                if (fieldReg.isMeta()) {
                    // B000041FA
                    if (field.couldBeParsed()) {
                        if (fieldReg.isMultiple()) {
                            doc.setMetaData(targetName.toUpperCase(),this.fieldlist(fieldName));
                        }
                        else {
                            doc.setMetaData(targetName.toUpperCase(),this.field(fieldName));
                        }
                    }
                    else {
                        doc.setMetaData(targetName.toUpperCase(), null);
                    }
                }
                

                else {
                    String relType = fieldReg.getRelationtype();
                    
                    // Relation field
                    if (relType != null && doc instanceof WGContent) {
                    
                        int relTypeInt = relType.equalsIgnoreCase("protected") ? WGContent.RELATIONTYPE_PROTECTED : WGContent.RELATIONTYPE_NORMAL;
                        
                        WGContent content = (WGContent) doc;
                    	
                    	// Relation field is filled
                    	if (!isempty(fieldName)) {
                    	    List<String> relationStrings = (List<String>) fieldlist(fieldName);
                    	    if (fieldReg.isMultiple()) {
                    	        content.clearRelationGroup(targetName);
                    	        for (String relStr : relationStrings) {
                    	            TMLContext relationContext = gettargetcontext().context("docid:"+ relStr, false);
                                    if (relationContext != null) {
                                        WGContent targetContent = relationContext.content();
                                        if (!targetContent.getStatus().equals(WGContent.STATUS_RELEASE)) {
                                            targetContent = targetContent.getStructEntry().getReleasedContent(targetContent.getLanguage().getName());
                                        }
                                        if (targetContent != null) {
                                            content.addRelationToGroup(targetName, targetContent, relTypeInt);
                                        }
                                    }   
                    	        }
                    	    }
                    	    else if (relationStrings.size() > 0) { // Need to test bc. of old isempty() behaviour < 6.2
                    	        TMLContext relationContext = gettargetcontext().context("docid:"+ relationStrings.get(0), false);
                    	        if (relationContext != null) {
                    	            WGContent targetContent = relationContext.content();
                                    if (!targetContent.getStatus().equals(WGContent.STATUS_RELEASE)) {
                                        targetContent = targetContent.getStructEntry().getReleasedContent(targetContent.getLanguage().getName());
                                    }
                                    if (targetContent != null) {
                                        content.setRelation(targetName, targetContent, relTypeInt);
                                    }
                    	        }
                    	    }
                    	}
                    	
                    	// Relation field is empty
                    	else {
                    	    if (fieldReg.isMultiple()) {
                    	        content.clearRelationGroup(targetName);
                    	    }
                    	    else {
                    	        content.removeRelation(targetName);
                    		}
                        }   
                    }
                    
                    // Plain item field
                    else {
                        if (fieldReg.isMultiple()) {
                            doc.setItemValue(targetName, this.fieldlist(fieldName));
                        }
                        else {
                            doc.setItemValue(targetName, this.field(fieldName));
                        }
                    }
                }
            }
        }

        // store fields without fieldreg e.g. customFields. Simulate standard behaviour of JDBC list storage prior to #00001362
        else {

            List valueList = this.fieldlist(fieldName);
            if (valueList.size() == 0) {
                doc.setItemValue(targetName, null);
            }
            else if (valueList.size() == 1) {
                doc.setItemValue(targetName,  valueList.get(0));
            }
            else {
                doc.setItemValue(targetName, valueList);
            }
        }
    }

    public void processrtf(String fieldName) throws WGException {
        
        String str = String.valueOf(this.field(fieldName));

        // Extract data URLs
        str = WGUtils.strReplace(str, "src=\"data:", new ReplaceProcessor() {
            
            @Override
            public int replace(String text, int from, int to, Writer out) throws IOException {

                int linkEnd = text.indexOf("\"", to);
                
                if (linkEnd != -1) {
                    try {
                        String fileName = extractFileFromDataURL(text.substring(from + 10, linkEnd));
                        if (fileName != null) {
                            out.write(getExtractedFileHTML(fileName, true));
                            return linkEnd + 1;
                        }
                    }
                    catch (Exception e) {
                        // Fail silently here. 
                    }
                }
                
                // Unparseable. Do not modify.
                out.write("s");
                return from + 1;
                
            }


            
        }, true);
        
        str = WGUtils.strReplace(str, "href=\"data:", new ReplaceProcessor() {
            
                @Override
                public int replace(String text, int from, int to, Writer out) throws IOException {

                    int linkEnd = text.indexOf("\"", to);
                    
                    if (linkEnd != -1) {
                        try {
                            String fileName = extractFileFromDataURL(text.substring(from + 11, linkEnd));
                            if (fileName != null) {
                                out.write(getExtractedFileHTML(fileName, false));
                                return linkEnd + 1;
                            }
                        }
                        catch (Exception e) {
                            // Fail silently here. Will return the url unmodified.
                        }
                    }
                    
                    // Unparseable. Do not modify.
                    out.write("h");
                    return from + 1;
                }
                
        }, true);
        
        setfield(fieldName, str);
        
    }

    private WGRelationData createRelationData(String relName, String relType, WGContent parentContent, String relationStr) throws WGAPIException, WGIllegalArgumentException {
        WGRelationData relData = null;
        TMLContext relationContext = gettargetcontext().context("docid:"+ relationStr, false);
        if (relationContext != null) {
        	if (relType.equalsIgnoreCase("normal")) {
        	    relData = new WGRelationData(parentContent.getDatabase(), parentContent.getContentKey(), relName, relationContext.content().getStructKey(), relationContext.content().getLanguage().getName(), WGContent.RELATIONTYPE_NORMAL, null);
        	}
        	else if (relType.equalsIgnoreCase("protected")) {
        	    relData = new WGRelationData(parentContent.getDatabase(), parentContent.getContentKey(), relName, relationContext.content().getStructKey(), relationContext.content().getLanguage().getName(), WGContent.RELATIONTYPE_PROTECTED, null);
        	} 
        	else {
        		throw new WGIllegalArgumentException("Unknown relationtype '" + relType + "'.");
        	}
        }
        else {
        	throw new WGIllegalArgumentException("Relation context lookup failed for: field '" + relName + "' relationdata '" + relationStr + "'.");
        }
        return relData;
    }	
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storeincontent(de.innovationgate.webgate.api.WGContent)
     */
	@Override
    public boolean storeincontent(WGContent content) throws WGException {
		return storeindocument(content);
	}
	
	public boolean storeinhdb() throws WGException {
	    return storeinhdb(null);
	}
	
	public boolean storeinhdb(Object parameter) throws WGException {

        if (!this.validate()) {
            return false;
        }
	    
	    // The reference document of the HDBModel operation
        TMLContext targetContext = gettargetcontext();
        WGContent content = gettargetcontext().content();
        
        // Get the model
        HDBModel model = (HDBModel) targetContext.db().getAttribute(WGACore.DBATTRIB_HDBMODEL);
        if (model == null) {
            throw new TMLScriptException("Cannot use tmlform.storeInHDB() in database " + targetContext.db().getDbReference() + " where HDB model is not available");
        }
        
        return model.storeForm(this, content);
        
    } 
		
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storeincontent()
     */
	@Override
    public boolean storeincontent() throws WGException, IOException {
		
	    if (!this.validate()) {
            return false;
        }
	    
        // Retrieve the orginal doc for which the form was created
        TMLContext targetContext = gettargetcontext();
                
		if (targetContext != null) {
		    FormSource formSource = new ContextDocumentFormSource();
		    formSource.init(targetContext);
		    return formSource.storeForm(this, false);
		}
		else {
			throw new WGIllegalStateException("Unable to store form, because it's target content could not be retrieved: " + _formInfo.getTargetContextPath());
		}
	}
	


	@CodeCompletion
    public TMLContext gettargetcontext() {
        TMLContext targetContext;
        targetContext = TMLContext.getThreadMainContext().context(_formInfo.getTargetContextPath(), false);
        return targetContext;
    }
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storeinportlet(de.innovationgate.wgpublisher.webtml.utils.TMLPortlet)
     */
	@Override
    public boolean storeinportlet(Portlet portlet) throws WGException {
	    
        if (!this.validate()) {
            return false;
        }
		
		if (portlet == null) {
		    TMLContext.getThreadMainContext().addwarning("Cannot find portlet to store data for storeinportlet()", false);
		}
		
		PortletConfigFormSource fs = new PortletConfigFormSource();
        fs.init(TMLContext.getThreadMainContext());
        fs.setPortlet((TMLPortlet) portlet);
        return fs.storeForm(this, false);

	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#storeinportlet()
     */
	@Override
    public boolean storeinportlet() throws WGException {
	    
        if (!this.validate()) {
            return false;
        }
	    
	    FormSource fs = new PortletConfigFormSource();
	    fs.init(TMLContext.getThreadMainContext());
	    return fs.storeForm(this, false);

	}

	@CodeCompletion
	public boolean writecsv( String destination, boolean includeHeaders, String delimiter ) {
		return CSVWriter.writeCSV( destination, this, includeHeaders, delimiter );
	}
    
	@CodeCompletion
    public boolean writecsv( String destination, boolean includeHeaders) {
        return CSVWriter.writeCSV( destination, this, includeHeaders, null );
    }


	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#geterrors()
     */
	@Override
    public List geterrors() {
		return _errors;
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getfilenames()
     */
	@Override
    public List getfilenames() {
	
		return new ArrayList(getProcessContext().getFiles().keySet());
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getfile(java.lang.String)
     */
	@Override
    public File getfile(String name) throws WGAPIException, IOException {
		PCFile pcFile = getProcessContext().getFiles().get(name.toLowerCase());
		if (pcFile != null) {
		    return pcFile.getDiskFile();
		}
		else {
		    return null;
		}
	}

	public long getfilesize(String name) throws IOException {
		PCFile pcFile = getProcessContext().getFiles().get(name.toLowerCase());
		if (pcFile != null) {
		    return pcFile.getSize();
		}
		else {
		    return -1;
		}
	}

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getfiletext(java.lang.String)
     */
    @Override
    public String getfiletext(String name) throws IOException, WGAPIException {
        
        PCFile file = getProcessContext().getFiles().get(name.toLowerCase());
        if (file == null) {
            return null;
        }
        
        Reader reader = new BufferedReader(new InputStreamReader(file.getData()));
        StringWriter writer = new StringWriter();
        WGUtils.inToOut(reader, writer, 2048);
        reader.close();
        return writer.toString();
        
    }

	
	public boolean attach( TMLUserProfile obj ) throws IOException, WGAPIException {
		return this.attach( obj.getprofile() );
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#attach(de.innovationgate.webgate.api.WGDocument, java.lang.String)
     */
    public synchronized boolean attach( WGDocument doc , String altFileName ) throws IOException, WGAPIException {
		
		if( altFileName != null && !altFileName.equals("") ) {
			// only the first file, because -> only one filename given
			PCFile file = getProcessContext().getFiles().values().iterator().next();
			if (!file.getName().equals(altFileName)) {
				TemporaryFile targetFile = new TemporaryFile(altFileName, file.getData(), WGFactory.getTempDir());
				targetFile.deleteOnEviction(doc.getDatabase().getSessionContext());
                boolean result = doc.attachFile( targetFile.getFile() );
                return result;
			}
			else {
			    return doc.attachFile(file.getData(), file.getName());
			}


		}
		else {
		    pushFiles(doc);
            return true;
		}
	}

    public boolean pushFiles(WGDocument doc) throws WGAPIException, IOException {
        
        boolean anythingPushed = false;
        Iterator<PCFile> fileIter = getProcessContext().getFiles().values().iterator();
        for ( PCFile file : getprocesscontext().getFiles().values()) {
        	
        	// Look if file already attached. Skip if file is identical or cannot be determined bc. too low CS version
        	if (doc.hasFile(file.getName())) {
        	    if (doc.getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA4_1 || file.getMd5Checksum().equals(doc.getFileMetaData(file.getName()).getMd5Checksum())) {
        	        continue;
        	    }
        	    
        	    doc.removeFile(file.getName());
        	}
        	
        	doc.attachFile(file.getData(), file.getName());
        	anythingPushed = true;
        }
        
        return anythingPushed;
    }
    
    public boolean retainFiles(WGDocument doc) throws WGAPIException {
        
        boolean anythingRetained = false;
        if (_formInfo.getVersionCompliance().isAtLeast(6, 0)) {
            List<String> docFileNames = doc.getFileNames();
            docFileNames.removeAll(getfilenames());
            for (String deletedFile : docFileNames) {
                doc.removeFile(deletedFile);
                anythingRetained = true;
            }
        }
        return anythingRetained;
        
    }
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#attach(de.innovationgate.webgate.api.WGDocument)
     */
	@Override
    public boolean attach( WGDocument doc ) throws IOException, WGAPIException {		
		return attach( doc , null );		
	}
	
	@CodeCompletion
	public long attachmentsize( String fieldname ) throws IOException {
	    return getfilesize(fieldname);
	}
	
	@CodeCompletion
	public void attachimage( WGDocument doc, int size ) {
		String sSize = String.valueOf(size);	 
		attachimage( doc, null , "true", null, sSize, sSize );
	}	
	
	@CodeCompletion
	public void attachimage( WGDocument doc, String fit2size, String compression, String width, String height ) {
		attachimage( doc, null , fit2size, compression, width, height );		
	}
    
	@CodeCompletion
    public void attachscaledimage(WGDocument doc, ImageScaler scaler, String targetFileName) throws IOException, WGAPIException {               
        getFormContext().attachscaledimage(doc, scaler, targetFileName);
    }
    
	@CodeCompletion
    public void attachscaledimage(WGDocument doc, ImageScaler scaler) throws IOException, WGAPIException {
        attachscaledimage(doc, scaler, null);
    }
    
    
    

    

	
	public void attachimage( WGDocument doc, String altFileName, String fit2size, String compression, String width, String height ) {
		
		if( width == null || width.equals("") ){
			width = "0";			
		}
		if( height == null || height.equals("") ){
			height = "0";			
		}
		
		Float fCompression = null;		
		if( compression != null && !compression.equals("") ){
			fCompression = new Float( compression );
		}
		
		
		try {		
			Iterator<PCFile> fileIter = getProcessContext().getFiles().values().iterator();
			
			while( fileIter.hasNext() ) {
				PCFile file = fileIter.next();
				attachSingleFile(doc, altFileName, fit2size, width, height, fCompression, file.getDiskFile());
			}
		}
		catch( Exception e ) {
			e.printStackTrace();
		}
	}	
	
	
	
	private void attachSingleFile(WGDocument doc, String altFileName, String keepRatio, String width, String height, Float fCompression, File file) throws NumberFormatException, IOException, WGException {

	    // Determine if image file is of valid format
	    String fileExt = file.getAbsolutePath().substring( file.getAbsolutePath().lastIndexOf(".") + 1 );
        if( !fileExt.equalsIgnoreCase("jpg") &&
        	!fileExt.equalsIgnoreCase("jpeg") && 
        	!fileExt.equalsIgnoreCase("bmp") &&
        	!fileExt.equalsIgnoreCase("gif") &&
        	!fileExt.equalsIgnoreCase("png") &&
        	!fileExt.equalsIgnoreCase("tif") &&
        	!fileExt.equalsIgnoreCase("tiff") &&
        	!fileExt.equalsIgnoreCase("fpx") ) {
            WGFactory.getLogger().error( "WGA Imaging API Error: wrong file type!" );
            return;
        }
	    
	    // Determine target file
	    String targetFileName = null;
	    if( altFileName != null && !altFileName.equals("") && !file.getName().equals(altFileName) ) {
	    	targetFileName = altFileName;
	    }
	    else {
	        String fileName = file.getName();
	        targetFileName = fileName.substring(0, fileName.lastIndexOf(".")) + ".jpg";
	    }
	    
	    TemporaryFile tempTargetFile = new TemporaryFile(targetFileName, null, WGFactory.getTempDir());
        tempTargetFile.deleteOnEviction(doc.getDatabase().getSessionContext());     
        File targetFile = tempTargetFile.getFile();
        	
        ImageScaler scaler = TMLContext.getThreadMainContext().createimagescaler(file);
        scaler.useJPEGForOutput();        
        scaler.setQuality(fCompression.floatValue());
        
        scaler.scaleToSize(Integer.parseInt(width), Integer.parseInt(height), new Boolean(keepRatio).booleanValue());
        
        // Write scaled image to target file
        scaler.writeImage(targetFile);
        
        doc.attachFile(targetFile);	
    }

    /**
	 * @return
	 */
	@CodeCompletion
	public String getformaction() {
	    
		if(!WGUtils.isEmpty(_formAction)) {
		    return _formAction;
		}
		else if (_formInfo.getDefaultAction() != null) {
		    return String.valueOf(_formInfo.getDefaultAction());
		}
		else {
		    return null;
		}
		
	}
	
	/**
	 * @param form
	 * @throws WGAPIException 
	 */
	public void importForm(TMLForm form) throws WGAPIException {
							
		_fields.putAll(form._fields);
		//metaFields.addAll(form.metaFields);		
		
		_formAction = form._formAction;
		_errors = form._errors;		
		_passwordSalt = form._passwordSalt;
		// documentPath = form.documentPath; // Document path should not be imported. Redefinition of it only possible via <tml:form>-Tag
	
        if (this._formInfo != null) {
            if (_formInfo.importFormInfo(form.getforminfo())) {
                reset();
            }
        }
        else {
            _formInfo = form.getforminfo();
        }
        
        //B00004D82
        _submitted = form._submitted;
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#reset()
     */
	@Override
    public void reset() throws WGAPIException {
        _formInfo.reset();
        
		_errors.clear();
		_fields.clear();
		
		getprocesscontext().reset();
		addDocumentAttachments();
		
        clearmessages();
		_formAction = null;
		//B0000471A
		_submitted = false;
		//_createdDoc = null;
		
		try {
		    _formSourceModuleDefinition = fetchFormSourceModuleDefinition();
		} catch (WGException e) {
		    throw new WGAPIException(e);
		}
	}

	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#remove()
     */
	@Override
    public void remove() {
		getFormContext().removetmlform(getformid());
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#iseditable()
     */
	@Override
    public boolean iseditable() {
        if (_formInfo.getMode().equals(TMLFormInfo.EDIT_MODE)) {
            return true;
        } else {
            return false;
        }
	}

	/**
     * orders messages by their fieldRegistration-order
     * this is necessary to display errors in form fields order
     * Nots: there is no guarantee that the order of submitted fields is correct
     *       so this reordering is better
     * @param messages
     * @return
     */
    private List<String> orderByFieldRegistration(Map<String,String> messages) {
        Iterator<FieldReg> fieldRegs = _formInfo.getFieldRegistrations().iterator();
        List<String> orderedMessages = new ArrayList<String>();
        while (fieldRegs.hasNext()) {
            FieldReg fieldReg = (FieldReg) fieldRegs.next();
            if (messages.containsKey(fieldReg.getName())) {
                orderedMessages.add(messages.get(fieldReg.getName()));
            }
        }
        return orderedMessages;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#ispersistent()
     */
    @Override
    public boolean ispersistent() {        
        return _formInfo.isPersistent();
    }

    /**
     * @param persistent The persistent to set.
     */
    /*
    public void setPersistent(boolean persistent) {
        this.persistent = persistent;
    }*/
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getmessages()
     */
    @Override
    public List<String> getmessages() {
        // merge field- and global messages
        List<String> allMessages = new ArrayList<String>();
        // reorder _messages because there is not guarantee that the HTML-Order is correct
        allMessages.addAll(this.orderByFieldRegistration(_messages));       
        allMessages.addAll(getglobalmessages());
        return allMessages;
    }
    
    public List<String> getglobalmessages() {

        List<String> globalMessages = new ArrayList<String>();
        globalMessages.addAll(_globalMessages.values());
        globalMessages.addAll(_customMessages);
        return globalMessages;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getmessage(java.lang.String)
     */
    @Override
    public String getmessage(String field) {
        return (String) this._messages.get(field);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#hasmessages()
     */
    @Override
    public boolean hasmessages() {
        boolean hasGlobalMessages = !_globalMessages.isEmpty();
        boolean hasFieldMessages = !_messages.isEmpty();
        boolean hasCustomMessages = !_customMessages.isEmpty();
        return (hasGlobalMessages || hasFieldMessages || hasCustomMessages);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#hasmessage(java.lang.String)
     */
    @Override
    public boolean hasmessage(String field) {
        if (this._messages.get(field) != null) {
            return true;
        } else {
            return false;
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#addmessage(java.lang.String)
     */
    @Override
    public void addmessage(String message) {
        _customMessages.add(message);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#setmessage(java.lang.String, java.lang.String)
     */
    @Override
    public void setmessage(String fieldName, String message) {
        this._messages.put(fieldName, message);
    }

    /**
     * checks if the given condition (globalFormValdation) failed
     * used by tml:validate to decide to display the message or not 
     * @param condition
     * @return
     */
    @CodeCompletion
    public boolean conditionFailed(String condition) {
        return _globalMessages.containsKey(condition);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getforminfo()
     */
    @Override
    @CodeCompletion
    public TMLFormInfo getforminfo() {
        return _formInfo;
    }

    @CodeCompletion
    public void setFormInfo(TMLFormInfo formInfo) {
        _formInfo = formInfo;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#clearmessages()
     */
    @Override
    public void clearmessages() {
        _messages.clear();
        _globalMessages.clear();
        _customMessages.clear();
        // reset validation status
        _formInfo.setValidated(false);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getinvalidfields()
     */
    @Override
    public List getinvalidfields() {
        List list = new ArrayList();
        list.addAll(_messages.keySet());
        return list; 
    }
    
    /**
     * returns the sourcetype ('content', 'portlet', etc.) of the form
     * @return
     */
    public String getsource() {
        return _formInfo.getSource();
    }
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#source()
     */
	@Override
    public String source() {
        return getsource();
    }

    /**
     * returns the formmode
     * @return 'edit', 'view', 'readonly'
     */
    public String getmode() {
        return _formInfo.getMode();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#mode()
     */
    @Override
    public String mode() {
        return getmode();
    }

    @CodeCompletion
    public boolean wasValidatedInThisRequest() {
        return _wasValidatedInThisRequest;
    }
    
    @CodeCompletion
    public void setWasValidatedInThisRequest(boolean wasValidatedInThisRequest) {
        _wasValidatedInThisRequest = wasValidatedInThisRequest;
    }

    @CodeCompletion
    public void importServicesForm(de.innovationgate.wgaservices.types.Form form) throws WGException {

        // Import fields
        Iterator<String> fieldNames = form.fieldNames().iterator();
        while (fieldNames.hasNext()) {
        	String fieldname = fieldNames.next();
            try {
                setfield(fieldname, form.getField(fieldname).getValues());
            }
            catch (TMLFormParsingException e) {
                // Should never happen, as services forms are not imported to forms with field registrations
                TMLContext.getThreadMainContext().addwarning("Cannot parse field " + fieldname + ": " + e.getMessage());
            }
        }
        
        // Import attachments
        Iterator<String> attNames = form.attachmentNames().iterator();
        while (attNames.hasNext()) {
        	String name = attNames.next();
            DataSource data = form.attachmentData(name);
            ServicesFileItem servicesFileItem = new ServicesFileItem(name, data);
            parseFileItem(servicesFileItem);
        }
        
    }
    
    @CodeCompletion
    public de.innovationgate.wgaservices.types.Form exportServicesForm() throws IOException, WGAPIException {

        de.innovationgate.wgaservices.types.Form servicesForm = new de.innovationgate.wgaservices.types.Form(getformid());
        servicesForm.setTrim(getforminfo().isTrim());
        
        // Export fields
        Iterator fieldNames = getfieldnames().iterator();
        while (fieldNames.hasNext()) {
            String fieldName = (String) fieldNames.next();
            servicesForm.setField(fieldName, new ArrayList<Object>(fieldlist(fieldName)));
        }
        
        // Export files
        Iterator<Map.Entry<String,PCFile>> files = getProcessContext().getFiles().entrySet().iterator();
        while (files.hasNext()) {
            Map.Entry<String,PCFile> entry = files.next();
            servicesForm.addFileAsAttachment(entry.getValue().getDiskFile());
        }
        
        return servicesForm;
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#issubmitted()
     */
    @Override
    public boolean issubmitted() {
        return _submitted;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#removefile(java.lang.String)
     */
    @Override
    public boolean removefile(String filename) {
        return getProcessContext().removeFile(filename);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#addfile(java.io.File, java.lang.String)
     */
    @Override
    public void addfile(File source, String fileName) throws IOException, NoSuchAlgorithmException {
        getProcessContext().addFile(new BufferedInputStream(new FileInputStream(source)), fileName);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#addfile(java.io.File)
     */
    @Override
    public void addfile(File source) throws IOException, NoSuchAlgorithmException {
        getProcessContext().addFile(new BufferedInputStream(new FileInputStream(source)), source.getName());
    }
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#addfile(java.io.InputStream, java.lang.String)
     */
    @Override
    public void addfile(InputStream stream, String fileName) throws IOException, NoSuchAlgorithmException {
        getProcessContext().addFile(stream, fileName);
    }
    
    @CodeCompletion
    public String getprocessid() {
        return _formInfo.getProcessId();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#getprocesscontext()
     */
    @Override
    public TMLFormProcessContext getprocesscontext() {
        return getProcessContext();
    }
    
    public boolean isempty(String fieldName) {
        
        // New behaviour, also regarding empty/non-submitted relations fields as "empty relation" 
        if (_formInfo.getVersionCompliance().isAtLeast(6,2)) {
            Object singleFieldValue = field(fieldName);
            if (WGUtils.isEmpty(singleFieldValue)) {
                return true;
            }
            
            // Special "emptiness" of single relation fields: Containing the relation nullplace holder 
            FieldReg reg = getforminfo().getFieldRegistration(fieldName);
            if (reg != null && reg.getRelationtype() != null && !reg.isMultiple()) {
                return RELATION_NULLPLACE_HOLDER.equals(singleFieldValue);    
            }
            
            return false;
        }
        
        // Old behaviour which regards relation fields only as empty if they contain the nullplace holder
        else {
            FieldReg reg = getforminfo().getFieldRegistration(fieldName);
            if (reg != null && reg.getRelationtype() != null) {
                return RELATION_NULLPLACE_HOLDER.equals(field(fieldName));    
            }
            else {
                return WGUtils.isEmpty(field(fieldName));
            }
        }
        
        
    }
    
    public void clearFiles() {
        getprocesscontext().clearFiles();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#isfilesdropped()
     */
    @Override
    public boolean isfilesdropped() {
        return _filesDropped;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#fileurl(java.lang.String)
     */
    @Override
    public String fileurl(String fileName) throws URIException {
        
        PCFile file = getProcessContext().getFiles().get(fileName.toLowerCase());
        if (file == null) {
            return null;
        }
        
        StringBuffer url = new StringBuffer();
        url.append(TMLContext.getThreadMainContext().getEnvironment().getPublisherURL());
        url.append("/");
        url.append(WGPRequestPath.PATHCMD_TMLFORM);
        url.append("/");
        url.append(getProcessContext().getProcessId());
        url.append("/");
        url.append(getFormContext().getwgacore().getURLEncoder().encodePathPart(fileName));
        
        return url.toString();
        
    }

    public void setcreateddoc(WGDocument createdDoc) {
        getprocesscontext().setCreatedDoc(createdDoc);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.form.Form#store()
     */
    @Override
    public boolean store() throws WGException {
        
        if (!this.validate()) {
            return false;
        }
        
        FormSource fs = fetchFormSource(TMLContext.getThreadMainContext(), false);
        return fs.storeForm(this, true);

    }

    protected String createPasswordHash(String pwd) throws WGException {
        try {
            HashingService hashingService = WGA.get(getFormContext()).service(HashingService.class);
            HashedPassword hashedPwd = HashedPassword.create(pwd, hashingService, _passwordSalt);
            return hashedPwd.toString();
        }
        catch (HashingException e) {
            throw new WGAServerException("Exception creating password hash", e);
        }
            
    }
    
    private Object createPasswordSalt() throws WGException {
        try {
            HashingService scheme = WGA.get(getFormContext()).service(HashingService.class);
            return scheme.generateSalt();
        }
        catch (HashingException e) {
            throw new WGAServerException("Exception generating hashing salt", e);
        }
    }

    private String extractFileFromDataURL(String dataURL) throws NoSuchAlgorithmException, IOException {

        int commaPos = dataURL.indexOf(",");
        if (commaPos == -1) {
            return null;
        }
        
        List<String> metadata = WGUtils.deserializeCollection(dataURL.substring(0,commaPos), ";");
        
        // Parse the metadata
        String mimeType = "text/plain";
        String charSet = "US-ASCII";
        boolean isBase64 = false;
        
        int idx = 0;
        for (String mdField : metadata) {
            if (mdField.startsWith("charset=")) {
                charSet = mdField.substring(8);
            }
            else if (mdField.equals("base64")) {
                isBase64 = true;
            }
            else if (idx == 0) {
                mimeType = mdField;
            }
            idx++;
        }
        
        // Parse the data
        String data = dataURL.substring(commaPos+1);
        
        byte[] dataBytes;
        if (isBase64) {
            dataBytes = Base64.decode(data);
        }
        else {
            dataBytes = URIUtil.decode(data, charSet).getBytes(getFormContext().getwgacore().getCharacterEncoding());
        }
        
        String suffix = WGFactory.getMimetypeDeterminationService().determineSuffixByMimeType(mimeType);
        if (suffix != null) {
            suffix = "." + suffix;
        }
        else {
            suffix = "";
        }
        String fileName = "dataurl_" + WGUtils.toHexString(MD5HashingInputStream.getStreamHashBytes(new ByteArrayInputStream(dataBytes))) + suffix; 
        ByteArrayInputStream stream = new ByteArrayInputStream(dataBytes);
        addfile(stream, fileName);
        
        return fileName;
        
        
    }
    
    private String getExtractedFileHTML(String fileName, boolean image) {
        
        StringBuilder out = new StringBuilder();
        if (image) {
            out.append("src=\"{%!imgurl:");
        }
        else {
            out.append("href=\"{%!fileurl:");
        }
        
        out.append(fileName).append("%}\" {%!rtfsystem:wga:urlinfo=\"intfile|").append(fileName).append("\"%} ");
        return out.toString();
    }
    
    private ModuleDefinition fetchFormSourceModuleDefinition() throws WGException {
        
        try {
            ModuleRegistry reg = TMLContext.getThreadMainContext().getwgacore().getModuleRegistry();
            ModuleDefinition md = reg.getModuleDefinitionByKey(TMLFormSourceModuleType.class, getforminfo().getSource());
            if (md != null) {
                md.testDependencies();
                return md;
            }
            else {
                throw new WGAServerException("Unknown WebTML form source type: " + getforminfo().getSource());
            }
        }
        catch (ModuleDependencyException e) {
            throw new WGAServerException("Cannot use form source '" + getforminfo().getSource() + " because of missing dependency", e);
        }
        
    }
    
    public FormSource fetchFormSource(TMLContext cx) throws WGException {
        return fetchFormSource(cx, true);
    }
    
    private FormSource fetchFormSource(TMLContext cx, boolean usedToFetchFieldValue) throws WGException {
        try {
            TMLContext targetContext;
            if ((!usedToFetchFieldValue || _formInfo.getVersionCompliance().isAtLeast(6, 2)) && _formInfo.getTargetContextPath() != null && !cx.getpath().equals(_formInfo.getTargetContextPath())) {
                targetContext = cx.context(_formInfo.getTargetContextPath(), false);
                if (targetContext == null) {
                    throw new WGAServerException("Unable to retrieve target context of form: " + _formInfo.getTargetContextPath());
                }
            }
            else {
                targetContext = cx;
            }
            
            FormSource fs = (FormSource) cx.getwgacore().getModuleRegistry().instantiate(_formSourceModuleDefinition);
            fs.init(targetContext);
            return fs;
        }
        catch (ModuleInstantiationException e) {
            throw new WGAServerException("Exception instantiating form source '" + getforminfo().getSource(), e);
        }
    }
    

    public Object getpasswordsalt() {
        return _passwordSalt;
    }

    private TMLFormProcessContext getProcessContext() {
        
        if (_processContext == null) {
            _processContext = retrieveProcessContext();
        }
        
        return _processContext;
    }

    private void setProcessContext(TMLFormProcessContext processContext) {
        _processContext = processContext;
    }


    
    

}
