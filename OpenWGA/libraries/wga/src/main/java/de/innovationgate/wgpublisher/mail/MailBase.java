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

package de.innovationgate.wgpublisher.mail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.activation.URLDataSource;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;
import javax.mail.internet.MimeUtility;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.mail.WGMail;
import de.innovationgate.webgate.api.mail.WGMailException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public abstract class MailBase implements WGMail {

    public static final String PROP_PROTOCOL = "workflow.mail.transport.protocol";

    @CodeCompletion
    public Message prepareMessage() throws WGMailException, MessagingException, AddressException {
        try {
            Session session = createMailSession();
            if (session == null) {
                throw new WGMailException("MAIL ERROR: Can't open session.");
            }

            // Set headers of the mail
            MimeMessage message = new MimeMessage(session);
            message.setFrom(_from);

            // message.setReplyTo( Address[] );

            /*
             * Could use alias names: Address address = new
             * InternetAddress("president@whitehouse.gov", "Barack Obama");
             */

            if (_to != null && !_to.isEmpty()) {
                message.addRecipients(Message.RecipientType.TO, _to.toArray(new InternetAddress[_to.size()]));
            }
            else {
                throw new WGMailException("MAIL ERROR: No address to send mail to.");

            }

            if (_cc != null && !_cc.isEmpty()) {
                message.addRecipients(Message.RecipientType.CC, _cc.toArray(new InternetAddress[_cc.size()]));
            }

            if (_bcc != null && !_bcc.isEmpty()) {
                message.addRecipients(Message.RecipientType.BCC, _bcc.toArray(new InternetAddress[_bcc.size()]));
            }

            if (_replyTo != null) {
                message.setReplyTo(new InternetAddress[] {_replyTo});
            }

            message.setSubject(prepareText(_subject));
            message.setSentDate(new Date());

            // Set contents of the mail
            if ((_tempfiles == null || _tempfiles.size() == 0) && (_attachmentURLs == null || _attachmentURLs.size() == 0)) {
                putContentToMessage(message);
            }
            else {
                prepareMultipartMessage(message);
            }
            
            return message;
        }
        catch (UnsupportedEncodingException e) {
            throw new MessagingException("Exception encoding message", e);
        }
    }

    protected abstract Session createMailSession();

    public abstract void send(Message message) throws MessagingException;

    public static final String PROP_PASSWORD = "workflow.mail.password";
    public static final String PROP_USER = "workflow.mail.user";
    public static final String PROP_ROOTURL = "workflow.mail.rooturl";
    protected List<InternetAddress> _to = null;
    protected List<InternetAddress> _cc = null;
    protected List<InternetAddress> _bcc = null;
    protected InternetAddress _from = null;
    protected String _subject = "";
    private String _body = "";
    private String _mimeType = "text/html";
    protected InternetAddress _replyTo = null;
    private boolean _encodeText = false;
    protected Map _attachmentURLs = null;
    protected List _tempfiles = null;
    protected WGAMailConfiguration _mailConfig;

    private static InternetAddress[] buildInternetAddressArray(List addresses) throws AddressException {
        
        List addressObjects = new ArrayList();
        for (int idx = 0; idx < addresses.size(); idx++) {
            String mailAddress = (String) addresses.get(idx);
            if (mailAddress != null && !mailAddress.trim().equals("")) {
            	String addr = getAddressPart(mailAddress);
            	String personalPart = getPersonalPart(mailAddress);
            	
            	if (personalPart != null) {
            		try {
    					addressObjects.add(new InternetAddress(addr, personalPart, "UTF-8"));
    				} catch (UnsupportedEncodingException e) {
    					// fallback
    					addressObjects.add(new InternetAddress(addr));
    				}	
            	} else {
            		addressObjects.add(new InternetAddress(addr));
            	}
            }
        }
        
        InternetAddress[] addressArray = new InternetAddress[addressObjects.size()];
        addressArray = (InternetAddress[]) addressObjects.toArray(addressArray);
        return addressArray;
        
    }

    protected void init(WGAMailConfiguration mailConfig) throws UnsupportedEncodingException {
    	_mailConfig = mailConfig;
    	setFromAddress(new InternetAddress(mailConfig.getFromAddress(), mailConfig.getFromName()));
    }

    @CodeCompletion
    public void send() throws WGMailException {
        try {
            // Prepare message
            Message message = prepareMessage();
    
            // Send mail
            send(message);
    
        }
        catch (MessagingException e) {
            throw new WGMailException("Exception sending SMTP mail", e);
        }
    }

    protected void putContentToMessage(MimePart message) throws MessagingException {
        
        if (_encodeText) {
            
            if (_mimeType.startsWith("text/plain")) {
                message.setText(_body, "UTF-8");
            }
            else if (_mimeType.startsWith("text/html")) {
                message.setContent(encodeHTML(_body), _mimeType);
            }
            
            // Impossible to encode text for different mimetype bc. we do not know what artefacts to use for unsupported characters
            else {
                message.setContent(_body, _mimeType);
            }
            
        }
        else {
            message.setContent(_body, _mimeType);
        }
    }

    protected void prepareMultipartMessage(Message message) throws MessagingException, WGMailException, UnsupportedEncodingException {
        Multipart multipart = new MimeMultipart();
    
        // Add body part
        MimeBodyPart messageBodyPart = new MimeBodyPart();
        putContentToMessage(messageBodyPart);
        multipart.addBodyPart(messageBodyPart);
    
        // Add file attachment parts
        if (this._attachmentURLs != null) {
            Iterator urls = _attachmentURLs.keySet().iterator();
            while (urls.hasNext()) {
                String sFileURL = (String) urls.next();
                URL fileURL;
                try {
                    fileURL = new URL(sFileURL);
                }
                catch (MalformedURLException e1) {
                    throw new WGMailException("Exception attaching file from URL " + sFileURL, e1);
                }
    
                // check if we have an alternate filename for this url
                String altFilename = (String) _attachmentURLs.get(sFileURL);
                String filename = null;
                if (altFilename != null) {
                    filename = altFilename;
                }
                else {
                    // build filename from url
                    String[] aFilename = sFileURL.split("/");
                    if (aFilename.length > 0) {
                        filename = aFilename[aFilename.length - 1];
                    }
                    else {
                        filename = sFileURL;
                    }
                }
    
                messageBodyPart = new MimeBodyPart();
    
                DataSource source = new URLDataSource(fileURL);
                messageBodyPart.setDataHandler(new DataHandler(source));
                messageBodyPart.setFileName(filename);
                multipart.addBodyPart(messageBodyPart);
    
            }
        }
        
        if (_tempfiles != null) {
            Iterator tempFiles = _tempfiles.iterator();
            while (tempFiles.hasNext()) {
                TemporaryFile temp = (TemporaryFile) tempFiles.next();
                messageBodyPart = new MimeBodyPart();
                DataSource source = new FileDataSource(temp.getFile());
                messageBodyPart.setDataHandler(new DataHandler(source));
                messageBodyPart.setFileName(temp.getFile().getName());
                multipart.addBodyPart(messageBodyPart);
    
            }
        }        
        message.setContent(multipart);
    }

    protected String prepareText(String text) throws UnsupportedEncodingException {
        if (_encodeText) {
            return MimeUtility.encodeText(text, "UTF-8", "Q");
        }
        else {
            return text;
        }
    }

    protected void finalize() throws Throwable {
    	// delete tempfiles
        try {
            if (_tempfiles != null) {
            	Iterator tempfiles = _tempfiles.iterator();
            	while (tempfiles.hasNext()) {
            		TemporaryFile temp = (TemporaryFile) tempfiles.next();
            		temp.delete();
            	}
            }
        } finally {
            super.finalize();
        }
    }

    /**
     * parses the personal part of an email address like "Franz Demo <franz@demo.de>"
     * @param mailAddress
     * @return
     */
    private static String getPersonalPart(String mailAddress) {
    	if (mailAddress != null) {
    		mailAddress = mailAddress.trim();
    		if (mailAddress.indexOf("<") != -1 && mailAddress.indexOf(">") != -1) {
    			String personalPart = null;
    			if (mailAddress.startsWith("<") && mailAddress.indexOf(">") != mailAddress.length()-1) {        			
        			personalPart = mailAddress.substring(mailAddress.indexOf(">")+1, mailAddress.length()).trim();
        		} else {
        			personalPart = mailAddress.substring(0, mailAddress.indexOf("<")).trim();
        		}	
    			if (personalPart.equals("")) {
    				return null;
    			} else {
    				return personalPart;
    			}
    		} else {
    			return null;
    		}
    	} else {
    		return null;
    	}
    }

    @CodeCompletion
    public String getSubject() {
        return _subject;
    }

    @CodeCompletion
    public void setSubject(String subject) {
        this._subject = subject;
    }

    @CodeCompletion
    public String getBody() {
        return _body;
    }

    @CodeCompletion
    public void setBody(String body) {
        this._body = body;
    }

    @CodeCompletion
    public List<String> getCc() {
        List<String> toAddr = new ArrayList();
        for (InternetAddress addr : _cc) {
            toAddr.add(addr.getAddress());
        }
        return toAddr;
    }

    @CodeCompletion
    public void setCc(List<String> addresses) throws AddressException {
        ArrayList newAddresses = new ArrayList<InternetAddress>();
        for (String address : addresses) {
            newAddresses.add(new InternetAddress(address));
        }
        this._cc = newAddresses;
    }

    @CodeCompletion
    public void setCc(String address) throws AddressException {
        _bcc = new ArrayList<InternetAddress>();
        _bcc.add(new InternetAddress(address));
    
    }

    @CodeCompletion
    public String getFrom() {
        return (_from != null ? _from.getAddress() : null);
    }

    @CodeCompletion
    public void setFrom(String from) throws UnsupportedEncodingException, AddressException {
       _from = new InternetAddress(from);
    }

    @CodeCompletion
    public String getFromName() {
        return (_from != null ? _from.getPersonal() : null);
    }

    /**
     * sets the given from address and from name
     * @param address e.g. john@example.com	
     * @param name e.g. John Mueller
     * @throws UnsupportedEncodingException 
     */
    @CodeCompletion
    public void setFrom(String address, String name) throws UnsupportedEncodingException {
    	_from = new InternetAddress(address, name);
    }

    @CodeCompletion
    public List getTo() {
       List<String> toAddr = new ArrayList();
       for (InternetAddress addr : _to) {
           toAddr.add(addr.getAddress());
       }
       return toAddr;
    }

    @CodeCompletion
    public void setTo(List<String> addresses) throws AddressException {
        ArrayList newAddresses = new ArrayList<InternetAddress>();
        for (String address : addresses) {
            newAddresses.add(new InternetAddress(address));
        }
        this._to = newAddresses;
    }

    @CodeCompletion
    public void setTo(String address) throws AddressException {
        _to = new ArrayList<InternetAddress>();
        _to.add(new InternetAddress(address));
    }

    @CodeCompletion
    public List<String> getBcc() {
        List<String> addresses = new ArrayList();
        for (InternetAddress addr : _bcc) {
            addresses.add(addr.getAddress());
        }
        return addresses;
    }

    @CodeCompletion
    public void setBcc(List<String> addresses) throws AddressException {
        ArrayList newAddresses = new ArrayList<InternetAddress>();
        for (String address : addresses) {
            newAddresses.add(new InternetAddress(address));
        }
        this._bcc = newAddresses;
    }

    @CodeCompletion
    public void setBcc(String address) throws AddressException {
        _bcc = new ArrayList<InternetAddress>();
        _bcc.add(new InternetAddress(address));
    }

    /**
     * @return Returns the mimeType.
     */
    @CodeCompletion
    public String getMimeType() {
        return _mimeType;
    }

    /**
     * @param mimeType
     *            The mimeType to set.
     */
    @CodeCompletion
    public void setMimeType(String mimeType) {
        this._mimeType = mimeType;
    }

    public List getAttachmentURLs() {
        return (List) _attachmentURLs.keySet();
    }

    public Map getAttachmentURLsMap() {
        return this._attachmentURLs;
    }

    public void setAttachmentURLs(List attachmentURLs) {
        if (attachmentURLs != null) {
            this._attachmentURLs = new HashMap();
            Iterator it = attachmentURLs.iterator();
            while (it.hasNext()) {
                String url = (String) it.next();
                addAttachmentURL(url);
            }   
        } else {
            this._attachmentURLs = null;
        }
    }

    public void setAttachmentURLs(Map attachmentURLs) {
        this._attachmentURLs = attachmentURLs;
    }

    @CodeCompletion
    public void addAttachmentURL(String url) {
        addAttachmentURL(url, null);
    }

    @CodeCompletion
    public void addAttachmentURL(String url, String altFilename) {
        if (this._attachmentURLs == null) {
            this._attachmentURLs = new HashMap();
        }
        this._attachmentURLs.put(url, altFilename);
    }

    @CodeCompletion
    public void addAttachment(InputStream in, String filename) throws WGMailException {
        
        try {
            TemporaryFile temp = new TemporaryFile(filename, in, WGFactory.getTempDir());
                if (_tempfiles == null) {
                    _tempfiles = new ArrayList();
                }
                _tempfiles.add(temp);
        }
        catch (IOException e) {
            throw new WGMailException("Unable to create tempfile for attachment '" + filename + "'.", e);        
        }
        
    }

    /**
     * adds the given inputstream as attachment to the mail
     * @param in InputStream
     * @param filename filename to use for the attachment
     * @throws TMLException 
     * @throws FileNotFoundException 
     */
    @CodeCompletion
    public void addAttachment(File file) throws WGMailException, FileNotFoundException {
        addAttachment(new FileInputStream(file), file.getName());
    }

    @CodeCompletion
    public void addAttachment(WGDocument document, String filename) throws WGAPIException, TMLException {
    	addAttachment(document, filename, null);
    }

    /**
     * adds the given file from the given container or content to the mail
     * @param document WGDocument
     * @param filename 
     * @param altFilename
     * @throws WGAPIException
     * @throws TMLException
     */
    @CodeCompletion
    public void addAttachment(WGDocument document, String filename, String altFilename) throws WGAPIException, TMLException {    
    	InputStream filedata = document.getFileData(filename);
    	if (filedata == null) {
    		throw new TMLException("No such file or attachment '" + filename + "' in WGDocument key='" + document.getDocumentKey() + "'.");
    	} else {
    		if (altFilename != null) {
    			addAttachment(filedata, altFilename);
    		} else {
    			addAttachment(filedata, filename);
    		}
    	}
    }

    @CodeCompletion
    public String getReplyTo() {
        return (_replyTo != null ? _replyTo.getAddress() : null);
    }

    @CodeCompletion
    public void setReplyTo(String address) throws UnsupportedEncodingException, AddressException {
        _replyTo = new InternetAddress(address);
    }

    /**
     * parses the address part of an email address like "Franz Demo <franz@demo.de>"
     * @param mailAddress
     * @return
     */
    private static String getAddressPart(String mailAddress) {
    	if (mailAddress != null) {
    		if (mailAddress.indexOf("<") != -1 && mailAddress.indexOf(">") != -1) {
    			return mailAddress.substring(mailAddress.indexOf("<") + 1, mailAddress.indexOf(">")).trim();
    		} else {
    			return mailAddress.trim();
    		}
    	} else {
    		return null;
    	}
    }

    public MailBase() {
        super();
    }

    @CodeCompletion
    public boolean isEncodeText() {
        return _encodeText;
    }

    @CodeCompletion
    public void setEncodeText(boolean encodeText) {
        this._encodeText = encodeText;
    }

    /**
     * A special version of encodeHTML which merely converts all Non-ASCII-Characters to HTML entities
     * @param html The html code
     * @return The encoded html code
     */
    private String encodeHTML(String html) {
        
        StringReader in = new StringReader(html);
        StringBuffer out = new StringBuffer();
        int ch;
    
        try {
            while ((ch = in.read()) != -1) {
                if (ch < 127) {
                    out.append((char) ch);
                }
                else {
                    out.append('&').append('#').append(ch).append(';');
                }
            }
        }
        catch (IOException exc) {
            exc.printStackTrace();
        }
    
        return out.toString();
        
    }

    public List<InternetAddress> getBccAddresses() {
        return _bcc;
    }

    public List getCcAddresses() {
        return _cc;
    }

    public InternetAddress getFromAddress() {
        return _from;
    }

    public InternetAddress getReplyToAddress() {
        return _replyTo;
    }

    public List<InternetAddress> getToAddresses() {
        return _to;
    }

    public void setBccAddresses(List<InternetAddress> addresses) {
        _bcc = addresses;
    }

    public void setCcAddresses(List<InternetAddress> addresses) {
        _cc = addresses;
    }

    public void setFromAddress(InternetAddress from) {
        _from = from;
    }

    public void setReplyToAddress(InternetAddress address) {
        _replyTo = address;
    }

    public void setToAddresses(List<InternetAddress> addresses) {
        _to = addresses;
    }

    public boolean getEncodeText() {
        return _encodeText;
    }

}