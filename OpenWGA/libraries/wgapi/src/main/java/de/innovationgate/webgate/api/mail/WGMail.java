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
package de.innovationgate.webgate.api.mail;

import java.io.InputStream;
import java.util.List;

import javax.mail.internet.InternetAddress;

/**
 * An interface representing a mail created by the {@link WGMailService}
 *
 */
public interface WGMail {

    
    /**
     * Sends the mail
     * @throws WGMailException
     */
    public abstract void send() throws WGMailException;

    /**
     * @return Returns the subject.
     */
    public abstract String getSubject();

    /**
     * @param subject
     *            The subject to set.
     */
    public abstract void setSubject(String subject);

    /**
     * @return Returns the body.
     */
    public abstract String getBody();

    /**
     * @param body
     *            The body to set.
     */
    public abstract void setBody(String body);

    /**
     * @return Returns the cc addresses.
     */
    public abstract List getCcAddresses();

    /**
     * @param cc
     *            The cc addresses to set.
     */
    public abstract void setCcAddresses(List<InternetAddress> cc);

    /**
     * @return Returns the from address.
     */
    public abstract InternetAddress getFromAddress();

    /**
     * @param from
     * The from adress to set.
     */
    public abstract void setFromAddress(InternetAddress from);

    /**
     * @return Returns the to addresses.
     */
    public abstract List<InternetAddress> getToAddresses();

    /**
     * @param to
     *            The to to set.
     */
    public abstract void setToAddresses(List<InternetAddress> to);

    /**
     * @return Returns the bcc addresses.
     */
    public abstract List<InternetAddress> getBccAddresses();

    /**
     * @param bcc
     *            The bcc addresses to set.
     */
    public abstract void setBccAddresses(List<InternetAddress> bcc);

    /**
     * adds the given inputstream as attachment to the mail
     * @param in InputStream
     * @param filename filename to use for the attachment
     * @throws TMLException 
     */
    public abstract void addAttachment(InputStream in, String filename) throws WGMailException;

    /**
     * @return Returns the replyTo address.
     */
    public abstract InternetAddress getReplyToAddress();

    /**
     * @param replyTo The replyTo address to set.
     */
    public abstract void setReplyToAddress(InternetAddress replyTo);
    
    /**
     * Sets if the mail content should be UTF-8 encoded. Prevent this is the mail content already is encoded.
     */
    public void setEncodeText(boolean encode);
    
    /**
     * Returns if the mail content is UTF-8 encoded on sending. Defaults to false.
     */
    public boolean getEncodeText();

}
