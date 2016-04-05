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

package de.innovationgate.wgpublisher.cluster;

import java.io.Serializable;
import java.util.Date;

public class MemberInformation implements Serializable {

    private static final long serialVersionUID = 2L;
    
    private String _name;
    private Date _activeSince;
    private int _highSeverityProblemCount;
    private int _lowSeverityProblemCount;
    private String _encryptionKeyHash;

    public String getName() {
        return _name;
    }

    public void setName(String name) {
        _name = name;
    }

    public Date getActiveSince() {
        return _activeSince;
    }

    public void setActiveSince(Date activeSince) {
        _activeSince = activeSince;
    }

    public int getHighSeverityProblemCount() {
        return _highSeverityProblemCount;
    }

    public void setHighSeverityProblemCount(int highSeverityProblemCount) {
        _highSeverityProblemCount = highSeverityProblemCount;
    }

    public int getLowSeverityProblemCount() {
        return _lowSeverityProblemCount;
    }

    public void setLowSeverityProblemCount(int lowSeverityProblemCount) {
        _lowSeverityProblemCount = lowSeverityProblemCount;
    }

    public String getEncryptionKeyHash() {
        return _encryptionKeyHash;
    }

    public void setEncryptionKeyHash(String encryptionKeyHash) {
        _encryptionKeyHash = encryptionKeyHash;
    }


}
