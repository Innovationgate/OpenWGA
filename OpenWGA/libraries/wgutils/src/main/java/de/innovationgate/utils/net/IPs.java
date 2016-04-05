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

package de.innovationgate.utils.net;

import java.net.InetAddress;

import com.googlecode.ipv6.IPv6Address;

import de.innovationgate.wga.config.ClientRestriction;

/**
 * Tool object to parse IP expressions - address, range, network, client restrictions - into IP objects
 */
public class IPs {
    
    public static IPAddress parseIPAddress(String address) throws Exception {
        
        if (address.contains(":")) {
            return new IPv6AddressWrapper(IPv6Address.fromString(address));
        }
        else {
            return new IPv4Address(address);
        }
        
    }
    
    public static IPRestriction parseRestriction(ClientRestriction cr) throws Exception {
        
        if (cr.isIP6()) {
            // IP6: Differ by type
            switch (cr.getType()) {
                
                case ClientRestriction.TYPE_HOST: {
                    return new IPv6AddressRestriction(cr.getHostIP());
                }
                
                case ClientRestriction.TYPE_NETWORK_CIDR_PREFIX: {
                    return new IPv6NetworkRestriction(cr.getNetwork());
                }
                
                case ClientRestriction.TYPE_NETWORK: {
                    throw new IllegalArgumentException("Invalid network restriction, IPs must be v4: " + cr.toString());
                }
                
                case ClientRestriction.TYPE_RANGE: {
                    return new IPv6RangeRestriction(cr.getStartIP(), cr.getEndIP());
                }
                
            }
        }
        
        // IP4, all implemented in one legacy object
        return new IPv4Restriction(cr);
        
    }

}
