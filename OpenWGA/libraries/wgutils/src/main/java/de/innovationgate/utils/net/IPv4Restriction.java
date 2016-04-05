/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils.net;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.ClientRestriction;


/**
 * this bean defines an ip restriction
 * used to restrict client access to contentstores
 *
 */

public class IPv4Restriction implements IPRestriction {
    
    public static IPRestriction parseRestrictionString(String str) throws FormattingException {
        
        try {
            // Range
            if (str.contains("-")) {
                String[] parts = str.split("-");
                IPv4Restriction restriction = new IPv4Restriction(ClientRestriction.TYPE_RANGE);
                restriction.setStartIP(new IPv4Address(parts[0]));
                restriction.setEndIP(new IPv4Address(parts[1]));
                return restriction;
            }
            
            // Network
            else if (str.contains("/")) {
                String[] parts = str.split("/");
                IPv4Restriction restriction = new IPv4Restriction(ClientRestriction.TYPE_NETWORK);
                restriction.setNetwork(new IPv4Address(parts[0]));
                if (parts[1].contains(".")) {
                    restriction.setNetmask(new IPv4Address(parts[1]));
                }
                else {
                    restriction.setType(ClientRestriction.TYPE_NETWORK_CIDR_PREFIX);
                    restriction.setNetmask(IPv4Address.subnetToNetmask(Integer.parseInt(parts[1])));
                }
                return restriction;
            }
            
            // Host
            else {
                IPv4Restriction restriction = new IPv4Restriction(ClientRestriction.TYPE_HOST);
                restriction.setHostIP(new IPv4Address(str));
                return restriction;
            }
        }
        catch (Exception e) {
            throw new FormattingException("Exception parsing restriction string", e);
        }
        
    }

    private String _type;

    //fields for type host
    private IPv4Address _hostIP = null;

    //fields for type network
    private IPv4Address _network = null;

    private IPv4Address _netmask = null;

    //fields for type range
    private IPv4Address _startIP = null;

    private IPv4Address _endIP = null;

    /**
     * Creates a restriction of the given type
     * @param type The restriction type. Use constants TYPE_...
     */
    public IPv4Restriction(String type) {
        _type = type;
    }
    
    public IPv4Restriction(ClientRestriction restriction) throws Exception {
    	setType(restriction.getType());
    	if (restriction.getHostIP() != null) {
			setHostIP(new IPv4Address(restriction.getHostIP()));
		}
		if (restriction.getNetwork() != null) {
		    
            if (getType().equals(ClientRestriction.TYPE_NETWORK_CIDR_PREFIX)) {
	            List<String> parts = WGUtils.deserializeCollection(restriction.getNetwork(), "/");
	            if (parts.size() != 2) {
	                throw new IllegalArgumentException("IP Network must be divided by a single slash but isn't: " + restriction.getNetwork());
	            }
	            setNetwork(new IPv4Address(parts.get(0)));
	            setNetmask(IPv4Address.subnetToNetmask(Integer.parseInt(parts.get(1))));
            }
            else {
                setNetwork(new IPv4Address(restriction.getNetwork()));
            }
		}
		if (restriction.getNetmask() != null) {
			setNetmask(new IPv4Address(restriction.getNetmask()));
		}
		if (restriction.getStartIP() != null) {
			setStartIP(new IPv4Address(restriction.getStartIP()));			
		}
		if (restriction.getEndIP() != null) {
			setEndIP(new IPv4Address(restriction.getEndIP()));
		}
    }

    public String toString() {
        if (_type.equals(ClientRestriction.TYPE_HOST) && _hostIP != null) {
            return _hostIP.toString();
        }
        else if ((_type.equals(ClientRestriction.TYPE_NETWORK) || _type.equals(ClientRestriction.TYPE_NETWORK_CIDR_PREFIX)) && _network != null && _netmask != null) {
            return _network.toString() + "/" + _netmask.toString();
        }
        else if (_type.equals(ClientRestriction.TYPE_RANGE) && _startIP != null && _endIP != null) {
            return _startIP.toString() + "-" + _endIP.toString();
        }
        else {
            return "unkown type or missing data";
        }
    }

    /**
     * Returns the end ip of a restriction of {@link #TYPE_RANGE}
     */
    public IPv4Address getEndIP() {
        return _endIP;
    }

    /**
     * Sets the end ip of a restriction of {@link #TYPE_RANGE}
     * @param endIP
     */
    public void setEndIP(IPv4Address endIP) {
        _endIP = endIP;
    }

    /**
     * Returns the host ip of a restriction of {@link #TYPE_HOST}
     */
    public IPv4Address getHostIP() {
        return _hostIP;
    }

    /**
     * Sets the host ip of a restriction of {@link #TYPE_HOST}
     * @param hostIP
     */
    public void setHostIP(IPv4Address hostIP) {
        _hostIP = hostIP;
    }

    /**
     * Returns the netmask of a restriction of {@link #TYPE_NETWORK}
     */
    public IPv4Address getNetmask() {
        return _netmask;
    }

    /**
     * Sets the netmask of a restriction of {@link #TYPE_NETWORK}
     * @param netmask
     */
    public void setNetmask(IPv4Address netmask) {
        _netmask = netmask;
    }

    /**
     * Returns the network address of a restriction of {@link #TYPE_NETWORK}
     */
    public IPv4Address getNetwork() {
        return _network;
    }

    /**
     * Sets the network address of a restriction of {@link #TYPE_NETWORK}
     * @param network
     */
    public void setNetwork(IPv4Address network) {
        _network = network;
    }

    /**
     * Returns the start ip of a restriction of type {@link #TYPE_RANGE}
     */
    public IPv4Address getStartIP() {
        return _startIP;
    }

    /**
     * Sets the start ip of a restriction of type {@link #TYPE_RANGE}
     * @param startIP
     */
    public void setStartIP(IPv4Address startIP) {
        _startIP = startIP;
    }

    /**
     * Returns the restriction type as constant TYPE_...
     */
    public String getType() {
        return _type;
    }

    /**
     * Sets the restriction type. Use constants TYPE_...
     * @param type
     */
    public void setType(String type) {
        _type = type;
    }

    /**
     * Parses client restrictions from wga.xml
     * @param clientRestrictions The client restrictions parent element in wga.xm�l
     * @return List of parsed {@link IPv4Restriction} objects
     */
    public static List getRestrictions(Element clientRestrictions) {
        ArrayList list = new ArrayList();
        Element restrictions = clientRestrictions.element("restrictions");
        Iterator restrictionsIt = restrictions.elementIterator("restriction");
        while (restrictionsIt.hasNext()) {
            try {
                Element restrictionElement = (Element) restrictionsIt.next();
                IPRestriction restriction = getFromElement(restrictionElement);
                list.add(restriction);
            }
            catch (Exception e) {
                // restriction could not be parsed - skip
            }
        }
        return list;
    }
    
    public static List<IPv4Restriction> getRestrictions(List<ClientRestriction> restrictions, String dbkey, Logger log) {
    	ArrayList<IPv4Restriction> ipv4Restrictions = new ArrayList<IPv4Restriction>();
    	Iterator<ClientRestriction> it = restrictions.iterator();
    	while (it.hasNext()) {
    		ClientRestriction restriction = it.next();    		
    		try {
    			IPv4Restriction ipV4Restriction = new IPv4Restriction(restriction);
				ipv4Restrictions.add(ipV4Restriction);
			} catch (Exception e) {
				  // restriction could not be parsed - skip
				log.error("Client restriction on db '" + dbkey + "' of type '" + restriction.getType() + "' could not be parsed.", e);
			}
    	}
        return ipv4Restrictions;
    }
    

    /**
     * Parses a single restriction from wga.xml
     * @param restrictionElement A single restriction element in wga.xml
     * @return The parsed restriction
     * @throws Exception
     */
    private static IPRestriction getFromElement(Element restrictionElement) throws Exception {
        IPv4Restriction restriction = new IPv4Restriction(restrictionElement.attributeValue("type"));

        String sHostIP = restrictionElement.attributeValue("hostIP", null);
        if (sHostIP != null) {
            restriction.setHostIP(new IPv4Address(sHostIP));
        }

        String snetwork = restrictionElement.attributeValue("network", null);
        if (snetwork != null) {
            restriction.setNetwork(new IPv4Address(snetwork));
        }                
        String snetmask = restrictionElement.attributeValue("netmask", null);
        if (snetmask != null) {
            restriction.setNetmask(new IPv4Address(snetmask));
        }
        
        String sStartIP = restrictionElement.attributeValue("startIP", null);
        if (sStartIP != null) {
            restriction.setStartIP(new IPv4Address(sStartIP));
        }                
        String sEndIP = restrictionElement.attributeValue("endIP", null);
        if (sEndIP != null) {
            restriction.setEndIP(new IPv4Address(sEndIP));
        }
        return restriction;
    }

    /**
     * Writes restrictions to wga.xml. Previously existing restrictions will be erased.
     * @param clientRestrictions The client restrictions parent element in wga.xml
     * @param restrictions The restrictions to write. List of {@link IPv4Restriction} objects.
     */
    public static void saveRestrictions(Element clientRestrictions, List restrictions) {
        Element restrictionsElement = clientRestrictions.element("restrictions");
        //remove old rules
        restrictionsElement.clearContent();
        Iterator restrictionsIt = restrictions.iterator();
        while (restrictionsIt.hasNext()) {
            IPv4Restriction restriction = (IPv4Restriction) restrictionsIt.next();
            restriction.addToElement(restrictionsElement);
        }        
    }

    /**
     * Adds the restrictions data to wga.xml creating a new element
     * @param restrictionsElement The client restrictions parent element in wga.xml
     */
    private void addToElement(Element restrictionsElement) {
        Element restriction = restrictionsElement.addElement("restriction");
        restriction.addAttribute("type", this._type);
        if (_hostIP != null) {
            restriction.addAttribute("hostIP", this._hostIP.toString());
        }
        
        if (_network != null) {
            restriction.addAttribute("network", this._network.toString());
        }
        if (_netmask != null) {
            restriction.addAttribute("netmask", this._netmask.toString());
        }
        
        if (_startIP != null) {
            restriction.addAttribute("startIP", this._startIP.toString());
        }
        if (_endIP != null) {
            restriction.addAttribute("endIP", this._endIP.toString());
        }       
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.utils.net.IPRestriction#exists(de.innovationgate.utils.net.IPv4Address)
     */
    @Override
    public boolean exists(IPAddress thatIp) {
        
        if (!(thatIp instanceof IPv4Address)) {
            return false;
        }
        
        IPv4Address ip = (IPv4Address) thatIp;
        
        if (_type.equals(ClientRestriction.TYPE_HOST)) {
            return ip.equals(_hostIP);
        } else if (_type.equals(ClientRestriction.TYPE_RANGE)) {
            IPv4Range range = new IPv4Range(_startIP, _endIP);
            return range.exists(ip);
        } else if (_type.equals(ClientRestriction.TYPE_NETWORK) || _type.equals(ClientRestriction.TYPE_NETWORK_CIDR_PREFIX)) {
            try {
                IPv4Range range = new IPv4Range(_network.toString(), _netmask.toString());
                return range.exists(ip);
            }
            catch (Exception e) {
                return false;
            }
        }
        return false;
    }
}
