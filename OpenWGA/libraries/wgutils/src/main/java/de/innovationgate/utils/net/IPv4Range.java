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

/**
 * Represents a range of IPv4 IPAddresses.
 * For example 1.1.1.1 to 255.255.255.255
 */
public class IPv4Range {

    private IPv4Address _start;

    private IPv4Address _end;

    /**
     * Creates an IP range with start and end IP
     * 
     * @param start
     *            start IP
     * @param end
     *            end IP
     */
    public IPv4Range(IPv4Address start, IPv4Address end) {
        _start = start;
        _end = end;
    }

    /**
     * creates an iprange from the given net and netmask
     * 
     * @param snet
     *            e.g. 10.1.1.0
     * @param snetmask
     *            e.g. 255.255.255.0
     * @throws Exception
     *             if strings are formated invalid
     */
    public IPv4Range(String snet, String snetmask) throws Exception {
        IPv4Address net = new IPv4Address(snet);
        if (!net.validate()) {
            throw new IllegalArgumentException("Invalid network '" + net.toString() + "'.");
        }
        IPv4Address netmask = new IPv4Address(snetmask);
        if (!netmask.validate()) {
            throw new IllegalArgumentException("Invalid netmask '" + netmask.toString() + "'.");
        }

        // calc network address and use for further operations because we cannot
        // be sure snet is one
        IPv4Address calcNet = IPv4Calculator.calcNetwork(net.getIP(), netmask.getIP());
        _start = IPv4Calculator.calcHostMin(calcNet, netmask);
        _end = IPv4Calculator.calcHostMax(calcNet, netmask);
    }

    public String toString() {
        return _start.toString() + "->" + _end.toString();
    }

    /**
     * Sets the start IP
     * @param start
     */
    public void setStart(IPv4Address start) {
        _start = start;
    }

    /**
     * Sets the end IP
     * @param end
     */
    public void setEnd(IPv4Address end) {
        _end = end;
    }

    /**
     * Sets start and end IP at once
     * @param start start IP
     * @param end end IP
     */
    public void set(IPv4Address start, IPv4Address end) {
        _start = start;
        _end = end;
    }

    /**
     * Returns the start IP
     */
    public IPv4Address getStart() {
        return _start;
    }

    /**
     * Returns the end IP
     */
    public IPv4Address getEnd() {
        return _end;
    }

    /**
     * Determines if an IP address is contained in this IP range
     * @param ip The IP adress to test
     * @return true if it is contained
     */
    public boolean exists(IPv4Address ip) {
        if ((_start.compareTo(ip) == 1) && (ip.compareTo(_end) == 1)) {
            return true;
        }
        else if (_start.compareTo(ip) == 0 || _end.compareTo(ip) == 0) {
            return true;
        }
        return false;
    }

    /**
     * Validates the IP range. Tests if the start address is lower or equal to the end address.
     */
    public boolean validate() {
        if (_start.compareTo(_end) >= 0) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Compares two ip ranges on equality.
     * @param range Other range
     * @return true if they are equal
     */
    public boolean equals(IPv4Range range) {
        if (_start.compareTo(range.getStart()) == 0 && _end.compareTo(range.getEnd()) == 0) {
            return true;
        }
        else {
            return false;
        }
    }

}
