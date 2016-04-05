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

import java.net.InetAddress;

/**
 * represents an IPv4Address
 *
 */
public class IPv4Address implements IPAddress, Comparable<IPv4Address> {
    
    
  public static IPv4Address subnetToNetmask(int prefix) throws Exception {
        
        int mask = 0xffffffff << (32 - prefix);

        int value = mask;
        byte[] bytes = new byte[]{ 
                (byte)(value >>> 24), (byte)(value >> 16 & 0xff), (byte)(value >> 8 & 0xff), (byte)(value & 0xff) };

        InetAddress netAddr = InetAddress.getByAddress(bytes);
        return new IPv4Address(netAddr.getCanonicalHostName());
        
  }

  public int IParray[] = new int[4];   

/**
 * Builds an uninitialized IP address
 */
public IPv4Address() {
    IParray[0] = -1;
    IParray[1] = -1;
    IParray[2] = -1;
    IParray[3] = -1;
  }
  
  /**
   * Parses an ip adress from a string.
   * 
   * @param ip The incoming String IP in the form x.x.x.x where each
   * x makes up one octal of the IP address and is &gt;&eq;0 and 
   * &lt;&eq;255.
   * <p>
   * @throws Exception Throws an exception if the string isn't
   * formatted as expected
   */
  public IPv4Address(String ip) throws Exception {
    setIP(ip);
  }

  /**
   * Set the IP from an incoming String. 
   * @param ip The incoming IP string in the form x.x.x.x
   * @throws NumberFormatException if there are illegal characters in
   * the incoming string
   * @throws StringIndexOutOfBoundsException if the IP is formatted
   * incorrectly
   * @throws Exception if all else fails
   */
  public void setIP(String ip) throws NumberFormatException, 
    StringIndexOutOfBoundsException, Exception { 
     
    int tempIP[] = new int[4];
    int dotIndex = 0, nextDotIndex;
    
    // performance tweaked IPv4 String parser that's faster than any 
    // I've found. Basically it will take index of the dots, and set 
    // each segment of the array representing the IP addr
    for (int x=0; x < 4; x++) {
      nextDotIndex = (x<3) ? ip.indexOf('.', dotIndex) :ip.length();
      tempIP[x] = Integer.parseInt(ip.substring(dotIndex,nextDotIndex));
      dotIndex = nextDotIndex + 1;
    }
    
    // If successfully parsed set it
    IParray = tempIP; 
  }

  /**
   * Set the whole IP array. 
   * @param IParray the int array to set
   */
  public void setIP(int[] IParray) {
    this.IParray = IParray;
  }
  
  /**
   * set the IP array value on the given index 0-3
   * @param index 0-3
   * @param value 0-255
   */
  public void setIP(int index, int value) {
      this.IParray[index] = value;
  }

  /**
   * Returns an array representing the IP object
   */
  public int[] getIP() {
    return IParray;
  }
  
  public String toString() {
    return IParray[0]+"."+IParray[1]+"."+IParray[2]+"."+IParray[3];
  }
        
  /**
   * Increment the IP address by a number. 
   * e.g. 172.20.1.5 + 2 = 172.20.1.7
   * @param number The integer number to add to the IP
   * @throws Exception if the IP has exceeded the bounds 0.0.0.0 to
   * 255.255.255.255
   */
  public void add(int number) throws Exception { 
    IParray[3] += number;
    fixIP();
  }

  /**
   * Internal function to fix an invalid IP. This is called by the add
   * method to make an incremented IP valid.
   *
   * @throws Exception if the IP has grown above 255.255.255.255 or has
   * gone below 0.0.0.0
   */
  private void fixIP() throws Exception {
    boolean done=false;

    // For each segment of the IP working from the right, fix it by
    // subtracting or adding to the next set over. Then check if
    // the left most octal is still valid ie 0-255 and leave.
    while(!done)  {
      done = true;
      if (IParray[3] > 255) {
        IParray[2]++;
        IParray[3] -= 256; // test to make sure it's not 255
        done = false;
      } else if (IParray[3] < 0) {
        IParray[2]--;
        IParray[3] += 256;
        done = false;
      }
      
      if (IParray[2] > 255) {
        IParray[1]++;
        IParray[2] -= 256;
        done = false;
      } else if (IParray[2] < 0) {
        IParray[1]--;
        IParray[2] += 256;
        done = false;
      }

      if (IParray[1] > 255) {
        IParray[0]++;
        IParray[1] -= 256;
        done = false;
      } else if (IParray[1] < 0) {
        IParray[0]--;
        IParray[1] += 256;
        done = false;
      } 
      
      if (IParray[0] > 255 || IParray[0] < 0) {
        throw new Exception("The IP is not valid!");
      }
    }
  }

  /**
   * Validate the current IP. Each octal needs to be within 0-255 giving
   * an IP between 0.0.0.0 and 255.255.255.255.
   *
   * @return True if it is valid, else False if it is not.
   */
  public boolean validate() {
    if (    (IParray[0] < 0 || IParray[0] > 255) ||
            (IParray[1] < 0 || IParray[1] > 255) ||
            (IParray[2] < 0 || IParray[2] > 255) ||
            (IParray[3] < 0 || IParray[3] > 255))
      return false;
    else return true;
  }

  /**
   * A defacto compare to operation. Given a second IP address it
   * will compare each octal and give a int either -1, 0 or 1 for less
   * than, equal to or greater than. 
   *
   * @param second The second IP to compare.
   * @return The value representing the compare result in the form -1,
   * 0 and 1 for less than, equal or greater than. 
   */
  public int compareTo(IPv4Address second) {
    // compares IPs and see's if they are equal
    if (this.IParray[0] > second.IParray[0])        return -1;
    else if (this.IParray[0] < second.IParray[0])   return  1;
    if (this.IParray[1] > second.IParray[1])        return -1;
    else if (this.IParray[1] < second.IParray[1])   return  1;
    if (this.IParray[2] > second.IParray[2])        return -1;
    else if (this.IParray[2] < second.IParray[2])   return  1;
    if (this.IParray[3] > second.IParray[3])        return -1;
    else if (this.IParray[3] < second.IParray[3])   return  1;
    else return 0; // equal
  }

  /**
   * A defacto equals operation. As per java spec equals(null) will
   * return false.
   *
   * @param secondIP IP object to test
   * @return True if it is equal to the other, else False
   */
  public boolean equals(Object secondIP) {
    IPv4Address second = (IPv4Address)secondIP;
    
    int result = -9; 
    try {
      result = compareTo(second);
    } catch (Exception e) {
      return false;
    }
    
    if (result == 0) return true;
    else return false;
  }
}
