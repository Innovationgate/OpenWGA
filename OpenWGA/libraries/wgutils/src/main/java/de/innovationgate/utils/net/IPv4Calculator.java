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
 * utility class for IPv4Address operations
 *
 */

public class IPv4Calculator {
    
    /**
     * Calculates the network address from an ip address and a network mask
     * @param ip The ip address as int array
     * @param mask The netmask as int array
     * @return The network address
     */
    public static IPv4Address calcNetwork(int ip[], int mask[]) {
        IPv4Address net = new IPv4Address();
        
        for (int i = 0; i < ip.length; i++) {
            net.setIP(i, ip[i] & mask[i]);
        }
        return net;
        
    }

    /**
     * Calculates the lowest possible ip address from an network address and a netmask
     * @param net The network address as int array
     * @param netmask The netmask as int array
     * @return The lowest ip address in this network
     * @throws Exception
     */
    public static IPv4Address calcHostMin(IPv4Address net, IPv4Address netmask) throws Exception {
        return calcHostMin(net.getIP(), netmask.getIP());
    }    

    /**
     * Calculates the highest possible ip address from an network address and a netmask
     * @param net The network address
     * @param netmask The netmask
     * @return The highest ip address in this network
     * @throws Exception
     */
    public static IPv4Address calcHostMax(IPv4Address net, IPv4Address netmask) throws Exception {
        return calcHostMax(net.getIP(), netmask.getIP());
    }            
    
    /**
     * Calculates the lowest possible ip address from an network address and a netmask
     * @param net The network address
     * @param mask The netmask
     * @return The lowest ip address in this network
     * @throws Exception
     */
    public static IPv4Address calcHostMin(int net[], int mask[]) throws Exception {
        IPv4Address min = new IPv4Address();
        for (int i = 0; i < net.length; i++) {
            min.setIP(i, net[i]);
        }
        if (!isSingleHost(mask)) {
            min.add(1);
        }
        return min;
    }
    
    /**
     * Returns if the given netmask is a single host IP. This is given if all netmask bytes are 255.
     * @param mask The netmask as int array
     * @return true if is single host IP
     */
    private static boolean isSingleHost(int mask[]) {
        for (int i=0; i<mask.length; i++) {
            if (mask[i] != 255) {
                return false;
            }
        }
        return true;
    }
    
    /**
     * Calculates the highest possible ip address from an network address and a netmask
     * @param net The network address as int array
     * @param mask The netmask as int array
     * @return The highest ip address in this network
     * @throws Exception
     */
    public static IPv4Address calcHostMax(int net[], int mask[]) throws Exception {
        IPv4Address max = new IPv4Address();
        for (int i = 0; i < net.length; i++) {
            max.setIP(i, net[i] + (~mask[i]) & 0xFF);            
        }
        if (!isSingleHost(mask)) {
            max.add(-1);
        }
        return max;
    }    

    /**
     * Calculates the broadcast address for a network adress and netmask
     * @param net The network address as int array
     * @param mask The netmask as int array
     * @return The broadcast address of this network
     */
    public static int[] calcBroadcast(int net[], int mask[]) {
        int max[] = new int[net.length];
        for (int i = 0; i < net.length; i++) {
            max[i] = net[i] + (~mask[i]) & 0xFF;
        }
        return max;
    }    

}
