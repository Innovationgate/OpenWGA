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
package de.innovationgate.igutils;

import de.innovationgate.igutils.pingback.PingBackClient;
import de.innovationgate.igutils.pingback.PingBackException;

public class Test {
	
	private static void testPingBack() {
		PingBackClient client = new PingBackClient();
		
		try {
			//String result = client.ping("http://www.binias-online.de/binias", "http://tbig.wordpress.com/2007/08/06/second-time/");
			
			//String result = client.ping("http://www.binias-online.de/binias/index.html", "http://google.com");
			String result = client.checkSourceURI("http://www.bertels.info/?p=145", "http://www.schmidetzki.net/schmidetzki/html/default/8a85818b148904490114ae1d8fa921e5.de.html");
			
			//client.checkSourceURI("http://www.binias-online.de/binias/index.html", "http://10.1.1.78:8080/WGAPublisher/myblog/html/default/4028857c145a8d9101145a99c614000f.de.html");
			
			System.out.println("Result: " + result);
		} catch (PingBackException e) {
			e.printStackTrace();
		}
	}
	
	private static void testLinkChecker() {
		LinkChecker linkChecker = new LinkChecker();
		linkChecker.setHttpConnectionTimeout(5000);
		linkChecker.setHttpTimeout(5000);
		//linkChecker.setProxy("localhost", 3128);
		//linkChecker.setProxy("localhost", 3128, "user", "password");
		
		try {
			int result = linkChecker.check("http://pv.binias-online.de");
			
			System.out.println("Result: " + result);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String args[]) {
		testLinkChecker();
		//testPingBack();
		/*
		try {
			String encryptThis = "Das ist sehr geheim.";
			RSAKeyPair keyPair = KeyPairUtils.generateRSAKeyPair("HalloWelt4");
			
			byte[] encrypted = RSAEncrypter.encrypt(encryptThis.getBytes(), keyPair);
			System.out.println("encryted: " + new String(encrypted));
			
			byte[] decrypted = RSAEncrypter.decrypt(encrypted, keyPair, "HalloWelt4");
			System.out.println("decryted: " + new String(decrypted));
			
		} catch (PersistentKeyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (GeneralSecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}*/

	}

}
