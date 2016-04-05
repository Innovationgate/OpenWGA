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
package de.innovationgate.utils.security;

import java.io.Serializable;

/**
 * Represents an RSA private/public key pair
 */
public class RSAKeyPair implements Serializable {

	private static final long serialVersionUID = -8580618976077073565L;

	private boolean _encrypted;
	
	// private key as Base64
	private String _privateKey;
	
	//public key as Base64
	private String _publicKey;
	
	

	RSAKeyPair(String privateKey, String publicKey, boolean encrypted) {
		_privateKey = privateKey;
		_publicKey = publicKey;
		_encrypted = encrypted;
	}
	
	// no args constructor for XStream on JRockit
	private RSAKeyPair() {};

	boolean isEncrypted() {
		return _encrypted;
	}

	void setEncrypted(boolean encrypted) {
		_encrypted = encrypted;
	}

	String getPrivateKey() {
		return _privateKey;
	}

	void setPrivateKey(String privateKey) {
		_privateKey = privateKey;
	}

	String getPublicKey() {
		return _publicKey;
	}

	void setPublicKey(String publicKey) {
		_publicKey = publicKey;
	}

}
