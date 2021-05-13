package de.innovationgate.wgpublisher;

import de.innovationgate.utils.security.SymmetricEncryptionEngine;
import de.innovationgate.wga.modules.options.PasswordEncodingException;
import de.innovationgate.wga.modules.options.PasswordOptionEncoder;

public class AESPasswordEncoder implements PasswordOptionEncoder{

	@Override
	public String encodePassword(String password) throws PasswordEncodingException {
		SymmetricEncryptionEngine engine = WGACore.INSTANCE.getSymmetricEncryptionEngine();
		try {
			return engine.encryptBase64Web(password.getBytes("UTF-8"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public String decodePassword(String password) throws PasswordEncodingException {
		SymmetricEncryptionEngine engine = WGACore.INSTANCE.getSymmetricEncryptionEngine();
		try {
			return new String(engine.decryptBase64Web(password), "UTF-8");
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public String getEncodingKey() {
		return "AES";
	}

}
