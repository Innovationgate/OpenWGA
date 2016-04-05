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
package de.innovationgate.wga.config;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import org.simpleframework.xml.core.Persister;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.model.ValidationError;

/**
 * Factory object to read/write/create {@link WGAConfiguration} objects
 */
public class WGAConfigurationFactory {
	
	private static Persister _serializer = new Persister();

	public WGAConfiguration createDefaultConfig(Version version) {
		WGAConfiguration config = new WGAConfiguration();
		config.setWgaVersion(version.getMainVersionString());
		
		try {
            Administrator admin = new Administrator("admin", WGUtils.hashPassword("wga"), Administrator.ENCODING_HASH);
            config.add(admin);

            config.createDefaultResources();
            config.setRunWizard(true);

            return config;
        }
        catch (Exception e) {
            throw new IllegalStateException("Exception creating default configuration", e);
        }	
	}

	public void write(WGAConfiguration config, OutputStream out) throws Exception {
		config.createDefaultResources();
		List<ValidationError> errors =  config.validate();
		if (!errors.isEmpty()) {
			ConfigValidationException exc = new ConfigValidationException("Configuration contains validation errors.");
			exc.setValidationErrors(errors);
			throw exc;
		}
		_serializer.write(config, out);
	}
	
	public WGAConfiguration read(InputStream in) throws Exception {
		WGAConfiguration config = _serializer.read(WGAConfiguration.class, in);
		config.createDefaultResources();
		List<ValidationError> errors =  config.validate(true);
		if (!errors.isEmpty()) {
			ConfigValidationException exc = new ConfigValidationException("Configuration integrity check fails.");
			exc.setValidationErrors(errors);
			throw exc;
		}
		return config;
	}
	
	public WGAConfiguration clone(WGAConfiguration config) throws CloneNotSupportedException {
		try {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			write(config, out);
			return read(new ByteArrayInputStream(out.toByteArray()));
		} catch (Exception e) {
			CloneNotSupportedException ex = new CloneNotSupportedException("Unable to clone wga configuration. Message: " + e.getMessage());
			ex.setStackTrace(e.getStackTrace());
			throw ex;
		} 		
	}


}
