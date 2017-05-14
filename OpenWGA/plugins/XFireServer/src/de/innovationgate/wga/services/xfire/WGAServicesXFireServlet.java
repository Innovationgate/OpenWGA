/*******************************************************************************
 * Copyright 2010 Innovation Gate GmbH
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
package de.innovationgate.wga.services.xfire;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.codehaus.xfire.transport.http.XFireServlet;

public abstract class WGAServicesXFireServlet extends XFireServlet {

	private static final long serialVersionUID = 3218012529486267557L;
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		//_coreServices.setRequest(req);
		/*
		if (_customServices != null) {
			_customServices.setRequest(req);
		}*/
		super.doPost(req, res);
		/*
		_coreServices.removeRequest();
		if (_customServices != null) {
			_customServices.removeRequest();
		}*/		
	}


}
