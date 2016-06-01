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

package de.innovationgate.igutils.security;

import java.awt.Color;
import java.io.IOException;
import java.io.Serializable;

import javax.imageio.ImageIO;

import nl.captcha.Captcha;
import nl.captcha.backgrounds.FlatColorBackgroundProducer;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class SimpleCaptchaService implements CaptchaServiceIF {
	public static final String SESSION_ATTRIB_SIMPLECAPTCHA = "de.innovationgate.igutils.security.SimpleCaptcha";
	
	public class TransientWrapper implements Serializable {
		private static final long serialVersionUID = -1035187971911197278L;		
		private transient Object _value;
		
		public TransientWrapper() {
			super();
		}

		public Object getValue() {
			return _value;
		}

		public void setValue(Object value) {
			_value = value;
		}
		
	}


	public void renderCaptcha(TMLContext context) throws IOException {
		java.awt.Color backgroundColor = Color.WHITE;
		
		Captcha captcha = new Captcha.Builder(200, 50)
	     .addText()
	     .addBackground(new FlatColorBackgroundProducer(backgroundColor))
	     .gimp()
	     .addNoise()
	     .build(); // Required! Always!
		
		try{
			TransientWrapper wrapper = new TransientWrapper();
			wrapper.setValue(captcha);
			context.getrequest().getSession().setAttribute(SESSION_ATTRIB_SIMPLECAPTCHA, wrapper);
						
			ImageIO.write(captcha.getImage(), "png", context.getresponse().getOutputStream());
		}catch(Exception e){
			new IOException("Captcha could not be initialized");
		}

	}

	public boolean validateResponse(TMLContext context, String answer) {
		try {
			Object wrapper = context.getrequest().getSession().getAttribute(SESSION_ATTRIB_SIMPLECAPTCHA);		
			if (wrapper != null && wrapper instanceof TransientWrapper) {
				Captcha captcha = (Captcha) ((TransientWrapper)wrapper).getValue();
				if (captcha == null) {
					throw new IllegalArgumentException("No captcha generated for session ('" + context.getrequest().getSession().getId() + "').");
				} else {
					return captcha.isCorrect(answer);
				}
			} else {
				throw new IllegalArgumentException("No captcha generated for session ('" + context.getrequest().getSession().getId() + "').");
			}
		} finally {
			context.getrequest().getSession().removeAttribute(SESSION_ATTRIB_SIMPLECAPTCHA);
		}
	}

}
