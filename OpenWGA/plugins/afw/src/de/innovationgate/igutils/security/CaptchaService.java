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
import java.awt.image.BufferedImage;
import java.awt.image.ImageFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import javax.imageio.ImageIO;

import com.octo.captcha.component.image.backgroundgenerator.BackgroundGenerator;
import com.octo.captcha.component.image.backgroundgenerator.UniColorBackgroundGenerator;
import com.octo.captcha.component.image.color.SingleColorGenerator;
import com.octo.captcha.component.image.deformation.ImageDeformation;
import com.octo.captcha.component.image.deformation.ImageDeformationByFilters;
import com.octo.captcha.component.image.fontgenerator.FontGenerator;
import com.octo.captcha.component.image.fontgenerator.RandomFontGenerator;
import com.octo.captcha.component.image.textpaster.DecoratedRandomTextPaster;
import com.octo.captcha.component.image.textpaster.TextPaster;
import com.octo.captcha.component.image.textpaster.textdecorator.BaffleTextDecorator;
import com.octo.captcha.component.image.textpaster.textdecorator.TextDecorator;
import com.octo.captcha.component.image.wordtoimage.DeformedComposedWordToImage;
import com.octo.captcha.engine.image.DefaultImageCaptchaEngine;
import com.octo.captcha.image.ImageCaptcha;
import com.octo.captcha.image.ImageCaptchaFactory;

import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Service to generate image captchas within WGA
 * @author tbinias
 *
 */
public class CaptchaService implements CaptchaServiceIF {
	
	private DefaultImageCaptchaEngine _engine = null;
	
	public static final String SESSION_ATTRIB_CAPTCHA = "de.innovationgate.igutils.security.Captcha";
	
	private int _sizeX = 200;
	private int _sizeY = 100;
	
	private int _minFontSize = 30;
	private int _maxFontSize = 35;
	private Color _fontColor = Color.black;
		
	private Color _backgroundColor = Color.white;
	private Color _holeColor = Color.white;
	private int _numberOfHolesPerGlyph = 1;
	
	private int _minWordLength = 6;
	private int _maxWordLength = 7;
	
	private double _amplitude = 3d;
	private boolean _antialias = true;
	private double _phase = 20d;
	private double _wavelength = 70d;
	
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
	
	
	/**
	 * constructs a new CaptchaService with default parameters
	 */
	public CaptchaService() {
		init();
	}
	
	/**
	 * inits the service, have to be called after parameter (setXXX...) changes
	 */
	public void init() {
		_engine = new DefaultImageCaptchaEngine(createFactories()){};		
	}
	
	/* (non-Javadoc)
	 * @see de.innovationgate.igutils.security.CaptchaServiceIF#renderCaptcha(de.innovationgate.wgpublisher.webtml.utils.TMLContext)
	 */
	public void renderCaptcha(TMLContext context) throws IOException {					
		ImageCaptcha imageCaptcha = _engine.getNextImageCaptcha();
		
		TransientWrapper wrapper = new TransientWrapper();
		wrapper.setValue(imageCaptcha);
		context.getrequest().getSession().setAttribute(SESSION_ATTRIB_CAPTCHA, wrapper);
						
		renderImage(imageCaptcha.getImageChallenge(), context.getresponse().getOutputStream());
	}
	
	
	private ImageCaptchaFactory[] createFactories() {
		//build filters
		com.jhlabs.image.WaterFilter water = new com.jhlabs.image.WaterFilter();

		water.setAmplitude(_amplitude);
		water.setAntialias(_antialias);
		water.setPhase(_phase);
		water.setWavelength(_wavelength);

		ImageDeformation backDef = new ImageDeformationByFilters(new ImageFilter[] {});
		ImageDeformation textDef = new ImageDeformationByFilters(new ImageFilter[] {});
		ImageDeformation postDef = new ImageDeformationByFilters(new ImageFilter[] { water });

		//word generator
		com.octo.captcha.component.word.wordgenerator.WordGenerator dictionnaryWords = new com.octo.captcha.component.word.wordgenerator.ComposeDictionaryWordGenerator(
				new com.octo.captcha.component.word.FileDictionary("toddlist"));

		//wordtoimage components
		TextPaster randomPaster = new DecoratedRandomTextPaster(new Integer(getMinWordLength()), new Integer(getMaxWordLength()), new SingleColorGenerator(getFontColor()),
				new TextDecorator[] { new BaffleTextDecorator(new Integer(getNumberOfHolesPerGlyph()), getHoleColor()) });
		
		BackgroundGenerator back = new UniColorBackgroundGenerator(new Integer(getSizeX()), new Integer(getSizeY()), getBackgroundColor());

		FontGenerator shearedFont = new RandomFontGenerator(new Integer(getMinFontSize()), new Integer(getMaxFontSize()));

		//word2image 1
		com.octo.captcha.component.image.wordtoimage.WordToImage word2image;		
		word2image = new DeformedComposedWordToImage(shearedFont, back, randomPaster, backDef, textDef, postDef);
		ImageCaptchaFactory[] factories = new ImageCaptchaFactory[1];
		factories[0] = new com.octo.captcha.image.gimpy.GimpyFactory(dictionnaryWords, word2image); 
		return factories;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.igutils.security.CaptchaServiceIF#validateResponse(de.innovationgate.wgpublisher.webtml.utils.TMLContext, java.lang.String)
	 */
	public boolean validateResponse(TMLContext context, String answer) {
		try {
			Object wrapper = context.getrequest().getSession().getAttribute(SESSION_ATTRIB_CAPTCHA);		
			if (wrapper != null && wrapper instanceof TransientWrapper) {
				ImageCaptcha captcha = (ImageCaptcha) ((TransientWrapper)wrapper).getValue();
				if (captcha == null) {
					throw new IllegalArgumentException("No captcha generated for session ('" + context.getrequest().getSession().getId() + "').");
				} else {
					return captcha.validateResponse(answer).booleanValue();
				}
			} else {
				throw new IllegalArgumentException("No captcha generated for session ('" + context.getrequest().getSession().getId() + "').");
			}
		} finally {
			context.getrequest().getSession().removeAttribute(SESSION_ATTRIB_CAPTCHA);
		}
		
	}

	private void renderImage(BufferedImage image, OutputStream out) throws IOException {			
		ImageIO.write(image, "png", out);
	}

	public int getSizeX() {
		return _sizeX;
	}
	
	public void setSizeX(int sizeX) {
		_sizeX = sizeX;
	}
	
	public int getSizeY() {
		return _sizeY;
	}
	public void setSizeY(int sizeY) {
		_sizeY = sizeY;
	}
	
	public int getMinFontSize() {
		return _minFontSize;
	}
	public void setMinFontSize(int minFontSize) {
		_minFontSize = minFontSize;
	}
	
	public int getMaxFontSize() {
		return _maxFontSize;
	}
	public void setMaxFontSize(int maxFontSize) {
		_maxFontSize = maxFontSize;
	}
	
	public Color getFontColor() {
		return _fontColor;
	}
	
	public void setFontColor(Color fontColor) {
		_fontColor = fontColor;
	}
	
	public void setFontColor(int r, int g, int b) {
		_fontColor = new Color(r, g, b);
	}
	
	public Color getBackgroundColor() {
		return _backgroundColor;
	}
			
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}
	
	public void setBackgroundColor(int r, int g, int b) {
		_backgroundColor = new Color(r, g, b);
	}
	
	public Color getHoleColor() {
		return _holeColor;
	}
	
	public void setHoleColor(Color wholeColor) {
		_holeColor = wholeColor;
	}
	
	public void setHoleColor(int r, int g, int b) {
		_holeColor = new Color(r, g, b);
	}
	
	public int getNumberOfHolesPerGlyph() {
		return _numberOfHolesPerGlyph;
	}
	
	public void setNumberOfHolesPerGlyph(int numberOfHolesPerGlyph) {
		_numberOfHolesPerGlyph = numberOfHolesPerGlyph;
	}
	
	public int getMinWordLength() {
		return _minWordLength;
	}
	
	public void setMinWordLength(int minWordLength) {
		_minWordLength = minWordLength;
	}
	
	public int getMaxWordLength() {
		return _maxWordLength;
	}
	
	public void setMaxWordLength(int maxWordLength) {
		_maxWordLength = maxWordLength;
	}

	public double getAmplitude() {
		return _amplitude;
	}

	public void setAmplitude(double amplitude) {
		_amplitude = amplitude;
	}

	public boolean isAntialias() {
		return _antialias;
	}

	public void setAntialias(boolean antialias) {
		_antialias = antialias;
	}

	public double getPhase() {
		return _phase;
	}

	public void setPhase(double phase) {
		_phase = phase;
	}

	public double getWavelength() {
		return _wavelength;
	}

	public void setWavelength(double wavelength) {
		_wavelength = wavelength;
	}

	
}
