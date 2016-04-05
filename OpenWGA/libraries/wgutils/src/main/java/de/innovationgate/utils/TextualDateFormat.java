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
package de.innovationgate.utils;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * A date format which supports textual date representations
 * such as "today,tomorrow,yesterday" or "heute,morgen,gestern"
 * currently Locale.ENGLISH and Locale.GERMAN are supported, 
 * all other locales fallback to Locale.ENGLISH
 */
public class TextualDateFormat extends DateFormat {

	private static final long serialVersionUID = 1L;
	
	public static final String BUNDLE_KEY_TODAY = "today";
	public static final String BUNDLE_KEY_YESTERDAY = "yesterday";
	public static final String BUNDLE_KEY_TOMORROW = "tomorrow";
	
	private static final String BUNDLE_NAME = "de.innovationgate.utils.TextualDateFormat";
	
	private ResourceBundle _resources;
	private String _valueForToday;
	private String _valueForYesterday;
	private String _valueForTomorrow;

	private DateFormat _fallback;

	/**
	 * create a DateFormat for the given locale and with the given fallback date format
	 * @param locale - Locale to use for textual date representations (currently supported: Locale.ENGLISH, Locale.GERMAN)
	 * @param fallback - DateFormat to use when given source is not a textual date representation
	 */
	public TextualDateFormat(Locale locale, DateFormat fallback) {
		
		_resources = ResourceBundle.getBundle(BUNDLE_NAME, locale);
		if (!_resources.getLocale().getLanguage().equals(locale.getLanguage())) {
			_resources = ResourceBundle.getBundle(BUNDLE_NAME, Locale.ENGLISH);
		}
		_valueForToday = _resources.getString(BUNDLE_KEY_TODAY);
		_valueForYesterday = _resources.getString(BUNDLE_KEY_YESTERDAY);
		_valueForTomorrow = _resources.getString(BUNDLE_KEY_TOMORROW);
		
		_fallback = fallback;
		_fallback.setLenient(false);
	}

	public StringBuffer format(Date date, StringBuffer toAppendTo,
			FieldPosition fieldPosition) {
		return _fallback.format(date, toAppendTo, fieldPosition);
	}

	public Date parse(String source, ParsePosition pos) {
		if (source == null) {
			return _fallback.parse(source, pos);
		} else {
			String toParse = source.substring(pos.getIndex());			
			Date result = null;
			if (toParse.equalsIgnoreCase(_valueForToday)) {				
				result = new Date();
				pos.setIndex(toParse.length() - 1);
			} else if (toParse.equalsIgnoreCase(_valueForYesterday)) {
				GregorianCalendar cal = new GregorianCalendar();
				cal.setTime(new Date());
				cal.add(GregorianCalendar.DAY_OF_YEAR, -1);
				result = cal.getTime();
				pos.setIndex(toParse.length() - 1);
			} else if (toParse.equalsIgnoreCase(_valueForTomorrow)) {
				GregorianCalendar cal = new GregorianCalendar();
				cal.setTime(new Date());
				cal.add(GregorianCalendar.DAY_OF_YEAR, 1);
				result =  cal.getTime();
				pos.setIndex(toParse.length() - 1);
			} 
			if (result != null) {
				return WGUtils.dateOnly(result);
			} else {
				return _fallback.parse(source, pos);
			}
		}
	}

}
