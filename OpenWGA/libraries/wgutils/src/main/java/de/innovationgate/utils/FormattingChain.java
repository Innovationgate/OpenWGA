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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * An object formatter that chains together multiple object formatters, that are processed in a well-defined order.
 * Unlike other formatters the formatting chain never throws a FormattingException. Instead it collects
 * FormattingExceptions that happen in one formatter in the chain in an errors list that is retrievable after
 * the formatting operaion.
 */
public class FormattingChain implements ObjectFormatter {
	
	private List<ObjectFormatter> formatters = null;
	private List<FormattingException> errors = new ArrayList<FormattingException>();
	
	public FormattingChain() {
		this.formatters = new ArrayList<ObjectFormatter>();
	}
	
	/**
     * Adds a formatter to the chain. The formatters are processed in the order that
     * they are added to the chain.
	 * @param formatter
	 */
	public void addFormatter(ObjectFormatter formatter) {
		this.formatters.add(formatter);
	}

	/**
	 * @throws FormattingException 
	 * @see de.innovationgate.utils.ObjectFormatter#format(Object)
	 */
	public String format(Object obj) {

	    errors.clear();
		Iterator<ObjectFormatter> formattersIt = this.formatters.iterator();
		ObjectFormatter formatter;
		while (formattersIt.hasNext()) {
			formatter = formattersIt.next();
			try {
                obj = formatter.format(obj);
            }
            catch (FormattingException e) {
                errors.add(e);
                
            }
		}
		return String.valueOf(obj);

	}

    /**
     * Returns a List of {@link FormattingException} objects that represent the errors
     * that occured on the last formatting operation.
     */
    public List<FormattingException> getErrors() {
        return errors;
    }

}
