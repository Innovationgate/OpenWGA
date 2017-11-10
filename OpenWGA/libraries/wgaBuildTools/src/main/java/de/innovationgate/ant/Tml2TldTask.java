/*******************************************************************************
 * Copyright (c) 2009, 2010 Innovation Gate GmbH.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Innovation Gate GmbH - initial API and implementation
 ******************************************************************************/
package de.innovationgate.ant;

import java.io.File;

public class Tml2TldTask {
	
	private File tml;
	private File tld;

	public void setTml(File tml) {
		this.tml = tml;
	}
	
	public void setTld(File tld) {
		this.tld = tld;
	}

	public void execute() {
		if(tml == null || tld == null)
			throw new RuntimeException("tml and tld must be specified");
		Tml2Tld t2t = new Tml2Tld(tml, tld);
		try {
			t2t.run();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
