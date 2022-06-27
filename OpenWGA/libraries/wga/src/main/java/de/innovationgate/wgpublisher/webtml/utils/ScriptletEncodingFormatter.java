package de.innovationgate.wgpublisher.webtml.utils;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;

public class ScriptletEncodingFormatter implements ObjectFormatter{

	private TMLContext _context;
	
	public ScriptletEncodingFormatter(TMLContext context) {
		_context = context;
	}

	@Override
	public String format(Object obj) throws FormattingException {
		try {
			return (String) WGA.get(_context).tmlscript().resolveScriptlets(_context, obj);
		} catch (WGException e) {
			throw new FormattingException("Exception parsing scriptlets", e);
		}
	}

}
