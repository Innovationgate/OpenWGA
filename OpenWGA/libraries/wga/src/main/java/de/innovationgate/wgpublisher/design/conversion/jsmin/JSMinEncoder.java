package de.innovationgate.wgpublisher.design.conversion.jsmin;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.wgpublisher.design.conversion.jsmin.JSMin.UnterminatedCommentException;
import de.innovationgate.wgpublisher.design.conversion.jsmin.JSMin.UnterminatedRegExpLiteralException;
import de.innovationgate.wgpublisher.design.conversion.jsmin.JSMin.UnterminatedStringLiteralException;

public class JSMinEncoder implements ObjectFormatter{

	@Override
	public String format(Object obj) throws FormattingException {
		StringReader in = new StringReader(obj.toString());
		StringWriter out = new StringWriter();
		JSMin processor = new JSMin(in, out);
		try {
			processor.jsmin();
		} catch (IOException | UnterminatedRegExpLiteralException | UnterminatedCommentException
				| UnterminatedStringLiteralException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return out.toString().trim();
	}

}
