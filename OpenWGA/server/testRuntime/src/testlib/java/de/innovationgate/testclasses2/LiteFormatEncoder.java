package de.innovationgate.testclasses2;

import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextAwareFormatter;


/**
 * @author ow
 *
 * Folgendes auswählen, um die Schablone für den erstellten Typenkommentar zu ändern:
 * Fenster&gt;Benutzervorgaben&gt;Java&gt;Codegenerierung&gt;Code und Kommentare
 */
public class LiteFormatEncoder extends de.innovationgate.wgpublisher.webtml.utils.HTMLXMLEncodingFormatter {

	/**
	 * @param format
	 */
	public LiteFormatEncoder() {
		super("html", new Version(6,0,0));
	}	/* (Kein Javadoc)
	 * @see de.innovationgate.utils.ObjectFormatter#format(java.lang.Object)
	 */
	public String format(Object obj) {
		
		String input = (String) obj;
		StringBuffer output = new StringBuffer();
		
		int lastFormatPos = -1;
		int formatPos = 0;
		int formatEndPos;
		while ((formatPos = input.indexOf("{", lastFormatPos)) != -1) {
			if (lastFormatPos != 0) {
				output.append(super.format(input.substring(lastFormatPos + 1, formatPos)));
			}
			formatEndPos = input.indexOf("}", formatPos);
			output.append(getFormat(input.substring(formatPos + 1, formatEndPos)));
			lastFormatPos = formatEndPos;
			
		}
		if (lastFormatPos < input.length() - 1) {
			output.append(super.format(input.substring(lastFormatPos + 1)));
		}
		
		return output.toString();
		
	}
	/**
	 * @param string
	 * @return
	 */
	private Object getFormat(String string) {
		
		if (string.equals("i")) {
			return "<i>";
		}
		else if (string.equals("/i")) {
			return "</i>";
		}
		else if (string.equals("b")) {
			return "<b>";
		}
		else if (string.equals("/b")) {
			return "</b>";
		}
		else {
			return "";
		}

		
	}

}
