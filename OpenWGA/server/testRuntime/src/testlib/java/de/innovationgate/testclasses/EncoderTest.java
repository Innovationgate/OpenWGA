package de.innovationgate.testclasses;

import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.WGUtils;

/**
 * @author ow
 *
 * Folgendes auswählen, um die Schablone für den erstellten Typenkommentar zu ändern:
 * Fenster&gt;Benutzervorgaben&gt;Java&gt;Codegenerierung&gt;Code und Kommentare
 */
public class EncoderTest implements ObjectFormatter {

	/* (Kein Javadoc)
	 * @see de.innovationgate.utils.ObjectFormatter#format(java.lang.Object)
	 */
	public String format(Object obj) {
		String result = result = WGUtils.strReplace(String.valueOf(obj), "u", "&uuml;", true); 
		result = WGUtils.strReplace(result, "a", "&uuml;", true);
		result = WGUtils.strReplace(result, "e", "&uuml;", true);
		result = WGUtils.strReplace(result, "i", "&uuml;", true);
		result = WGUtils.strReplace(result, "o", "&uuml;", true);
		
		return result;
	}

}
