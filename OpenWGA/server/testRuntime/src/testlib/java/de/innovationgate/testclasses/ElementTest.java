package de.innovationgate.testclasses;
import de.innovationgate.wgpublisher.webtml.utils.ElementImpl;
import de.innovationgate.wgpublisher.webtml.utils.ElementImplContext;


/**
 * @author ow
 *
 * Folgendes auswählen, um die Schablone für den erstellten Typenkommentar zu ändern:
 * Fenster&gt;Benutzervorgaben&gt;Java&gt;Codegenerierung&gt;Code und Kommentare
 */
public class ElementTest implements ElementImpl {

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.ElementImpl#begin(de.innovationgate.wgpublisher.webtml.utils.ElementImplContext)
	 */
	public void begin(ElementImplContext context) {
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.ElementImpl#beforeBody(de.innovationgate.wgpublisher.webtml.utils.ElementImplContext)
	 */
	public boolean beforeBody(ElementImplContext context) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.ElementImpl#afterBody(de.innovationgate.wgpublisher.webtml.utils.ElementImplContext)
	 */
	public boolean afterBody(ElementImplContext context) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.ElementImpl#end(de.innovationgate.wgpublisher.webtml.utils.ElementImplContext)
	 */
	public void end(ElementImplContext context) {
		context.appendResult("This is element ElementTest!");

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.webtml.utils.ElementImpl#tagInfo(java.lang.String)
	 */
	public Object tagInfo(String name) {
		return null;
	}

}
