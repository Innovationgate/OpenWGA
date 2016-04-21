/*
 * Created on Nov 17, 2006 from ow
 *
 */
package de.innovationgate.testclasses2;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.utils.ElementImpl;
import de.innovationgate.wgpublisher.webtml.utils.ElementImplContext;

public class UnitTestElement implements ElementImpl {

    public boolean afterBody(ElementImplContext context) throws WGAPIException {
        return false;
    }

    public boolean beforeBody(ElementImplContext context) {
        return false;
    }

    public void begin(ElementImplContext context) throws WGAPIException {
    }

    public void end(ElementImplContext context) {
        context.appendResult("htmlunittest");

    }

    public Object tagInfo(String name) {
        return null;
    }

}
