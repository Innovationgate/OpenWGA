package de.innovationgate.testclasses;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.utils.ElementImpl;
import de.innovationgate.wgpublisher.webtml.utils.ElementImplContext;

public class ElementOptionTest implements ElementImpl {

    public void begin(ElementImplContext context) throws WGAPIException {
    }

    public boolean beforeBody(ElementImplContext context) throws WGAPIException {
        return false;
    }

    public boolean afterBody(ElementImplContext context) throws WGAPIException {
        return false;
    }

    public void end(ElementImplContext context) throws WGAPIException {
        context.clearResult();
        context.appendResult("Option:" + (String) context.getTMLContext().option("preferredElementOption"));

    }

    public Object tagInfo(String name) {
        return null;
    }

}
