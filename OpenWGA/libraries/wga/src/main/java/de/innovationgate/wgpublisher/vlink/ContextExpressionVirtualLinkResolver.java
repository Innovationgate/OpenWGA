package de.innovationgate.wgpublisher.vlink;

import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget.Type;

public class ContextExpressionVirtualLinkResolver implements VirtualLinkResolver{

	@Override
	public VirtualLinkTarget resolve(WGA wga, WGContent content) throws WGException {
        String exp = content.getVirtualLink();
        
        Context cx = wga.isTMLContextAvailable() ? wga.tmlcontext() : wga.createTMLContext(content);
        Context targetCx = cx.context(exp, false);
        if (targetCx != null) {
            VirtualLinkTarget target = new VirtualLinkTarget(Type.CONTENT);
            target.setContainerKey((String) targetCx.meta("KEY"));
            return target;
        }
        else {
            return null;
        }
	}

}
