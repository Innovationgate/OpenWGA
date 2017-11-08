package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import javax.servlet.ServletRequest;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.wgpublisher.filter.WGAFilter.RequestWrapper;

public class ServletRequestWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private RequestWrapper request;
	
	public ServletRequestWrapper(Scriptable scope, RequestWrapper request) {
		super(scope, request, RequestWrapper.class);
		this.request = request;
		
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.RequestWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		String argLC = arg0.toLowerCase();
		if (super.has(arg0, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
		}
		else {
			return Context.javaToJS(this.request.getParameter(arg0), arg1);
		}
	}

}
