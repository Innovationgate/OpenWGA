package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import javax.servlet.ServletRequest;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;

public class ServletRequestWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private ServletRequest request;
	
	public ServletRequestWrapper(Scriptable scope, ServletRequest request) {
		super(scope, request, ServletRequest.class);
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
		if (super.has(arg0, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
		}
		else {
			return Context.javaToJS(this.request.getParameter(arg0), arg1);
		}
	}

}
