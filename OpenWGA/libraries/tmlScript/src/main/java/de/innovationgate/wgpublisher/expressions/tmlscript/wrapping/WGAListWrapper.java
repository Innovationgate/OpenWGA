package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;
import de.innovationgate.wga.server.api.WGAList;

public class WGAListWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private WGAList<Object> list;
	
	public WGAListWrapper(Scriptable scope, WGAList<Object> list) {
		super(scope, list, WGAList.class);
		this.list = list;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.WGAListWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(int index, Scriptable arg1) {
		if (super.has(index, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(index, arg1));
		}
		else {
			try {
				return Context.javaToJS(this.list.get(index), arg1);
			}
			catch(Exception e) {
				return Undefined.instance;				
			}
		}
	}

	public void put(int index, Scriptable start, Object value) {
        this.list.set(index, Context.jsToJava(value, Object.class));
	}

}
