package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.util.HashMap;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.webgate.api.WGException;

public class MapWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private HashMap map;
	
	public MapWrapper(Scriptable scope, HashMap map) {
		super(scope, map, HashMap.class);
		this.map = map;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.MapWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		if (super.has(arg0, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
		}
		else {
			return Context.javaToJS(this.map.get(arg0), arg1);
		}
	}

	public void put(String name, Scriptable start, Object value) {
        this.map.put(name, Context.jsToJava(value, Object.class));
	}

}
