package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;
import de.innovationgate.wga.server.api.WGAMap;

public class WGAMapWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private WGAMap<Object,Object> map;
	
	public WGAMapWrapper(Scriptable scope, WGAMap<Object,Object> map) {
		super(scope, map, WGAMap.class);
		this.map = (WGAMap<Object,Object>)map;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.WGAMapWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		if (super.has(arg0, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
		}
		else {
			try {
				if(this.map.containsKey(arg0))
					return Context.javaToJS(this.map.get(arg0), arg1);
				else return Undefined.instance;
			}
			catch(Exception e) {
				return Undefined.instance;				
			}
		}
	}

	public void put(String name, Scriptable start, Object value) {
        this.map.put(name, Context.jsToJava(value, Object.class));
	}

}
