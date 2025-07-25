package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.util.ArrayList;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;

public class ListWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private ArrayList<Object> list;
	
	public ListWrapper(Scriptable scope, ArrayList<Object> list) {
		super(scope, list, ArrayList.class);
		this.list = list;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.ListWrapper";
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
