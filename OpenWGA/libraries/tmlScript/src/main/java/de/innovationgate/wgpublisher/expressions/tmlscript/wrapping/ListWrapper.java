package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.tmlscript.JavascriptFunctionComparator;

public class ListWrapper extends ScriptableObject implements Wrapper {

	private static final long serialVersionUID = 1L;
	private ArrayList<Object> list;

    public static final String[] METHODS = {
            "sort",
            "reverse"
        };

	public ListWrapper(Scriptable scope, ArrayList<Object> list) {
		super(scope, new NativeJavaObject(scope, list, ArrayList.class));
		defineFunctionProperties(METHODS, ListWrapper.class, DONTENUM);
		this.list = list;
	}

	@SuppressWarnings("unchecked")
	public static ArrayList<Object> sort(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
		ArrayList<Object> list = ((ListWrapper)thisObj).unwrap();		
		Comparator<Object> compare;
		if(args.length > 0 && args[0] instanceof Function){
			compare = new JavascriptFunctionComparator((Function) args[0]);
		}
		else compare = new Comparator<Object>() {
			@Override
			public int compare(Object o1, Object o2) {
				String s1 = o1.toString();
				String s2 = o2.toString();
				return s1.compareTo(s2);
			}
		};
		list.sort(compare);
		return list;
		
	}

	public static ArrayList<Object> reverse(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
		ArrayList<Object> list = ((ListWrapper)thisObj).unwrap();
		Collections.reverse(list);
		return list;
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
			return Context.javaToJS(this.list.get(index), arg1);
		}
	}

	public void put(int index, Scriptable start, Object value) {
        this.list.set(index, Context.jsToJava(value, Object.class));
	}

	@Override
	public ArrayList<Object> unwrap() {
		// TODO Auto-generated method stub
		return this.list;
	}

}
