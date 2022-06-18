package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.FunctionObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.wgpublisher.expressions.tmlscript.JavascriptFunctionComparator;

public class ListWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private ArrayList<Object> list;

	private Function _sortFunction;
	private Function _reverseFunction;

	public ListWrapper(Scriptable scope, ArrayList<Object> list){
		super(scope, list, ArrayList.class);

		this.list = list;
		
		try {
			Method method = this.getClass().getMethod("sort", Function.class);
			_sortFunction = new FunctionObject("sort", method, this);
			method = this.getClass().getMethod("reverse");
			_reverseFunction = new FunctionObject("reverse", method, this);
		} catch (NoSuchMethodException | SecurityException e) {
			e.printStackTrace();
		} 
	}

	public ArrayList<Object> sort(Function f){
		Comparator<Object> compare;
		if(f==null){
			compare = new Comparator<Object>() {
				@Override
				public int compare(Object o1, Object o2) {
					return o1.toString().compareTo(o2.toString());
				}
			};
		}
		else compare = new JavascriptFunctionComparator(f);
		list.sort(compare);
		return list;
	}

	public ArrayList<Object> reverse(){
		Collections.reverse(list);
		return list;
	}

	public ArrayList<Object> unwrap() {
		return this.list;
	}
		
	
	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.ListWrapper";
	}

	public Object get(String prop, Scriptable arg1) {
		if(prop.equals("sort")){
			return _sortFunction;
		}
		else if(prop.equals("reverse")){
			return _reverseFunction;
		}
		else return RhinoWrapFactory.notFoundToNull(super.get(prop, arg1));
	}
	
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

}
