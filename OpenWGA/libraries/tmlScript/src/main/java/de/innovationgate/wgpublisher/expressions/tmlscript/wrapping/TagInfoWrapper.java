package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.utils.TagInfo;

public class TagInfoWrapper extends NativeJavaObject {

	private static final long serialVersionUID = 1L;
	private TagInfo _info;
	
	public TagInfoWrapper(Scriptable scope, TagInfo info) {
		super(scope, info, TagInfo.class);
		_info = info;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.TagInosWrapper";
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
				return Context.javaToJS(_info.getInfo(arg0), arg1);
			} catch (WGAPIException e) {
				e.printStackTrace();
				return null;
			}
		}
	}

}
