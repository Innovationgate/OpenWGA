function(obj) {
	obj.invoke = function(method, params) {
			return runtime.invoke(this, method, params)
	}.bind(obj);
	
	obj.toString = function() {
		return runtime.objectToString(this);
	}.bind(obj);
}