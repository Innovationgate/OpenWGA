define(["cm", "appnav"], function(CM, Appnav){

	var url = CM.url.json + "/access-rights.int.json";
		
	var template;

	function onContextChange(context){
		context && template && $.getJSON(url, context, function(result){
			template.render(result)
		})		
	}
	
	function init(){
	
		template = CM.template("appnav-access-rights")
		
		onContextChange(Appnav.getContext());
		
		Appnav.setContextChangeListener(onContextChange);
		Appnav.selectView("access-rights"); 
	}
	
	return {
		init: init
	}
	
})
