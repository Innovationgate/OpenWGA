define(["cm", "appnav"], function(CM, Appnav){

	var url = CM.url.json + "/dashboard.int.json";
		
	var template;

	function onContextChange(context){
		context && template && $.getJSON(url, context, function(result){
			template.render(result)
		})		
	}
	
	function init(){
	
		template = CM.template("appnav-dashboard")
		
		$("#appnav-dashboard").on("click", "a", function(ev){
			ev.preventDefault();
			$("#site-panel").attr("src", this.href);
		})
				
		onContextChange(Appnav.getContext());
		
		Appnav.setContextChangeListener(onContextChange);
		Appnav.selectView("dashboard"); 
	}
	
	return {
		init: init
	}
	
})
	
