define(["cm", "appnav"], function(CM, Appnav){

	var url = CM.url.json + "/child-docs"
	var template;

	function onContextChange(context){
		template && $.getJSON(url, context, function(result){
			template.render(result)
		})		
	}
	
	function init(){
	
		template = CM.template("appnav-child-docs")
		
		$("#app-child-docs").on("click", "a", function(ev){
			ev.preventDefault();
			$("#site-panel").attr("src", this.href);
		})
		
		onContextChange(Appnav.getContext());
		
		Appnav.setContextChangeListener(onContextChange)

	}
	
	return {
		init: init
	}
	
})
	
