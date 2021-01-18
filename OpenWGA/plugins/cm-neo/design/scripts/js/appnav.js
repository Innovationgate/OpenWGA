define(["jquery"], function($){

	var context,
		contextChangeListener;
	
	WGA.event.addListener("siteexplorer", "content-changed", function(ev){
		
		context = ev.params;

		$("#appnav [data-view='outline']")[ev.params.contentkey ? "show" : "hide"]()

		contextChangeListener && contextChangeListener(context);
		
		$("#appnav").show();
	})
	
	$(document).on("click.toolbar", "#appnav .toolbar [data-view]", function(ev){

		ev.preventDefault();
		$this = $(this)
		var selected = $this.hasClass("selected")
		
		$this.siblings().removeClass("selected");
		if(selected){
			$this.removeClass("selected");
			$("#page").removeClass("appnav")
		}
		else {			
			$this.addClass("selected");
			$("#page").addClass("appnav")
		}
		
		WGA.event.fireEvent("appnav-view-change", "appnav", {
			view: $this.data("view")
		})

	})
	
	return {
		
		getContext: function(){
			return context;
		},
		setContextChangeListener: function(listener){
			contextChangeListener = listener;
		}
		
	}
	
})